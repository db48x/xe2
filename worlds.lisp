;;; worlds.lisp --- turn-based roguelike grid worlds

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Worlds are the focus of the action in RLX. A world is a 3-D grid of
;; interacting cells. The world object performs the following tasks:

;; - Keeps track of a single player in a world of cells
;; - Receives command messages from the user
;; - Handles some messages, forwards the rest on to the player cell.
;; - Runs the CPU phase so that all non-player :actor cells get their turns
;; - Keeps track of lit squares :. lighting >

;;; Code:

(in-package :rlx)

(define-prototype world
    (:documentation "An RLX game world filled with cells.")
  (name :initform "Unknown")
  (description :initform "Unknown area.")
  (required-modes :initform nil)
  (categories :initform nil)
  (mission-grammar :initform '())
  (scale :initform '(1 m)
	 :documentation "Scale per square side in the form (N UNIT) where UNIT is m, km, ly etc.")
  (player :documentation "The player cell.")
  (width :documentation "The width of the world map, measured in tiles.")
  (height :documentation "The height of the world map, measured in tiles.")
  ;; cells 
  (grid :documentation "A two-dimensional array of adjustable vectors of cells.")
  ;; environment 
  (environment-grid :documentation "A two-dimensional array of environment data cells.")
  ;; lighting 
  (automapped :initform nil :documentation "Show all previously lit squares.")
  (light-grid 
   :documentation 
   "A 2d array of integers giving the light level at that point in <grid>.
At the moment, only 0=off and 1=on are supported.")
  (ambient-light :initform :total :documentation 
		 "Radius of ambient visibility. :total means that lighting is turned off.")
  ;; action-points 
  (phase-number :initform 1 :documentation "Integer number of current phase.")
  (turn-number :initform 1 :documentation "Integer number of elapsed user turns (actions).")
  ;; queueing 
  (message-queue :initform (make-queue))
  ;; narration 
  (narrator :documentation "The narration widget object.")
  ;; browsing 
  (browser :documentation "The browser object.")
  ;; viewing
  (viewport :initform nil)
  ;; space
  (edge-condition :initform :exit
		  :documentation "Either :block the player, :exit the world, or :wrap around.")
  (exited :initform nil
	  :documentation "Non-nil when the player has exited. See also `forward'.")
  (player-exit-row :initform 0)
  (player-exit-column :initform 0))

(defparameter *default-world-axis-size* 10)
(defparameter *default-world-z-size* 4)

(define-method in-category world (category)
  (member category <categories>))
    
;; <: environment :>
(define-prototype environment
    (:documentation "A cell giving general environmental conditions at a world location.")
  (temperature :initform nil :documentation "Temperature at this location, in degrees Celsius.")
  (radiation-level :initform nil :documentation "Radiation level at this location, in clicks.")
  (oxygen :initform nil :documentation "Oxygen level, in percent.")
  (pressure :initform nil :documentation "Atmospheric pressure, in multiples of Earth's.")
  (overlay :initform nil 
	   :documentation "Possibly transparent image overlay to be drawn at this location."))
  
(define-method create-grid world (&key width height)
  (let ((dims (list height width)))
    (let ((grid (make-array dims 
		 :element-type 'vector :adjustable t)))
      ;; now put a vector in each square to represent the z-axis
      (dotimes (i height)
	(dotimes (j width)
	  (setf (aref grid i j)
		(make-array *default-world-z-size* 
			    :adjustable t
			    :fill-pointer 0))))
      (setf <grid> grid
	    <height> height
	    <width> width)
      ;; we need a grid of integers for the lighting map.
      (setf <light-grid> (make-array dims
				     :element-type 'integer
				     :initial-element 0))
      ;;and a grid of special objects for the environment map.
      (let ((environment (make-array dims)))
	(setf <environment-grid> environment)
	(dotimes (i height)
	  (dotimes (j width)
	    (setf (aref environment i j) (clone =environment=))))))))

(define-method create-default-grid world ()
  (when (and (numberp <width>)
	     (numberp <height>))
    [create-grid self :width <width> :height <height>]))

(define-method location-name world ()
  <name>)
     
(define-method environment-at world (row column)
  (aref <environment-grid> row column))

(define-method environment-condition-at world (row column condition)
  (field-value condition (aref <environment-grid> row column)))

(define-method set-environment-condition-at world (row column condition value)
  (setf (field-value condition 
		     (aref <environment-grid> row column))
	value))

;;; Narration

(define-method set-narrator world (narrator)
  (setf <narrator> narrator))

(define-method set-browser world (browser)
  (setf <browser> browser))

(define-method cells-at world (row column)
  (when (array-in-bounds-p <grid> row column)
    (aref <grid> row column)))

(define-method replace-cells-at world (row column data)
  (when (array-in-bounds-p <grid> row column)
    (setf (aref <grid> row column)
	  (etypecase data
	    (vector data)
	    (clon:object (let ((cells (make-array *default-world-z-size* 
						  :adjustable t
						  :fill-pointer 0)))
			   (prog1 cells
			     (vector-push-extend data cells))))))))

(define-method drop-cell world (cell row column 
				     &optional &key 
				     loadout no-stepping no-collisions exclusive probe)
  "Put CELL on top of the stack of cells at ROW, COLUMN. If LOADOUT is
non-nil, then the `loadout' method of the dropped cell is invoked
after dropping. If NO-COLLISIONS is non-nil, then an object is not
dropped on top of an obstacle. If EXCLUSIVE is non-nil, then two
objects with category :exclusive will not be placed together. If PROBE
is non-nil, try to place the cell in the immediate neighborhood.
Return T if a cell is placed; nil otherwise. If both NO-COLLISIONS and
EXCLUSIVE are both non-nil, an error is signaled."
  (assert (not (and no-collisions exclusive)))
  (when (array-in-bounds-p <grid> row column)
    (labels ((drop-it (row column)
	       (prog1 t
		 (vector-push-extend cell (aref <grid> row column))
		 (setf (field-value :row cell) row)
		 (setf (field-value :column cell) column)
		 (when loadout
		   [loadout cell])
		 (unless no-stepping
		   [step-on-current-square cell]))))
      (if (or no-collisions exclusive)
	  (progn 
	    (when no-collisions
	      (when (not [obstacle-at-p self row column])
		(drop-it row column)))
	    (when exclusive
	      (if [category-at-p self row column :exclusive]
		  (when probe
		    (block probing
		      (dolist (dir *compass-directions*)
			(multiple-value-bind (r c) 
			    (step-in-direction row column dir)
			  (when (not [category-at-p self row column :exclusive])
			    (return-from probing (drop-it r c)))))))
		  (drop-it row column))))
	  (drop-it row column)))))

(define-method replace-cell world (cell new-cell row column
					&optional &key loadout no-collisions)
  (let* ((cells [cells-at self row column])
	 (pos (position cell cells)))
    (if (numberp pos)
	(setf (aref cells pos) new-cell)
	(error "Could not find cell to replace."))))

(define-method drop-player-at-entry world (player)
  (with-field-values (width height grid) self
    (multiple-value-bind (dest-row dest-column)
	(block seeking
	  (dotimes (i height)
	    (dotimes (j width)
	      (when [category-at-p self i j :player-entry-point]
		(return-from seeking (values i j)))))
	  (return-from seeking (values 0 0)))
      (setf <player> player)
      [drop-cell self player dest-row dest-column :no-stepping t])))

(define-method drop-player-at-last-location world (player)
  (setf <player> player)
  [drop-cell self player <player-exit-row> <player-exit-column>])
  
(define-method nth-cell world (n row column)
  (aref (aref <grid> row column) n))

(define-method get-player world ()
  <player>)

(define-method player-row world ()
  (field-value :row <player>))

(define-method player-column world ()
  (field-value :column <player>))

(define-method exit world ()
  (setf <exited> t) ;; see also `forward' method
  ;; record current location so we can exit back to it
  (setf <player-exit-row> (field-value :row <player>))
  (setf <player-exit-column> (field-value :column <player>))
  [delete-cell self <player> <player-exit-row> <player-exit-column>])
  
(define-method obstacle-at-p world (row column)
  (or (not (array-in-bounds-p <grid> row column))
      (some #'(lambda (cell)
		(when [in-category cell :obstacle]
		  cell))
	    (aref <grid> row column))))

(define-method category-at-p world (row column category)
  (let ((catlist (etypecase category
		   (keyword (list category))
		   (list category))))
    (and (array-in-bounds-p <grid> row column)
	 (some #'(lambda (cell)
		   (when (intersection catlist
				       (field-value :categories cell))
		     cell))
	       (aref <grid> row column)))))

(define-method in-bounds-p world (row column)
  (array-in-bounds-p <grid> row column))

(define-method direction-to-player world (row column)
  (direction-to row column 
		[player-row self]
		[player-column self]))

(define-method distance-to-player world (row column)
  (distance row column
	    [player-row self]
	    [player-column self]))
	    
(define-method adjacent-to-player world (row column)
  (<= [distance-to-player self row column] 1.5))
	
(define-method obstacle-in-direction-p world (row column direction)
  (multiple-value-bind (nrow ncol)
      (step-in-direction row column direction)
    [obstacle-at-p self nrow ncol]))

(define-method category-in-direction-p world (row column direction category)
  (multiple-value-bind (nrow ncol)
      (step-in-direction row column direction)
    [category-at-p self nrow ncol category]))

(define-method target-in-direction-p world (row column direction)
  (multiple-value-bind (nrow ncol)
      (step-in-direction row column direction)
    [category-at-p self nrow ncol :target]))

(define-method set-player world (player)
  "Set PLAYER as the player object to which the World will forward
most user command messages. (See also the method `forward'.)"
  (setf <player> player))

(define-method resolve-receiver world (receiver)
  (case receiver
    (:world self)
    (:browser <browser>)
    (:narrator <narrator>)
    (:viewport <viewport>)
    (:player <player>)))

(define-method process-messages world ()
  "Process, narrate, and send all the messages in the queue.
The processing step allows the sender to specify the receiver
indirectly as a keyword symbol (like `:world', `:player', or
`:output'.) Any resulting queued messages are processed and sent, and
so on, until no more messages are generated."
  (let ((player <player>))
    (with-message-queue <message-queue> 
      (loop while (queued-messages-p) do
	   (destructuring-bind (sender method-key receiver args)
	       (unqueue-message)
	     (let ((rec (or [resolve-receiver self receiver] 
			    receiver)))
	       ;; (when (and <narrator> 
	       ;; 		  ;; only narrate player-related messages
	       ;; 		  (or (eq player sender)
	       ;; 		      (eq player rec)))
	       ;; 	 ;; now print message
	       ;; 	 (when (not (zerop (field-value :verbosity <narrator>)))
	       ;; 	   [narrate-message <narrator> sender method-key rec args]))
	       ;; stop everything if player dies
					;(when (not [in-category player :dead])
	       (apply #'send sender method-key rec args)))))))

(define-method forward world (method-key &rest args)
  "Send unhandled messages to the player object.
This is where most world computations start, because nothing happens
in a roguelike until the user has pressed a key."
  (assert <player>)
  (message "FORWARDWORLD: ~S ~S ~S" <phase-number> 
	   (field-value :phase-number <player>)
	   method-key)
  (prog1 nil
    (let ((player <player>)
	  (phase-number <phase-number>))
      (with-message-queue <message-queue> 
	(when <narrator> 
	  [narrate-message <narrator> nil method-key player args])
	;; run the player
	[run player]
	;; send the message to the player, possibly generating queued messages
	(apply #'send self method-key player args)
	;; process any messages that were generated
	[process-messages self]
	;; if this is the player's last turn, begin the cpu phase
	;; otherwise, stay in player phase and exit
	(unless [can-act player phase-number]
	  [end-phase player]
	  (unless <exited>
	    (incf <phase-number>)
	    (when (not [in-category <player> :dead])
	      [run-cpu-phase self])
	    [begin-phase player]))))))
        
(define-method get-phase-number world ()
  <phase-number>)

(define-method run-cpu-phase world (&optional timer-p)
  "Run all non-player actor cells."
  (when timer-p
    (incf <phase-number>))
  (with-message-queue <message-queue> 
    (let ((cells nil)
	  (cell nil)
	  (phase-number <phase-number>)
	  (player <player>)
	  (grid <grid>))
      [run player]
      [clear-light-grid self]
      (dotimes (i <height>)
	(dotimes (j <width>)
	  (setf cells (aref grid i j))
	  (dotimes (z (fill-pointer cells))
	    (setf cell (aref cells z))
	    ;; perform lighting
	    (when (or [is-player cell]
		      [is-light-source cell])
	      [render-lighting self cell])
	    (when (and (not (eq player cell))
		       [in-category cell :actor]
		       (not [in-category player :dead]))
	      [begin-phase cell]
	      ;; do turns 
	      (loop while [can-act cell phase-number] do
		   [run cell]
		   [process-messages self]
		   [end-phase cell]))))))))

;; <: lighting :>

(defvar *lighting-hack-function* nil)

(define-method render-lighting world (cell)
  (let* ((light-radius (field-value :light-radius cell))
	 (ambient <ambient-light>)
	 (light-grid <light-grid>)
	 (grid <grid>)
	 (source-row (field-value :row cell))
	 (source-column (field-value :column cell))
	 (total (+ light-radius 
		   (if (numberp ambient) ambient 0)))
	 (octagon (make-array 100 :initial-element nil :adjustable t :fill-pointer 0))
	 (line (make-array 100 :initial-element nil :adjustable t :fill-pointer 0)))
    ;; don't bother lighting if everything is lit.
    (when (not (eq :total ambient))
      ;; draw only odd-radius octagons that have a center pixel
      (when (evenp total)
	(incf total))
      (labels ((light-square (row column)
		 (when (array-in-bounds-p light-grid row column)
		   (setf (aref light-grid row column) 1) nil))
	       (collect-line-point (x y)
		 (prog1 nil (vector-push-extend (list x y) line)))
		 ;; (if (array-in-bounds-p light-grid x y)
		 ;;     (prog1 nil (vector-push-extend (list x y) line))
		 ;;     t))
	       (make-line (row column)
		 (setf (fill-pointer line) 0)
		 (let ((flipped (trace-line #'collect-line-point 
					    source-column source-row
					    column row)))
		   ;; Bresenham's swaps the input points around when x0 is to the
		   ;; right of x1. We need to reverse the list of points if this
		   ;; happens, otherwise shadows will be cast the wrong way.
		   (if flipped
		       (setf line (nreverse line))
		       ;; Furthermore, when a non-flipped line is drawn, the endpoint 
		       ;; isn't actually visited, so we append it to the list. (Maybe this 
		       ;; is a bug in my implementation?)
		       ;;
		       ;; Make sure endpoint of ray is traced.
		       (when (array-in-bounds-p grid row column)
			 (vector-push-extend (list row column) line)))))
	       (light-line (row column)
		 (make-line row column)
		 (block lighting 
		   (dotimes (i (fill-pointer line))
		     do (destructuring-bind (r c) (aref line i)
			  (when (array-in-bounds-p grid r c)
			    (light-square r c)
			    ;; HACK
			    (when *lighting-hack-function*
			      (funcall *lighting-hack-function* 
				       source-row source-column
				       r c))
			    ;; should we stop lighting?
			    (when [category-at-p self r c :opaque] ;;'(:opaque :obstacle)]
			      (return-from lighting t)))))))
	       (collect-octagon-point (r c)
		 (vector-push-extend (list r c) octagon) nil)
	       (light-rectangle (row column radius)
		 (trace-rectangle #'light-square 
				  (- row radius)
				  (- column radius) 
				  (* 2 radius)
				  (* 2 radius)
				  :fill))
	       (light-octagon (row column radius)
		 (setf (fill-pointer octagon) 0)
	       	 (trace-octagon #'collect-octagon-point 
	       			row column radius :thicken)
	       	 (dotimes (i (fill-pointer octagon))
	       	   (destructuring-bind (row column) (aref octagon i)
		     ;; HACK
		     ;; (when *lighting-hack-function*
		     ;;   (funcall *lighting-hack-function* 
		     ;; 		source-row source-column
		     ;; 		row column ".red"))
	       	     (light-line row column)))))
	(light-octagon source-row source-column total)
	(light-octagon source-row source-column (- total 2))))))

(define-method clear-light-grid world ()
  (unless <automapped>
    (let ((light-grid <light-grid>))
      (dotimes (i <height>)
	(dotimes (j <width>)	
	  (setf (aref light-grid i j) 0))))))

(define-method deserialize world (sexp)
  "Load a saved world from Lisp data."
  (declare (ignore sexp))
  nil)

(define-method begin-ambient-loop world ()
  nil)

(define-method describe world ()
  [>>newline :narrator]
  (when (and <narrator> (stringp <description>))
    (dolist (line (split-string-on-lines <description>))
      [>>narrateln :narrator line])))

(define-method start world ()
  (assert <player>)
  ;; start player at same phase (avoid free catch-up turns)
  (message "STARTWORLD: ~S ~S" <phase-number> (field-value :phase-number <player>))
  ;; get everyone on the same turn.
  (setf <phase-number> (+ 1 (field-value :phase-number <player>)))
  (let ((grid <grid>)
	(phase-number <phase-number>))
    (dotimes (i <height>)
      (dotimes (j <width>)
	(do-cells (cell (aref grid i j))
	  (setf (field-value :phase-number cell) phase-number)))))
  ;; mark the world as entered
  (setf <exited> nil)
  ;; light up the world
  [render-lighting self <player>]
  ;; clear out any pending messages
  (setf <message-queue> (make-queue))
  (with-message-queue <message-queue>
    [describe self]
    [run-cpu-phase self]
    (incf <phase-number>)
    [start <player>]
    [begin-phase <player>]
    (when (has-method :show-location <player>)
      [show-location <player>])
    [process-messages self])
  [begin-ambient-loop self])
    
(define-method set-viewport world (viewport)
  (setf <viewport> viewport))

(define-method delete-cell world (cell row column)
  (let* ((grid <grid>)
	 (square (aref grid row column))
	 (start (position cell square :test #'eq)))
    (when start
      (replace square square :start1 start :start2 (1+ start))
      (decf (fill-pointer square)))))

;;     (setf (aref grid row column)
;; 	  (make-array :de(delete cell square :test #'eq))))

(define-method move-cell world (cell row column)
  (let* ((old-row (field-value :row cell))
	 (old-column (field-value :column cell)))
    [delete-cell self cell old-row old-column]
    [drop-cell self cell row column]))

;;; Universes are composed of connected worlds.

(defvar *active-universe* nil)

(defun normalize-address (address)
  "Sort the plist ADDRESS so that its keys come in alphabetical order
by symbol name. This enables them to be used as hash keys."
  (assert (and (symbolp (first address))
	       (or (null (rest address))
		   (keywordp (second address)))))
  (labels ((all-keys (plist)
	     (let (keys)
	       (loop while (not (null plist))
		     do (progn (push (pop plist) keys)
			       (pop plist)))
	       keys)))
    (let (address2)
      (dolist (key (sort (all-keys (cdr address)) #'string> :key #'symbol-name))
	;; build sorted plist
	(push (getf (cdr address) key) address2)
	(push key address2))
	(cons (car address) address2))))

(defparameter *default-space-size* 10)

(define-prototype universe 
    (:documentation "A collection of connected worlds.")
  (worlds :initform (make-hash-table :test 'equal)
	  :documentation "Address-to-world mapping.")
  (viewport :initform nil)
  (current-address :initform nil)
  (player :initform nil)
  (stack :initform '())
  (space :initform nil 
	 :documentation "When non-nil, this vector of worlds
represents the z-axis of a euclidean 3-D space."))

(define-method make-euclidean universe ()
  (setf <space> (make-array *default-space-size* 
			    :adjustable t
			    :fill-pointer 0)))

(define-method get-space-at universe (index)
  (aref <space> index))

(define-method set-space-at universe (index world)
  (setf (aref <space> index) world))

(define-method get-next-space universe (index)
  (incf index)
  (when (and (<= 0 index)
	     (< index (fill-pointer <space>)))
    (aref <space> index)))

(define-method get-previous-space universe (index)
  (decf index)
  (when (and (<= 0 index)
	     (< index (fill-pointer <space>)))
    (aref <space> index)))

(define-method add-world universe (address world)
  (setf (gethash (normalize-address address) <worlds>) world))
 
(define-method remove-world universe (address)
  (remhash (normalize-address address) <worlds>))

(define-method get-world universe (address)
  (gethash (normalize-address address) <worlds>))

(define-method get-player universe ()
  <player>)

(define-method set-player universe (player)
  (setf <player> player))

(define-method get-current-world universe ()
  (car <stack>))

(define-method get-current-address universe ()
  <current-address>)

(define-method destroy universe ()
  (setf <worlds> (make-hash-table :test 'equal))
  (setf <stack> nil)
  (setf <current-address> nil))

(define-method generate-world universe (address)
  (destructuring-bind (prototype &rest parameters) address
    (let ((world (clone (symbol-value prototype))))
      (prog1 world
	;; make sure any loadouts or intializers get run with the proper world
	(let ((*active-world* world)) 
	  [generate-with world parameters])))))

(define-method generate world (&rest parameters)
  "Generate a world, reading generation parameters from the plist
  PARAMETERS."  
  (declare (ignore parameters))
  nil)

(define-method generate-with world (parameters)
  (apply #'send self :generate self parameters))

(define-method find-world universe (address)
  (let ((candidate [get-world self address]))
    (if (null candidate)
	[add-world self (normalize-address address)
		   [generate-world self address]]
	candidate)))

(define-method play universe (&key address player prompt narrator viewport)
  (setf <current-address> address)
  (when player (setf <player> player))
  (when prompt (setf <prompt> prompt))
  (when narrator (setf <narrator> narrator))
  (when viewport (setf <viewport> viewport))
  (assert (and <prompt> <narrator>))
  (let ((world [find-world self address])
	(player <player>)
	(previous-world (car <stack>)))
    ;; make sure exit coordinates are saved, so we can go back to this point
    (when previous-world 
      [exit previous-world])
    ;; make the new world the current world
    (push world <stack>)
    (setf *active-world* world)
    (setf *active-universe* self)
    [set-viewport world <viewport>]
    [drop-player-at-entry world player]
    [set-receiver <prompt> world]
    [set-narrator world <narrator>]
    [start world]))

(define-method exit universe (&key player)
  (when player (setf <player> player))
  (with-fields (stack) self
    ;; exit and discard current world
    [exit (pop stack)]
    ;; 
    (let ((world (car stack)))
      (when world
	(setf *active-world* world)
	(setf *active-universe* self)
	;; resume at previous play coordinates
	[drop-player-at-last-location world <player>]
	[start world]
	[set-receiver <prompt> world]
	[set-narrator world <narrator>]
	[set-player world <player>]
	[set-viewport world <viewport>]))))

;;; Gateways and launchpads connect worlds together

(defcell gateway
  (tile :initform "gateway")
  (name :initform "Gateway")
  (categories :initform '(:gateway :exclusive))
  (address :initform nil))

(define-method activate gateway ()
  [play *active-universe* :address <address> :player [get-player *active-world*]])

(define-prototype launchpad (:parent =gateway=)
  (tile :initform "launchpad")
  (categories :initform '(:gateway :player-entry-point))
  (description :initform "Press RETURN here to exit this area."))

(define-method activate launchpad ()
  [exit *active-universe* :player [get-player *active-world*]])

(define-method drop-entry-point world (row column)
  [replace-cells-at self row column (clone =launchpad=)])

;;; worlds.lisp ends here
