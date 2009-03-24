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

;; <: worlds :>

(define-prototype world
    (:documentation "An RLX game world filled with cells.")
  (player :documentation "The player cell.")
  (width :documentation "The width of the world map, measured in tiles.")
  (height :documentation "The height of the world map, measured in tiles.")
  ;; <: cells :>
  (grid :documentation "A two-dimensional array of adjustable vectors of cells.")
  ;; <: environment :>
  (environment-grid :documentation "A two-dimensional array of environment data cells.")
  ;; <: lighting :>
  (light-grid 
   :documentation 
   "A 2d array of integers giving the light level at that point in <grid>.")
  (ambient-light :initform :total :documentation 
		 "Radius of ambient visibility. :total means that lighting is turned off.")
  ;; <: action-points :>
  (phase-number :initform 1 :documentation "Integer number of current phase.")
  (turn-number :initform 1 :documentation "Integer number of elapsed user turns (actions).")
  ;; <: queueing :>
  (message-queue :initform (make-queue))
  ;; <: narration :>
  (narrator :documentation "The narration widget object.")
  ;; <: browsing :>
  (browser :documentation "The browser object."))

(defparameter *default-world-axis-size* 10)
(defparameter *default-world-z-size* 4)

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
  [create-grid self :width <width> :height <height>])

(define-method environment-at world (row column)
  (aref <environment-grid> row column))

(define-method environment-condition-at world (row column condition)
  (field-value condition (aref <environment-grid> row column)))

(define-method set-environment-condition-at world (row column condition value)
  (setf (field-value condition 
		     (aref <environment-grid> row column))
	value))

;; <: narration :>

(define-method set-narrator world (narrator)
  (setf <narrator> narrator))

(define-method set-browser world (browser)
  (setf <browser> browser))

(define-method cells-at world (row column)
  (aref <grid> row column))

(define-method drop-cell world (cell row column 
				     &optional &key loadout no-collisions)
  "Put CELL on top of the stack of cells at ROW, COLUMN. If LOADOUT is
non-nil, then the `loadout' method of the dropped cell is invoked
after dropping. If NO-COLLISIONS is non-nil, then an object is not
dropped on top of an obstacle."
  (if (not (array-in-bounds-p <grid> row column))
      (message "Warning: dropping cell off world at (~S, ~S)." row column)
      (when (or (null no-collisions)
		(not [obstacle-at-p self row column]))
	(prog1 t
	  (vector-push-extend cell (aref <grid> row column))
	  (setf (field-value :row cell) row)
	  (setf (field-value :column cell) column)
	  (when loadout
	    [loadout cell])))))

(define-method nth-cell world (n row column)
  (aref (aref <grid> row column) n))

(define-method get-player world ()
  <player>)

(define-method player-row world ()
  (field-value :row <player>))

(define-method player-column world ()
  (field-value :column <player>))

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
	       (when (and <narrator> 
			  ;; only narrate player-related messages
			  (or (eq player sender)
			      (eq player rec)))
		 ;; now print message
		 [narrate-message <narrator> sender method-key rec args])
	       ;; stop everything if player dies
	       (when (not [in-category player :dead])
		 (apply #'send sender method-key rec args))))))))
  
;; <: events :>
;; <: main :>

(define-method forward world (method-key &rest args)
  "Send unhandled messages to the player object.
This is where most world computations start, because nothing happens
in a roguelike until the user has pressed a key."
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
	;; <: action-points :>
	(unless [can-act player phase-number]
	  [end-phase player]
	  (incf <turn-number>)
	  (when (not [in-category <player> :dead])
	    [run-cpu-phase self])
	  (incf <phase-number>)
	  [begin-phase player])
	;; TODO fix this
	[render-lighting self player]))))
        
(define-method get-phase-number world ()
  <phase-number>)

(define-method run-cpu-phase world ()
  "Run all non-player actor cells."
  (with-message-queue <message-queue> 
    (let ((cells nil)
	  (cell nil)
	  (phase-number <phase-number>)
	  (player <player>)
	  (grid <grid>))
      (dotimes (i <height>)
	(dotimes (j <width>)
	  (setf cells (aref grid i j))
	  (dotimes (z (fill-pointer cells))
	    (setf cell (aref cells z))
	    ;; <: lighting :>
	    (when (or (eq player cell)
		      [is-light-source cell])
	      [render-lighting self cell])
	    (when (and (not (eq player cell))
		       [in-category cell :actor]
		       (not [in-category player :dead]))
	      [begin-phase cell]
	      ;; <: action-points :>
	      (loop while [can-act cell phase-number] do
		   [run cell]
		   [process-messages self]
		   [end-phase cell]))))))))

;; <: lighting :>

(define-method render-lighting world (cell)
  (let* ((light-radius (field-value :light-radius cell))
	 (ambient <ambient-light>)
	 (light-grid <light-grid>)
	 (grid <grid>)
	 (turn-number <turn-number>)
	 (source-row (field-value :row cell))
	 (source-column (field-value :column cell))
	 (total (+ light-radius 
		   (if (numberp ambient) ambient 0)))
	 (octagon nil)
	 (line nil))
    ;; don't bother lighting if everything is lit.
    (when (not (eq :total ambient))
      ;; draw only odd-radius octagons that have a center pixel
      (when (evenp total)
	(incf total))
      (labels ((light-square (row column)
		 (setf (aref light-grid row column) turn-number))
	       (collect-line-point (x y)
		 (push (list x y) line) nil)
	       (make-line (row column)
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
			 (setf line (nconc line (list (list row column))))))))
	       (light-line (row column)
		 (make-line row column)
		 (block lighting 
		   (dolist (point line)
		     do (destructuring-bind (r c) point
			  (when (array-in-bounds-p grid r c)
			    (light-square r c)
			    ;; should we stop lighting?
			    (when [category-at-p self r c :opaque]
			      (return-from lighting)))))))
	       (collect-octagon-point (r c)
		 (push (list r c) octagon) nil)
	       (light-octagon (row column radius)
		 (trace-octagon #'collect-octagon-point 
				row column radius :thicken)
		 (dolist (point octagon)
		   (destructuring-bind (row column) point
		     (light-line row column)))))
	(light-octagon source-row source-column total)))))

(define-method generate world (&optional parameters)
  "Generate a world, reading generation parameters from the plist
  PARAMETERS."  
  (declare (ignore parameters))
  nil)

(define-method deserialize world (sexp)
  "Load a saved world from Lisp data."
  (declare (ignore sexp))
  nil)

(define-method start world ()
  (with-message-queue <message-queue>
    [begin-phase <player>]))

(define-method delete-cell world (cell row column)
  (let* ((grid <grid>)
	 (square (aref grid row column)))
    (setf (aref grid row column)
	  (delete cell square :test #'eq))))

(define-method move-cell world (cell row column)
  (let* ((old-row (field-value :row cell))
	 (old-column (field-value :column cell)))
    [delete-cell self cell old-row old-column]
    [drop-cell self cell row column]))

;;; Standard tile-display viewport widget

;; A world may have one or more viewport widgets. In a standard
;; viewport, cells are represented onscreen by uniformly-sized
;; graphical tiles; worlds are viewed from a bird's-eye
;; perspective. Other viewports may render schematic or compressed
;; views of the world (for example, an auto-map display.)

(define-prototype viewport 
    (:parent =widget= :documentation "A map display for RLX worlds.")
  (world :documentation "The world object to be displayed.")
  (origin-x :initform 0 
	    :documentation "The world x-coordinate of the tile at the viewport's origin.")
  (origin-y :initform 0 
	    :documentation "The world y-coordinate of the tile at the viewport's origin.")
  (origin-width :initform 10 :documentation "The width in tiles of the viewport.")
  (origin-height :initform 10 :documentation "The height in tiles of the viewport.")
  (tile-size :initform 16 :documentation "Size in pixels of a tile. They must be square."))

(define-method set-world viewport (world)
  (setf <world> world))
    
(define-method render viewport ()
  [adjust self] ;; hehe
  (let* ((world <world>)
	 (origin-width <origin-width>)
	 (origin-height <origin-height>)
	 (origin-x <origin-x>)
	 (origin-y <origin-y>)
	 (image <image>)
	 (tile-size <tile-size>)
	 objects cell)
    (with-field-values (grid light-grid environment-grid phase-number
			     turn-number ambient-light) world
      ;; blank the display
      [clear self]
      ;; draw the tiles
      (dotimes (i origin-height)
	(dotimes (j origin-width)
	  ;; is this square lit? 
	  ;; <: lighting :>
	    (if (array-in-bounds-p grid (+ i origin-y) (+ j origin-x))
		(when (or (eq :total ambient-light)
			  (= turn-number (aref light-grid (+ i origin-y) (+ j origin-x))))
		  (progn (setf objects (aref grid 
					     (+ i origin-y)
					     (+ j origin-x)))
			 (dotimes (k (fill-pointer objects))
			   (setf cell (aref objects k))
			   (when (object-p cell)
			     (draw-resource-image (field-value :tile cell)
						  (* j tile-size) (* i tile-size)
						  :destination image)))))
		;; not in bounds; draw blackness
		(draw-resource-image ".blackness" (* j tile-size) (* i tile-size)
				     :destination image))))
      ;; update geometry
      (setf <width> (* tile-size origin-width))
      (setf <height> (* tile-size origin-height)))))



(define-method set-origin viewport (&key x y height width)
  (setf <origin-x> x
	<origin-y> y
	<origin-width> width
	<origin-height> height))

(defparameter *viewport-margin* 6)

(define-method adjust viewport ()
  "Move the viewport's origin if required to keep the player onscreen."
  (let* ((world <world>)
	 (world-width (field-value :width world))
	 (world-height (field-value :height world))
	 (player (field-value :player world))
	 (player-x (field-value :column player))
	 (player-y (field-value :row player))
	 (origin-x <origin-x>)
	 (origin-y <origin-y>)
	 (origin-height <origin-height>)
	 (origin-width <origin-width>))
    ;; are we outside the "comfort zone"?
    (when (or 
	   ;; too far left
	   (> (+ origin-x *viewport-margin*) 
	      player-x)
	   ;; too far right
	   (> player-x
	      (- (+ origin-x origin-width)
		 *viewport-margin*))
	   ;; too far up
	   (> (+ origin-y *viewport-margin*) 
	      player-y)
	   ;; too far down 
	   (> player-y 
	      (- (+ origin-y origin-height)
		 *viewport-margin*)))
      ;; yes. recenter.
      (setf <origin-x> 
	    (max 0
		 (min (- world-width origin-width)
		      (- player-x 
			 (truncate (/ origin-width 2))))))
      (setf <origin-y> 
	    (max 0 
		 (min (- world-height origin-height)
		      (- player-y 
			 (truncate (/ origin-height 2)))))))))

;;; Metaworlds: implementing larger maps as grids of worlds

;; (define-prototype metaworld 
    

;;; worlds.lisp ends here
