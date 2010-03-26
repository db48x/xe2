;; cons.lisp --- a game about lisp

;; Copyright (C) 2010  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: games

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

(defpackage :cons-game
  (:documentation "CONS is an alternate universe sci-fi shooter game.")
  (:use :xe2 :common-lisp)
  (:export cons-game))

(in-package :cons-game)

(setf xe2:*dt* 20)

;;; Text labels

(defcell label 
  (categories :initform '(:drawn :actor :label))
  text stroke-color background-color timeout)

(define-method initialize label (&key text (stroke-color ".white") (background-color ".gray30")
					(style :label) (timeout nil) name tile description)
  (setf <text> text) 
  (when tile (setf <tile> tile))
  (when name (setf <name> name))
  (when description (setf <description> description))
  (setf <stroke-color> stroke-color)
  (setf <background-color> background-color)
  (setf <style> style)
  (setf <timeout> (if (floatp timeout)
		      ;; specify in (roughly) seconds if floating
		      (truncate (* 15 timeout)) ;; TODO fixme
		      ;; leave as frames if integer
		      timeout)))
  
(define-method draw label (x y image)
  (clon:with-field-values (text style) self
    (let* ((offset (ecase style
		     (:label 16)
		     (:flat 0)))
	   (x0 x)
	   (y0 y)
	   (x1 (+ x0 offset))
	   (y1 y0)
	   (margin 4)
	   (height (+ (* 2 margin) (apply #'+ (mapcar #'formatted-line-height text))))
	   (width (+ (* 2 margin) (apply #'max (mapcar #'formatted-line-width text)))))
      (draw-box x1 y1 width height 
		:stroke-color <stroke-color>
		:color <background-color>
		:destination image)
      ;; (when (eq style :label)
      ;; 	(draw-line x0 y0 x1 y1 :destination image))
      (let ((x2 (+ margin x1))
	    (y2 (+ margin y1)))
	(dolist (line text)
	  (render-formatted-line line x2 y2 :destination image)
	  (incf y2 (formatted-line-height line)))))))

(define-method run label ()
  [expend-default-action-points self]
  (when (integerp <timeout>)
    (when (minusp (decf <timeout>))
      [die self])))

;;; List body segments

(defcell segment 
  (tile :initform "segment")
  (item-tile :initform nil :documentation "When non-nil, superimpose this tile.")
  (description :initform "List snake body segment.")
  (direction :initform :north :documentation "When non-nil, move once in this direction.")
  (last-direction :initform :north)
  (movement-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (categories :initform '(:actor :opaque :target :segment :drawn))
  (team :initform :player))

(define-method run segment ()
  (when <direction>
    [move self <direction>]
    (setf <direction> nil)))

(define-method move segment (direction)
  (setf <last-direction> direction)
  [parent>>move self direction :ignore-obstacles])

(define-method queue-move segment (direction)
  (setf <direction> direction))

(define-method show-item segment (item-tile)
  (setf <item-tile> item-tile))

(define-method draw segment (x y image)
  (draw-resource-image <tile> x y :destination image)
  (when <item-tile>
    (draw-resource-image <item-tile> x y :destination image)))

;;; Agent: the player

(defcell agent 
  (tile :initform "agent-north")
  (description :initform "You are a sentient warrior cons cell.")
  (segments :initform nil)
  (items :initform nil)
  (direction :initform :north)
  (last-direction :initform :north :documentation "Last direction actually moved.")
  (dead :initform nil)
  (last-turn-moved :initform 0)
  (team :initform :player)
  (hit-points :initform (make-stat :base 20 :min 0 :max 20))
  (movement-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 10 :min 0 :max 25))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 15)
  (movement-cost :initform (make-stat :base 10))
  (stepping :initform t)
  (light-radius :initform 7)
  (categories :initform '(:actor :obstacle :player :target :container :light-source)))

(define-method loadout agent ()
  (clon:with-field-values (row column) self
    (dotimes (n 3)
      [add-segment self (- (+ row 4) n) column])))

(define-method hit agent (&optional object)
  [play-sample self "ouch"]
  [parent>>damage self 1])

(define-method pause agent ()
  [pause *world*])

(defparameter *agent-tiles* '(:north "agent-north"
			     :south "agent-south"
			     :east "agent-east"
			     :west "agent-west"))

(define-method aim agent (direction)
  (setf <direction> direction)
  (setf <tile> (getf *agent-tiles* direction)))

(define-method move agent (&optional direction)
  (unless <dead>
    (let ((phase (field-value :phase-number *world*))
	  (dir (or direction <direction>)))
      (unless (= <last-turn-moved> phase)
	(setf <last-turn-moved> phase)
	[aim self dir]
	(when [parent>>move self dir]
	  [move-segments self]
	  (setf <last-direction> dir))))))

(define-method move-segments agent ()
  (clon:with-field-values (items last-direction segments) self
    (let ((next-dir last-direction))
      (dolist (segment segments)
	[queue-move segment next-dir]
	(setf next-dir (field-value :last-direction segment))))))

(define-method update-tiles agent ()
  (clon:with-field-values (items segments) self
    (let ((n 0)
	  (num-items (length items)))
      (dolist (segment segments)
	[show-item segment (when (< n num-items)
			     (field-value :tile (nth n items)))]
	(incf n)))))

(define-method add-segment agent (&optional force-row force-column)
  (clon:with-fields (segments) self
    (multiple-value-bind (row column)
	(if (or (null segments) (or force-row force-column))
	    (step-in-direction <row> <column> (opposite-direction <last-direction>))
	    (when (and (consp segments)
		       (consp (last segments))
		       (clon:object-p (car (last segments))))
	      (clon:with-field-values (row column last-direction) (car (last segments))
		(step-in-direction row column (opposite-direction last-direction)))))
      (let ((segment (clone =segment=)))
	[drop-cell *world* segment (or force-row) (or force-column column)]
	(push segment segments)))))

(define-method space-at-head agent ()
  (step-in-direction <row> <column> <direction>))

(define-method category-at-head agent (category)
  (multiple-value-bind (row column) 
      [space-at-head self]
    [category-at-p *world* row column category]))

(define-method item-at-head agent ()
  [category-at-head self :item])

(define-method obstacle-at-head agent ()
  [category-at-head self :obstacle])
  
(define-method push agent () 
  ;; TODO verify enough segments
  (let ((item [item-at-head self]))
    (if item
	(progn (push item <items>)
	       [delete-from-world item])
	[say self "Nothing to push."])))
	
(define-method pop agent ()
  (clon:with-fields (items) self
    (multiple-value-bind (row column)
	[space-at-head self]
      (if [category-at-head self :obstacle]
	  [say self "Cannot drop item."]
	  (progn
	    (let ((item (car items)))
	      (if (clon:object-p item)
		  (progn (setf items (delete item items))
			 [drop-cell *world* item row column])
		  [say self "Nothing to drop."])))))))
	       
(define-method rotate agent () 
  (clon:with-fields (items) self
    (let ((tail (pop items)))
      (setf items (append items (list tail))))))

(define-method call agent ()
  (let ((item (car <items>)))
    (if (and item [in-category item :item]
	     (clon:has-method :call item))
	[call item self]
	[say self "Cannot call."])))

(define-method run agent () 
  [update-tiles self])
  
(define-method quit agent ()
  (xe2:quit :shutdown))

(define-method die agent ()
  (unless <dead>
    (setf <tile> "agent-disabled")
    [play-sample self "gameover"]
    [say self "You died. Press escape to reset."]
    (setf <dead> t)))

(define-method restart agent ()
  (let ((agent (clone =agent=)))
    [say self "Restarting CONS..."]
    (halt-sample t)
    [destroy *universe*]
    [set-player *universe* agent]
    [set-player *form* agent]
    [set-character *status* agent]
    [play *universe*
	  :address '(=highway=)]
    [loadout agent]))

;;; Inert blocks

(defcell block 
  (tile :initform "block")
  (team :initform :neutral)
  (categories :initform '(:item :obstacle :target)))

;;; Bombs!

(defcell bomb
  (tile :initform "bomb")
  (categories :initform '(:item :obstacle :target)))

(defun same-team (obj1 obj2)
  (eq (field-value :team obj1)
      (field-value :team obj2)))

;;; Particle gun

(defcell particle 
  (tile :initform "particle")
  (movement-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 5 :min 0 :max 10))
  (team :initform :player)
  (categories :initform '(:actor))
  (direction :initform :north))

(define-method initialize particle (direction)
  (setf <direction> direction))

(define-method run particle ()
  (multiple-value-bind (r c) (step-in-direction <row> <column> <direction>)
    (let ((thing (or [category-at-p *world* r c :obstacle]
		     [category-at-p *world* r c :target])))
      (if (null thing)
	  [move self <direction>]
	  (progn (when (and (clon:has-method :hit thing)
			    (not (same-team self thing)))
		   [hit thing])
		 [die self])))))

(defcell gun
  agent
  (tile :initform "gun")
  (categories :initform '(:item :target :function)))

(define-method call gun (caller)
  (clon:with-field-values (direction row column) caller
    (multiple-value-bind (r c) (step-in-direction row column direction)
      [play-sample caller "fire"]
      [drop-cell *world* (clone =particle= direction) r c])))

;;; Storage container 

(defcell orange-barrier
  (description :initform "Impenetrable barrier.")
  (tile :initform "orangeworld")
  (categories :initform '(:obstacle)))

(defcell blue-brick
  (description :initform "Breakable brick.")
  (hit-points :initform (make-stat :base 20 :min 0))
  (tile :initform "darkorangeworld2")
  (categories :initform '(:obstacle)))

(define-method hit blue-brick ()
  [play-sample self "break"]
  [damage self 1])

(define-method die blue-brick ()
  [play-sample self "break2"]
  [parent>>die self])

(defcell purple-brick
  (description :initform "Impenetrable barrier.")
  (hit-points :initform (make-stat :base 10 :min 0))
  (tile :initform "darkorangeworld3")
  (categories :initform '(:obstacle)))

(defcell orange-barrier4
  (description :initform "Impenetrable barrier.")
  (tile :initform "darkorangeworld4")
  (categories :initform '(:obstacle)))

(defcell orange-road
  (description :initform "Security vehicle transit area.")
  (tile :initform "darkorangeworld"))

(define-prototype storage (:parent xe2:=world=)
  (description :initform "Security logging station.")
  (height :initform 120)
  (width :initform 120)
  (level :initform 1)
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (scale :initform '(1 xm))
  (edge-condition :initform :block)
  (grammar :initform 
	   '((world >> (=launchpad= :color :drop
			90 :right
			20 :jump 
			=gun= :color :drop
			90 :left
		        30 :jump
			90 :left
			:pushloc security-structure :poploc
			90 :right 90 :right 40 :jump
			security-structure))
	     (side-chamber >> (:pushloc
			       room3 90 random-turn
			       room3 90 :left
			       1 :jump
			       gun-maybe
			       :poploc))
	     (gun-maybe >> :noop :noop (=gun= :color :drop))
	     (security-structure >> (room 90 :left 
				     room 90 :left 
				     room 90 :left
				     room 90 :right
				     room2 45 random-turn room2 45 random-turn room2
				     side-chamber))
             (random-turn >> :right :left)
	     (random-barrier >> =orange-barrier= =purple-brick= =orange-barrier4=)
	     (room >> (=orange-barrier= :color 
		       10 :draw 
		       90 :right 
		       4 :draw
		       1 :jump
		       3 :draw
		       90 :right 
		       10 :draw))
	     (room2 >> (random-barrier :color 
			5 :draw 
			90 :right 
			5 :draw 
			90 :right 
			4 :draw
			2 :jump
			3 :draw
			90 :right
		        10 :draw))
	     (room3 >> (=blue-brick= :color 
			3 :draw 
			90 :right 
			4 :draw 
			90 :right 
			4 :draw 
			90 :right
		        4 :draw)))))

(define-method generate storage (&rest params)
  [create-default-grid self]
  (dotimes (row <height>)
    (dotimes (column <width>)
      [drop-cell self (clone =orange-road=) row column]))
  [parent>>generate self])

(define-method begin-ambient-loop storage ()
  (play-music "beatup" :loop t))

;;; Basic level

(defcell road
  (description :initform "Security vehicle transit area.")
  (tile :initform "darkcyanworld"))

(defcell barrier
  (description :initform "Impenetrable barrier.")
  (tile :initform "cyanworld")
  (categories :initform '(:obstacle)))

(define-prototype highway (:parent xe2:=world=)
  gen-row gen-column 
  ;;
  (description :initform "You enter a long corridor.")
  (level :initform 1)
  ;;
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (scale :initform '(3 m))
  (edge-condition :initform :block))

(define-method generate highway (&key (height 80)
					    (width 50))
  (setf *notes* nil)
  (setf <height> height <width> width)
  [create-default-grid self]
  (labels ((drop-barrier (r c)
	     (prog1 nil
	       [drop-cell self (clone =barrier=) r c])))
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =road=)
		 i j]))
    (dotimes (i 20)
      [drop-cell self (clone =block=) (random height) (random width)])
    (dotimes (i 20)
      [drop-cell self (clone =gun=) (random height) (random width)])
    (dotimes (i 20)
      [drop-cell self (clone =bomb=) (random height) (random width)])
    (dotimes (i 25)
      (let ((draw-function (if (= 0 (random 3))
			       #'trace-row #'trace-column)))
	(funcall draw-function #'drop-barrier
		 (+ 10 (random 50))
		 (+ 10 (random 50))
		 (+ 10 (random 50)))))
    [drop-cell self (clone =launchpad=) 10 10]))

(define-method begin-ambient-loop highway ()
  (play-music "beatup" :loop t))

;;; Splash screen
  
(defvar *pager* nil)

(define-prototype splash (:parent =widget=))

(define-method render splash ()
  (xe2:draw-resource-image "splash" 0 0 
			   :destination <image>))

(defvar *space-bar-function*)

(define-method dismiss splash ()
  [select *pager* :game]
  (when (functionp *space-bar-function*)
    (funcall *space-bar-function*))
  (xe2:show-widgets))

(define-prototype splash-prompt (:parent =prompt=)
  (default-keybindings :initform '(("SPACE" nil "dismiss ."))))

;;; Key bindings

(defparameter *numpad-keybindings* 
  '(("KP8" nil "aim :north .")
    ("KP4" nil "aim :west .")
    ("KP6" nil "aim :east .")
    ("KP2" nil "aim :south .")
    ;; arrows
    ("UP" nil "aim :north .")
    ("LEFT" nil "aim :west .")
    ("RIGHT" nil "aim :east .")
    ("DOWN" nil "aim :south .")))

(defparameter *qwerty-keybindings*
  (append *numpad-keybindings*
	  '(("K" nil "aim :north .")
	    ("H" nil "aim :west .")
	    ("L" nil "aim :east .")
	    ("J" nil "aim :south .")
	    ;;
	    ("Z" nil "push .")
	    ("X" nil "pop .")
	    ("C" nil "call .")
	    ("V" nil "rotate .")
	    ("SPACE" nil "move .")
	    ;;
	    ("P" (:control) "pause .")
	    ("PAUSE" nil "pause .")
	    ("ESCAPE" nil "restart .")
	    ("Q" (:control) "quit ."))))
  
(define-prototype cons-prompt (:parent xe2:=prompt=))

(define-method install-keybindings cons-prompt ()
  (dolist (k *qwerty-keybindings*)
      (apply #'bind-key-to-prompt-insertion self k)))

(define-method install-keybindings cons-prompt ()
  (let ((keys (ecase xe2:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:alternate-qwerty *alternate-qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k))))

;;; Custom formatter pauses when shown; it's the help screen

(define-prototype cons-formatter (:parent xe2:=formatter=))

(define-method render cons-formatter ()
  [pause *world* :always]
  [parent>>render self])

;;; Player status

(defvar *status* nil)

(define-prototype status (:parent xe2:=formatter=)
  (character :documentation "The character cell."))

(define-method set-character status (character)
  (setf <character> character))

(define-method print-stat status (stat-name &key warn-below show-max)
  (let* ((stat (field-value stat-name <character>))
	 (value [stat-value <character> stat-name]))
    (destructuring-bind (&key min max base delta unit) stat
      (let ((color (if (and (numberp warn-below)
			    (< value warn-below))
		       ".red"
		       ".gray40")))
	[print self (symbol-name stat-name)
	       :foreground ".white"]
	[print self ":["]
	[print self (format nil "~S" value) 
	       :foreground ".yellow"
	       :background color]
	(when show-max
	  [print self (format nil "/~S" max)
		 :foreground ".yellow"
		 :background color])
	(when unit 
	  [print self " "]
	  [print self (symbol-name unit)])
	[print self "]"]
	))))

(defparameter *status-bar-character* " ")

(define-method print-stat-bar status (stat &key 
					   (color ".yellow")
					   (background-color ".gray40")
					   (divisor 1))
  (let ((value (truncate (/ [stat-value <character> stat] divisor)))
	(max (truncate (/ [stat-value <character> stat :max] divisor))))
    (dotimes (i max)
      [print self *status-bar-character*
	     :foreground ".yellow"
	     :background (if (< i value)
			     color
			   background-color)])))

(define-method update status ()
  [delete-all-lines self]
  (let* ((char <character>))
    (when char
	[print-stat self :hit-points :warn-below 7 :show-max t]
	[print-stat-bar self :hit-points :color ".red"]
	[space self]
	[newline self])))

;;; Custom bordered viewport

(define-prototype view (:parent xe2:=viewport=))

(defvar *viewport*)

(define-method render view ()
  [parent>>render self]
  (xe2:draw-rectangle 0 0 
		      <width>
		      <height>
		      :color ".blue" :destination <image>))

;;; Joystick screen.

(defvar *form*)
  
(defparameter *default-commands*
  '(("aim :north ." "UP")
    ("aim :south ." "DOWN")
    ("aim :east ." "RIGHT")
    ("aim :west ." "LEFT")
    ("push ." "Z")
    ("pop ." "X")
    ("call ." "C")
    ("rotate ." "V")
    ("move ." "SPACE")
    ("quit ." "Q" :control)))

(define-prototype joystick-world (:parent =world=)
  (height :initform 18)
  (width :initform 4)
  (prompt :initform nil))

(define-method set-prompt joystick-world (prompt)
  (setf <prompt> prompt))

(define-method configure-keybindings joystick-world ()
  (clon:with-field-values (prompt variables) self
    (assert (hash-table-p variables))
    [clear-keymap prompt]
    (labels ((install (command event)
	       (destructuring-bind (key &rest modifiers) event
		 (message "Installing ~S" event)
		 (bind-key-to-prompt-insertion prompt key modifiers command))))
      (message "Configuring ~S keybindings." (hash-table-count variables))
      (maphash #'install variables))))

(define-method generate joystick-world ()
  [create-default-grid self]
  ;; todo write 
  (let ((row 1))
    (labels ((drop-config-row (command event)
	       (let ((event-cell (clone =event-cell=))
		     (var-cell (clone =var-cell= command)))
		 [drop-cell self event-cell row 1]
		 [set event-cell event]
		 [drop-cell self var-cell row 2])
	       (incf row)))
      (let ((c1 (clone =comment-cell= "Keypress"))
	    (c2 (clone =comment-cell= "Resulting action")))
	[drop-cell self c1 row 1]
	[drop-cell self c2 row 2]
	(incf row))
      (dolist (command *default-commands*)
	(destructuring-bind (command-string &rest event) command
	  (drop-config-row command-string event)))
      (incf row)
      [drop-cell self (clone =button-cell= 
			     :closure #'(lambda () 
					  [configure-keybindings self])
			     :text "  APPLY  ")
		 row 2])))
			
;;; Main program. 

(defparameter *cons-window-width* 800)
(defparameter *cons-window-height* 600)

(defun cons-game ()
  (xe2:message "Initializing CONS...")
  (setf xe2:*window-title* "CONS")
  (clon:initialize)
  (xe2:set-screen-height *cons-window-height*)
  (xe2:set-screen-width *cons-window-width*)
  (let* ((prompt (clone =cons-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (player (clone =agent=))
	 (splash (clone =splash=))
	 (help (clone =cons-formatter=))
	 (quickhelp (clone =formatter=))
	 (form (clone =form=))
	 (viewport (clone =view=))
	 (status (clone =status=))
	 (form-prompt (clone =prompt=))
	 (splash-prompt (clone =splash-prompt=))
	 (terminal (clone =narrator=))
	 (stack (clone =stack=)))
    ;;
    (setf *form* form)
    (setf *viewport* viewport)
    (setf *status* status)
    ;;
    [resize splash :height (- *cons-window-height* 20) :width *cons-window-width*]
    [move splash :x 0 :y 0]
    [resize splash-prompt :width 10 :height 10]
    [move splash-prompt :x 0 :y 0]
    [hide splash-prompt]
    [set-receiver splash-prompt splash]
    ;;
    [resize *status* :height 20 :width *cons-window-width*]
    [move *status* :x 0 :y 0]
    ;;
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    ;; 
    [resize form :height 500 :width 800]
    [move form :x 0 :y 0]
    ;;
    [resize form-prompt :height 20 :width *cons-window-width*]
    [move form-prompt :x 0 :y (- *cons-window-height* 20)]
    [show form-prompt]
    [install-keybindings form-prompt]
    [set-receiver form-prompt form]
    ;;
    (labels ((spacebar ()
	       ;;
	       (xe2:halt-music 1000)
	       (setf xe2:*physics-function* #'(lambda (&rest ignore)
						(when *world* [run-cpu-phase *world* :timer]
						      ;; (maphash #'(lambda (&rest args)
						      ;; 		   (message "VAR: ~S" args))
						      ;; 	       (field-value :variables *world*))
						      )))
	       (xe2:enable-held-keys 1 3)
	       ;;
	       [set-player universe player]
	       [play universe
	       	     :address '(=storage=)
	       	     :prompt prompt
	       	     :narrator terminal
	       	     :viewport viewport]
	       [loadout player]
	       (let ((config-screen (clone =joystick-world=)))
		 [generate config-screen]
		 [set-prompt config-screen prompt]
		 [configure form config-screen])
	       ;;
	       [set-character *status* player]
	       ;;
	       [set-tile-size viewport 16]
	       [resize viewport :height 470 :width *cons-window-width*]
	       [move viewport :x 0 :y 0]
	       [set-origin viewport :x 0 :y 0 
			   :height (truncate (/ (- *cons-window-height* 130) 16))
			   :width (truncate (/ *cons-window-width* 16))]
	       [adjust viewport]))
      (setf *space-bar-function* #'spacebar))
    
    [resize help :height 540 :width 800] 
    [move help :x 0 :y 0]
    (let ((text	(find-resource-object "help-message")))
      (dolist (line text)
    	(dolist (string line)
    	  (funcall #'send nil :print-formatted-string help string))
    	[newline help]))
    ;;
    [resize quickhelp :height 85 :width 250] 
    [move quickhelp :y (- *cons-window-height* 130) :x (- *cons-window-width* 250)]
    (let ((text	(find-resource-object "quickhelp-message")))
      (dolist (line text)
    	(dolist (string line)
    	  (funcall #'send nil :print-formatted-string quickhelp string))
    	[newline quickhelp]))
    ;;
    (play-music "cons" :loop t)
    (set-music-volume 255)	       
    ;;
    [resize stack :width *cons-window-width* :height (- *cons-window-height* 20)]
    [move stack :x 0 :y 0]
    [set-children stack (list viewport terminal status)]
    ;;
    [resize terminal :height 80 :width *cons-window-width*]
    [move terminal :x 0 :y (- *cons-window-height* 80)]
    [set-verbosity terminal 0]
    ;;
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    (xe2:install-widgets splash-prompt splash)
    [add-page *pager* :config (list form)]
    [add-page *pager* :game (list prompt stack viewport terminal quickhelp *status*)]
    [set-page-property *pager* :game :held-keys :t]
    [add-page *pager* :help (list help)]
))


(cons-game)



(provide 'cons)
;;; cons.lisp ends here
