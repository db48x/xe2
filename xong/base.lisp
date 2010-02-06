(in-package :xong)

;;; Colors

(defparameter *colors* '(:purple :red :blue :orange :green :yellow :white))

;;; Gates 

(defparameter *gate-timeout* 70)

(defcell gate
  (categories :initform '(:actor :obstacle :gate :exclusive))
  (speed :initform (make-stat :base 10))
  (tile :initform "gate-closed")
  (clock :initform 0)
  (description :initform "Opens for a brief time when hit with the puck."))

(define-method open gate ()
  [delete-category self :obstacle]
  (setf <tile> "gate-open")
  (setf <clock> *gate-timeout*))

(define-method close gate ()
  [add-category self :obstacle]
  [play-sample self "gate-closing-sound"]
  (setf <tile> "gate-closed"))

(define-method is-open gate ()
  (let ((retval (null [in-category self :obstacle])))
    (prog1 retval (message "IS-OPEN: ~S" retval))))

(define-method run gate ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (zerop <clock>)
    [close self]))

;;; Door to next level

(define-prototype door (:parent xe2:=gateway=)
  (tile :initform "door")
  (name :initform "Level exit")
  (description :initform "Door to the next level of Xong.")
  (categories :initform '(:gateway :actor :exclusive :obstacle))
  (address :initform nil))

(define-method level door (lev)
  (setf <address> (generate-level-address lev)))

(define-method step door (stepper)
  (when [is-player stepper]
    (if (and (zerop *enemies*) (not (snake-living-p)))
	(progn 
	  (score 20000)
	  [play-sample self "go"]
	  [say self "You made it to the next level!"]
	  [activate self])
	[play-sample self "error"])))

(define-method run door ()
  (when (and (zerop *enemies*) (not (snake-living-p)))
    [delete-category self :obstacle]
    (setf <tile> "door-open")))
	
;;; Breakable paint walls re-color the ball

(defvar *wall-tiles* '(:purple "wall-purple"
			:black "wall-black"
			:red "wall-red"
			:blue "wall-blue"
			:orange "wall-orange"
			:green "wall-green"
			:white "wall-white"
			:yellow "wall-yellow"))

(defcell wall 
  (name :initform "Paint block")
  (tile :initform "wall-purple")
  (description :initform
"These blocks of paint can be broken using the puck to 
reach new areas and items. The puck also picks up the color.")
  (categories :initform '(:exclusive :obstacle :wall))
  (color :initform :purple))

(define-method paint wall (c)
  (setf <color> c)
  (let ((res (getf *wall-tiles* c)))
    (assert (stringp res))
    (setf <tile> res)))

(define-method die wall ()
  (score 100)
  [parent>>die self])

;;; The floor

(defcell floor
  (tile :initform "floor")
  (color :initform ".black"))

;;; Radioactive gas

(defcell plasma
  (tile :initform "plasma-white")
  (color :initform :white)
  (name :initform "Toxic paint plasma")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (clock :initform 100)
  (categories :initform '(:actor :paint-source :plasma))
  (description :initform "Spreading toxic paint gas. Avoid at all costs!"))

(define-method step plasma (stepper)
  (when [is-player stepper]
    [damage stepper 1]))

(define-method set-color plasma (color)
  (setf <color> color)
  (setf <tile> (getf *plasma-tiles* color)))

(define-method set-clock plasma (clock)
  (setf <clock> clock))

(define-method run plasma ()
  [play-sample self "plasma"]
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      (progn 
	(do-cells (cell [cells-at *world* <row> <column>])
	  (when (has-field :hit-points cell)
	    [damage cell 1]))
	(let ((dir (random-direction)))
	  (multiple-value-bind (r c) (step-in-direction <row> <column> dir)
	    (let ((brick [category-at-p *world* r c :wall]))
	      (if brick
		  (progn 
		    [paint brick <color>]
		    [die self])
		  [move self dir])))))))

;;; Bulkheads are indestructible walls

(defcell bulkhead
  (name :initform "Bulkhead")
  (tile :initform "bulkhead")
  (categories :initform '(:obstacle :bulkhead :exclusive))
  (description :initform "It's an indestructible wall."))

;;; Xong game board

(defun generate-level-address (n)
  (assert (and (integerp n) (plusp n)))
  (list '=xong= 
	:level n
	:extenders (truncate (/ (* 3 (1- n)) 2))
	:tracers (+ 4 (truncate (/ (* (1- n) 2) 3)))
	:monitors (if (= n 1)
		      0
		      (* 2 (truncate (/ n 2))))
	:rooms 1
	:mystery-boxes (+ 1 (truncate (/ n 2)))
	:oscillators (* (max 0 (- n 2)) (truncate (/ n 4)))
	:puzzle-length (+ 4 (truncate (/ n 3)))
	:extra-holes (+ 1 (truncate (/ n 3)))
	:puckups (+ 4 (truncate (* (1- n) 2.5)))
	:diamonds (+ 9 (* (1- n) 3))
	:swatches (+ 10 (truncate (* 1.6 n)))))

(defparameter *xong-level-width* 50)
(defparameter *xong-level-height* 29)

(define-prototype xong (:parent xe2:=world=)
  (name :initform "Xong board")
  (description :initform 
	       '((("Welcome to Xong." :foreground ".white" :background ".blue")
		  ("Press F1 for general help" :foreground ".white" :background ".red")
		  (", or click any object." :foreground ".white" :background ".blue"))))
  (edge-condition :initform :block)
  (level :initform 1)
  (width :initform *xong-level-width*)
  (height :initform *xong-level-height*)
  (scale :initform '(1 nm))
  (ambient-light :initform :total))

(define-method drop-snake xong (column row1 row2)
  (setf *snake* nil)
  (let (piece last-piece)
    (labels ((drop-piece (r c)
	       (progn nil
		      (setf piece (clone =snake=))
		      (push piece *snake*)
		      [drop-cell *world* piece r c]
		      [set-color piece (car (one-of *colors*))]
		      (when last-piece
			[attach last-piece piece])
		      (setf last-piece piece))))
      (trace-column #'drop-piece column row1 row2))))

(define-method drop-room xong (row column height width 
				   next-level puzzle-length &optional (material =bulkhead=))
  (let (rectangle openings)
    (labels ((collect-point (&rest args)
	       (prog1 nil (push args rectangle)))
	     (drop-wall (r c)
	       (unless (and (= r 0)
			    (= c 0))
		 (let ((wall (clone =bulkhead=)))
		   [replace-cells-at self r c wall]
		   [set-location wall r c]))))
      (trace-rectangle #'collect-point row column height width)
      ;; make sure there are openings
      (dotimes (i 6)
	(let* ((n (random (length rectangle)))
	       (point (nth n rectangle)))
	  (destructuring-bind (r c) point
	    ;; don't make gate holes on corners or above exit
	    (unless (or (and (= r row) (= c (+ -1 column (truncate (/ width 2)))))
			(and (= r row) (= c (+ column (truncate (/ width 2)))))
			(and (= r row) (= c (+ 1 column (truncate (/ width 2)))))
			(and (= r row) (= c column)) ;; top left 
			(and (= r row) (= c (+ -1 column width))) ;; top right
			(and (= r (+ -1 row height)) (= c column)) ;; bottom left
			(and (= r (+ -1 row height)) (= c (+ -1 column width)))) ;; bottom right
	      (push (nth n rectangle) openings)
	      (setf rectangle (delete (nth n rectangle) rectangle))))))
      ;; draw walls
      (dolist (point rectangle)
	(destructuring-bind (r c) point
	  (drop-wall r c)))
      ;; draw gates
      (dolist (point openings)
	(destructuring-bind (r c) point
	  [replace-cells-at self r c (clone =gate=)]))
      ;; drop floor, obliterating what's below
      (labels ((drop-floor (r c)
		 (prog1 nil
		   [replace-cells-at self r c (clone =floor=)])))
	(trace-rectangle #'drop-floor (1+ row) (1+ column) (- height 2) (- width 2) :fill)
	;; drop lock puzzle
	(let ((col (+ column (truncate (/ width 2)))))
	  (trace-column #'drop-wall (- col 1) row (+ 1 row puzzle-length))
	  [drop-snake self col (+ row 2) (+ 1 row puzzle-length)]
	  (trace-column #'drop-wall (+ col 1) row (+ 1 row puzzle-length))
	  ;; drop door
	  (let ((door (clone =door=)))
	    [level door next-level]
	    [drop-cell self (clone material) (+ 1 row) col]
	    [drop-cell self door (+ 2 row) col])
	  ;; drop a puck or two
	  (dotimes (n (1+ (random 2)))
	    [drop-cell self (clone =puckup=) (+ 2 (random 2) row)
		       (+ 2 (random 2) column)]))))))
  
(define-method generate xong (&key (level 1)
				   (extenders 0)
				   (tracers 4)
				   (rooms 1)
				   (mystery-boxes 2)
				   (oscillators 3)
				   (puzzle-length 4)
				   (puckups 4)
				   (extra-holes 4)
				   (monitors 3)
				   (diamonds 6)
				   (swatches 8))
  [create-default-grid self]
  (setf <level> level)
  (setf *enemies* 0)
  (clon:with-fields (height width grid player) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))
    (dotimes (n (+ swatches 3))
      ;; ensure all colors are present,
      ;; after that make it random
      (let ((color (if (< n (length *colors*))
		       (nth n *colors*)
		       (car (one-of *colors*)))))
	(labels ((drop-wall (r c)
		   (prog1 nil
		     (let ((wall (clone =wall=)))
		       [drop-cell self wall r c :exclusive t]
		       [paint wall color]))))
	  (multiple-value-bind (r c) [random-place self]
	    (unless (= 0 r c)
	      (let ((hr (+ r 2 (random 3)))
		    (hc (+ c 2 (random 3))))
		[replace-cells-at self hr hc
				  (clone =floor=)]
		[drop-cell self (clone =hole=) hr hc]
		(trace-rectangle #'drop-wall r c
				 (+ 4 (random 8)) (+ 4 (random 8)) :fill)))))))
    (dotimes (n extra-holes)
      (multiple-value-bind (r c) [random-place self]
	[drop-cell self (clone =hole=) r c]))
    (dotimes (n rooms)
      [drop-room self 
		 (+ 5 (random (- height 20)))
		 (+ 5 (random (- width 20)))
		 (+ 10 (random 6)) (+ 10 (random 4)) (+ level 1) puzzle-length])
    (dotimes (n monitors)
      (let ((monitor (clone =monitor=)))
	(multiple-value-bind (r c)
	    [random-place self :avoiding player :distance 10]
	  [drop-cell self monitor r c :loadout t])))
    ;; EXPERIMENTAL
    ;; (dotimes (n )
    ;;   (let ((balloon (clone =balloon= :text '((("This is some") (" formatted " :foreground ".red") ("text."))
    ;; 					      (("Here's some more."))) :style :balloon)))
    ;; 	(multiple-value-bind (r c)
    ;; 	    [random-place self :avoiding player :distance 10]
    ;; 	  (setf (field-value :tile balloon) "yasichi")
    ;; 	  [drop-cell self balloon r c :loadout t])))
    ;; ;; EXPERIMENTAL
    ;; (dotimes (n 1)
    ;;   (let ((npc (clone =beckoner=)))
    ;; 	(multiple-value-bind (r c)
    ;; 	    [random-place self :avoiding player :distance 10]
    ;; 	  [drop-cell self npc r c :loadout t])))
    ;;
    ;; EXPERIMENTAL
    ;; (dotimes (n 20)			
    ;;   (let ((p (clone =particle=)))
    ;; 	[loadout p]
    ;; 	[add-sprite self p]))
    ;; (dotimes (n 4)
    ;;   (let ((p (clone =yasichi=)))
    ;; 	[loadout p]
    ;; 	[add-sprite self p]))
    ;;
    ;;
    (dotimes (n tracers)
      (let ((tracer (clone =tracer=)))
	(multiple-value-bind (r c)
	    [random-place self :avoiding player :distance 10]
	  [drop-cell self tracer r c :loadout t])))
    (dotimes (n oscillators)
      (let ((oscillator (clone =oscillator=)))
	(multiple-value-bind (r c)
	    [random-place self :avoiding player :distance 10]
	  [drop-cell self oscillator r c :loadout t])))
    (dotimes (n extenders)
      (multiple-value-bind (r c) [random-place self]
	[drop-cell self (clone =extender=) r c]))
    (dotimes (n diamonds)
      (multiple-value-bind (r c) [random-place self]
	[drop-cell self (clone =diamond=) r c]))
    (dotimes (n puckups)
      (multiple-value-bind (r c) [random-place self]
	[drop-cell self (clone =puckup=) r c]))
    (dotimes (n mystery-boxes)
      (multiple-value-bind (r c) [random-place self]
	[drop-cell self (clone =mystery-box=) r c]))))

(define-method begin-ambient-loop xong ()  
  (play-music (car (one-of '("flyby" "sparqq" "synthy" "neon" "phong" "xong-theme" "pensive" "toybox"))) :loop t))

;;; Other level gates

(defcell portal 
  text
  (tile :initform "portal") 
  (name :initform "Portal")
  (categories :initform '(:above))
  (description :initform "A doorway to another level.")
  (address :initform nil))

(define-method initialize portal (&key address text)
  (when address
    (setf <address> address))
  (when text 
    (setf <text> text)))

(define-method loadout portal ()
  (when <text> [drop self (clone =balloon= :text <text>)]))

(define-method step portal (stepper)
  (when [is-player stepper]
    [play *universe* :address <address> :player [get-player *world*]]))

;;; Dancefloor

(defparameter *light-tiles* '("floor"
			      "rezlight5"
			      "rezlight4"
			      "rezlight3"
			      "rezlight2"
			      "rezlight1"))

(defparameter *light-clock* 12)

(defcell dancefloor 
  (tile :initform nil)
  (repeating :initform nil)
  (clock :initform *light-clock*)
  (categories :initform '(:actor :dancefloor)))

(define-method initialize dancefloor (&key repeating (clock *light-clock*))
  (setf <clock> clock)
  (setf <repeating> repeating))

(define-method update-tile dancefloor ()
  (when [is-located self]
    (setf <tile> 
	  (if [category-at-p *world* <row> <column> '(:obstacle :above)]
	      nil
	      (nth (truncate (/ <clock> 2)) *light-tiles*)))))

(define-method light dancefloor (&optional (time *light-clock*))
  (setf <clock> (max 0 time))
  [update-tile self])

(define-method light-toward dancefloor (direction)
  (multiple-value-bind (r c) (step-in-direction <row> <column> direction)
    (unless [category-at-p *world* r c :dancefloor]
      (let ((dancefloor (clone =dancefloor=)))
	[drop-cell *world* dancefloor r c]
	[light dancefloor (max 0 (- <clock> 1))]))))
      
(define-method light-plus dancefloor ()
  (dolist (dir '(:north :south :east :west))
    [light-toward self dir]))
		
(define-method run dancefloor ()
  (setf <clock> (max 0 (- <clock> 1)))
  (if (plusp <clock>)
      (progn
	(when (< 2 <clock>)
	  [light-plus self])
	[update-tile self])
      [die self]))

;;; Karma

(defparameter *karma-tiles* '("rezblur5"
			       "rezblur4"
			       "rezblur3"
			       "rezblur2"
			       "rezblur1"))

(defparameter *karma-samples* '("zap1" "zap2" "zap3"))

(defparameter *karma-alt-samples* '("zap4" "zap5" "zap6" "zap7"))

(defparameter *karma-sample-schemes* (list *karma-samples* *karma-alt-samples*))

(defcell karma
  (tile :initform "rezblur1")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (clock :initform 20)
  (samples :initform *karma-samples*)
  (categories :initform '(:actor :paint-source :karma)))

(define-method initialize karma (&key clock)
  (setf <clock> clock))

(define-method set-clock karma (clock)
  (setf <clock> clock))

(define-method run karma ()
  (when <clock> (decf <clock>))
  (if (and <clock> (> 0 <clock>))
      [die self]
      (let ((dir (random-direction)))
	(setf <tile> (car (one-of *karma-tiles*)))
	(percent-of-time 1
	  [play-sample self (car (one-of <samples>))]
	  [drop self (clone =dancefloor=)])
	(do-cells (cell [cells-at *world* <row> <column>])
	  (when (has-field :hit-points cell)
	    [damage cell 1]))
	[move self dir])))

;;; Menu level

(define-prototype menu-world (:parent xe2:=world=)
  (height :initform *xong-level-height*)
  (width :initform *xong-level-width*)
  (edge-condition :initform :block)
  (ambient-light :initform :total))

(define-method generate menu-world (&rest args)
  [create-default-grid self]
  (setf <level> 0)
  (clon:with-fields (height width grid player) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))
    ;; (dotimes (i 10)
    ;;   [drop-cell self (clone =karma= :clock nil) (random height) (random width)])
    (let ((beckoner (clone =beckoner=))
	  (urhere (clone =balloon= 
			 :style :flat
			 :timeout 3.0
			 :text '((("<----- YOU ARE HERE. Use the arrow keys to move."))))))
      [drop-cell self urhere 0 2 :loadout t]
      [drop-cell self beckoner 8 14]
      [drop-cell self (clone =portal= 
			     :address (generate-level-address 1)
			     :text '((("To Level 1"))))
		 3 12 :loadout t]
      [drop-cell self (clone =portal=
			     :address '(=puckman-world=)
			     :text '((("To the Tutorial"))))
		 10 5 :loadout t])))

(define-method begin-ambient-loop menu-world ()
  (play-music "flyby" :loop t))

