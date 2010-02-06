(in-package :xong)

;;; Counting enemies

(defvar *enemies* 0)

;;; Yasichi

(defparameter *yasichi-bounce-time* 8)

(defsprite yasichi
  (image :initform "yasichi2")
  (bounce-clock :initform 0)
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform (random-direction))
  (categories :initform '(:actor :obstacle)))

(define-method loadout yasichi ()
  [update-position self (+ 200 (random 100)) (+ 200 (random 100))])

(define-method run yasichi ()
  (unless (zerop <bounce-clock>)
    (decf <bounce-clock>))
  [expend-action-points self 10]
  (multiple-value-bind (y x) (xe2:step-in-direction <y> <x> <direction>)
    [update-position self x y]))

(define-method do-collision yasichi (object)
  (labels ((do-box (image)
	     (prog1 t
	       (when (clon:object-p object)
		 (multiple-value-bind (x y) 
		     [viewport-coordinates self]
		   (multiple-value-bind (x0 y0)
		       [viewport-coordinates object]
		     (draw-box x0 y0 16 16 :color ".cyan" :destination image)))))))
    [>>add-overlay :viewport #'do-box])
  (if (zerop <bounce-clock>)
      (progn 
	(setf <direction> (opposite-direction <direction>))
	(setf <bounce-clock> *yasichi-bounce-time*))
      ;; another collision too soon
      (setf <direction> (random-direction))))

(define-method light-square yasichi (r c)
  (labels ((do-square (image)
	     (prog1 t
	       (multiple-value-bind (x y) 
		   [get-viewport-coordinates (field-value :viewport *world*)
					      r
					      c]
		 (draw-rectangle x y 16 16 :destination image :color ".magenta")))))
    [>>add-overlay :viewport #'do-square]))

;;; Tracers lay down deadly red wires

(defcell wire 
  (categories :initform '(:actor :damaging))
  (stepping :initform t)
  (speed :initform (make-stat :base 1))
  (clock :initform 20)
  (description :initform "Deadly wires are an instant kill for player and puck."))
  
(define-method initialize wire (&key direction clock)
  (setf <clock> clock)
  (setf <tile> (ecase direction
		 (:north "wire-north")
		 (:south "wire-south")
		 (:east "wire-east")
		 (:west "wire-west"))))

(define-method run wire ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (< <clock> 0) (setf <clock> 0))
  (when (zerop <clock>)
    [die self]))

(define-method step wire (stepper)
  (if [is-player stepper]
      [damage stepper 1]
      (when [in-category stepper :puck]
	(unless (or [in-category stepper :enemy]
		    [in-category stepper :snowflake])
	  [die stepper]))))

(defvar *tracer-tiles* '(:north "tracer-north"
			  :south "tracer-south"
			  :east "tracer-east"
			  :west "tracer-west"))

(defcell tracer 
  (tile :initform "tracer-north")
  (categories :initform '(:actor :target :obstacle 
			  :opaque :exclusive :enemy :equipper :puck :tailed :tracer))
  (dead :initform nil)
  (speed :initform (make-stat :base 7 :min 5))
  (max-items :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 1))
  (stepping :initform t)
  (tail-length :initform (make-stat :base 20))
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (direction :initform (car (one-of '(:north :south :east :west))))
  (strength :initform (make-stat :base 4 :min 0 :max 30))
  (dexterity :initform (make-stat :base 5 :min 0 :max 30))
  (intelligence :initform (make-stat :base 11 :min 0 :max 30))
  (hit-points :initform (make-stat :base 10 :min 0 :max 10))
  (description :initform 
"The Tracer drags a live wire behind it. Don't touch! 
Use chevrons to direct tracers into Black Holes."))

(define-method update-tile tracer ()
  (setf <tile> (getf *tracer-tiles* <direction>)))

(define-method kick tracer (direction)
  (setf <direction> direction))
    
(define-method run tracer ()
  (clon:with-field-values (row column) self
    (let ((world *world*))
      (when [obstacle-in-direction-p world row column <direction>]
	(setf <direction> (car (one-of '(:north :south :east :west)))))
      [expend-action-points self 25]
      [move self <direction>])))

(define-method drop-wire tracer ()
  [drop-cell *world* (clone =wire= :direction <direction> :clock 5)
	     <row> <column>])

(define-method move tracer (direction)
  [drop-wire self]
  [update-tile self]
  [parent>>move self direction])

(define-method loadout tracer ()
  (incf *enemies*))

*(define-method cancel tracer ()
  (decf *enemies*))

(define-method die tracer ()
  (unless <dead>
    (setf <dead> t)
    (decf *enemies*)
    (score 2000)
    [delete-from-world self]))

;;; The deadly security monitor has a deadly ring field attack

(defcell monitor
  (tile :initform "monitor")
  (name :initform "Monitor")
  (categories :initform '(:obstacle :actor :equipper :opaque 
			  :exclusive :enemy :target :puck :monitor))
  (direction :initform nil)
  (speed :initform (make-stat :base 2))
  (movement-cost :initform (make-stat :base 6))
  (hit-points :initform (make-stat :base 20 :min 0))
  (equipment-slots :initform '(:robotic-arm))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (dead :initform nil)
  (attacking-with :initform :robotic-arm)
  (energy :initform (make-stat :base 800 :min 0 :max 1000))
  (firing-with :initform :robotic-arm)
  (strength :initform (make-stat :base 24))
  (dexterity :initform (make-stat :base 12))
  (description :initform 
"These security drones scan areas methodically until an intruder is
detected; if you get close enough, an alarm sounds, and an electric
shock field is projected in pulses. If you become trapped, try
squeezing by in between pulses!"))
    
(define-method choose-new-direction monitor ()
  [expend-action-points self 2]
  (setf <direction>
	(if (= 0 (random 20))
	    ;; occasionally choose a random dir
	    (nth (random 3)
		 '(:north :south :east :west))
	    ;; otherwise turn left
	    (getf '(:north :west :west :south :south :east :east :north)
		  (or <direction> :north)))))
  
(define-method loadout monitor ()
  (incf *enemies*)
  [choose-new-direction self])
  
(define-method cancel monitor ()
  (decf *enemies*))

;; (define-method initialize monitor ()
;;   [make-inventory self]
;;   [make-equipment self])

(define-method kick monitor (direction)
  (setf <direction> direction))

(define-method alarm monitor ()
  [play-sample self "activate"]
  [expend-action-points self 10]
  (labels ((do-circle (image)
	     (prog1 t
	       (multiple-value-bind (x y) 
		   [viewport-coordinates self]
		 (let ((x0 (+ x 8))
		       (y0 (+ y 8)))
		   (draw-circle x0 y0 40 :destination image)
		   (draw-circle x0 y0 35 :destination image))))))
    [>>add-overlay :viewport #'do-circle])
  (when (< [distance-to-player self] 3.5)
    [damage [get-player *world*] 1]))

(define-method run monitor ()
  (clon:with-field-values (row column) self
    (let ((world *world*))
      (if (and (< [distance-to-player world row column] 7)
	       [line-of-sight world row column 
			      [player-row world]
			      [player-column world]])
	  (let ((player-dir [direction-to-player world row column]))
	    [alarm self]
;;	    (setf <direction> player-dir)
	    [move self player-dir]
	    [expend-action-points self 10])
	  (multiple-value-bind (r c)
	      (step-in-direction <row> <column> <direction>)
	    (when [obstacle-at-p world r c]
	      [choose-new-direction self])
	    [move self <direction>])))))
  
(define-method die monitor ()
  (unless <dead>
    (setf <dead> t)
    (decf *enemies*)
    [play-sample self "death-alien"]
    (score 5000)
    [delete-from-world self]))

;;; Replacement puck

(defcell puckup 
  (tile :initform "puckup")
  (name :initform "Replacement puck")
  (categories :initform '(:exclusive))
  (description :initform "A new puck, in case you lose the one you have."))

(define-method step puckup (stepper)
  (when [is-player stepper]
    (when (field-value :puck stepper)
      [>>narrateln :narrator "You wasted your puck."]
      [play-sample self "buzz"])
    (let ((puck (clone =puck=)))
      [drop self puck]
      (score 1000)
      [grab stepper puck]
      [die self])))

;;; Black hole eats anything in category :puck (and the player)

(defcell hole 
  (tile :initform "hole")
  (nospew :initform nil)
  (open :initform t)
  (categories :initform '(:exclusive :hole))
  (name :initform "Black hole")
  (description :initform 
"These holes eat the puck and enemies. The object of the game is to
defeat enemies by guiding them into the black holes. Be careful; black
holes can only eat one object before closing. Not only that, they
explode with deadly plasma radiation!"))

(define-method spew-karma hole ()
  (clon:with-field-values (row column) self
    (let ((color (car (one-of *colors*))))
      (assert (and row column))
      (dotimes (n (+ 9 (random 10)))
	(let ((karma (clone =karma=)))
;;	  [set-color karma color]
	  [set-clock karma (+ 10 (random 10))]
	  (let ((limit 10))
	    (block placing
	      (loop do (let ((r (+ row (- (random 3) (random 5))))
			     (c (+ column (- (random 3) (random 5)))))
			 (if [line-of-sight *world* row column r c]
			     (progn 
			       [drop-cell *world* karma r c]
			       (return-from placing))
			     ;; try again
			     (decf limit)))
		    while (plusp limit)))))))))

(define-method step hole (stepper)
  (when <open>
    (assert (and <row> <column>))
    (unless <nospew>
      [spew-karma self])
    (progn [play-sample self "hole-suck"]
	   (if [in-category stepper :puck]
	       [die stepper]
	       (when [is-player stepper]
		 [damage stepper 1]))
	   (setf <open> nil)
	   (setf <tile> "hole-closed"))))

(define-method initialize hole (&key nospew)
  (setf <nospew> nospew))

;;; Snakes are the body components of a snake

(defvar *snake* nil)

(defun snake-living-p ()
  (labels ((smashed (c)
	     (field-value :smashed c)))
    (notevery #'smashed *snake*)))

(defvar *snake-tiles* '(:purple "brick-purple"
			:black "brick-black"
			:red "brick-red"
			:blue "brick-blue"
			:orange "brick-orange"
			:green "brick-green"
			:white "brick-white"
			:yellow "brick-yellow"))

(defparameter *snake-escape-time* 100)

(defcell snake 
  (tile :initform "snake-white")
  (smashed :initform nil)
  (speed :initform (make-stat :base 20))
  (movement-cost :initform (make-stat :base 60))
  (escape-clock :initform 0)
  (ahead :initform nil)
  (behind :initform nil)
  (color :initform :white)
  (direction :initform :south)
  (categories :initform '(:obstacle :exclusive :paintable :actor :snake))
  (description :initform "The deadly Snake's body segments must be painted to defeat it."))

(define-method set-color snake (c)
  (setf <color> c)
  (let ((res (getf *snake-tiles* c)))
    (assert (stringp res))
    (setf <tile> res)))

(define-method paint snake (color)
  (if (and (eq <color> color) (not <smashed>))
      (progn [play-sample self "lock-opening-sound"]
	     (score 1000)
	     (setf <smashed> t)
	     (setf <tile> "brick-smashed"))
      [play-sample self "error"]))

(define-method attach snake (piece)
  (setf <behind> piece)
  (setf (field-value :ahead piece) self))

(define-method adjacent-gate snake ()
  (clon:with-field-values (row column) self
    (block searching
      (dolist (dir '(:north :south :east :west))
	(multiple-value-bind (r c) (step-in-direction row column dir)
	  (let ((gate [category-at-p *world* r c :gate]))
	    (when (and (clon:object-p gate)
		       [is-open gate]
		       (zerop <escape-clock>))
	      (setf <escape-clock> *snake-escape-time*)
	      (return dir))))))))	

(define-method probe snake (direction)
  (let ((retval (clon:with-field-values (row column) self
		  (multiple-value-bind (r c) (step-in-direction row column direction)
		    (if (and [in-bounds-p *world* r c]
			     (not [category-at-p *world* r c :obstacle]))
			;; all clear
			direction
			;; allow overlapping self
			(when (or [category-at-p *world* r c :snake]
				  ;; try to escape the room
				  (and (let ((gate [category-at-p *world* r c :gate]))
					 (when (clon:object-p gate)
					   [is-open gate]))))
			  direction))))))
    (when retval 
      (prog1 retval [expend-action-points self 20]))))
	      
(define-method run snake ()
  (setf <escape-clock> (max 0 (1- <escape-clock>)))
  (clon:with-field-values (row column) self
    (when (null <ahead>)
      ;; kill player if adjacent
      (when [adjacent-to-player self]
	[damage [get-player *world*] 1])
      ;; we are the head of the snake
      (let ((dir (or [adjacent-gate self] <direction>)))
	(if [probe self dir]
	    (progn [move self dir :ignoring-obstacles]
		   (let ((piece <behind>)
			 (r row)
			 (c column)
			 next-r next-c)
		     (loop while piece
			   do [>>move piece (direction-to (setf next-r (field-value :row piece))
							  (setf next-c (field-value :column piece))
						      r c) :ignoring-obstacles]
			      (setf r next-r
				    c next-c
				    piece (field-value :behind piece)))))
	    (let ((dir (car (one-of '(:north :south :east :west)))))
	      (setf <direction> (or [probe self dir] <direction>))))))))

;; (define-method spawn snake ()
;;   (let ((dir (car (one-of '(:north :south :east :west))))
;; 	(snake (clone =snake=)))
;;     (multiple-value-bind (r c)
;; 	(step-in-direction <row> <column> dir)
;;       [drop-cell *world* snake r c :probe t :exclusive t])))

;;; Muon bullets

(defvar *muon-tiles* '(:north "muon-north"
		       :south "muon-south"
		       :east "muon-east"
		       :west "muon-west"
		       :northeast "muon-northeast"
		       :southeast "muon-southeast"
		       :southwest "muon-southwest"
		       :northwest "muon-northwest"))

(defvar *trail-middle-tiles* '(:north "bullet-trail-middle-north"
			       :south "bullet-trail-middle-south"
			       :east "bullet-trail-middle-east"
			       :west "bullet-trail-middle-west"
			       :northeast "bullet-trail-middle-northeast"
			       :southeast "bullet-trail-middle-southeast"
			       :southwest "bullet-trail-middle-southwest"
			       :northwest "bullet-trail-middle-northwest"))

(defvar *trail-end-tiles* '(:north "bullet-trail-end-north"
			       :south "bullet-trail-end-south"
			       :east "bullet-trail-end-east"
			       :west "bullet-trail-end-west"
			       :northeast "bullet-trail-end-northeast"
			       :southeast "bullet-trail-end-southeast"
			       :southwest "bullet-trail-end-southwest"
			       :northwest "bullet-trail-end-northwest"))

(defvar *trail-tile-map* (list *trail-end-tiles* *trail-middle-tiles* *trail-middle-tiles*))

(defcell muon-trail
  (categories :initform '(:actor))
  (clock :initform 2)
  (speed :initform (make-stat :base 10))
  (default-cost :initform (make-stat :base 10))
  (tile :initform ".gear")
  (direction :initform :north))

(define-method orient muon-trail (direction)
  (setf <direction> direction)
  (setf <tile> (getf *trail-middle-tiles* direction)))

(define-method run muon-trail ()
  (setf <tile> (getf (nth <clock> *trail-tile-map*)
		     <direction>))
  [expend-default-action-points self]
  (decf <clock>)
  (when (minusp <clock>)
    [die self]))

;;; Basic muon particle

(defcell muon-particle 
  (categories :initform '(:actor :muon :target))
  (speed :initform (make-stat :base 20))
  (default-cost :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 20))
  (attack-power :initform 5)
  (tile :initform "muon")
  (name :initform "Muon particle")
  (firing-sound :initform "muon-fire")
  (direction :initform :here)
  (clock :initform 12)
  (description :initform
"This high-energy particle will kill you instantly."))

(define-method initialize muon-particle (&key attack-power)
  (when attack-power
    (setf <attack-power> attack-power)))

(define-method drop-trail muon-particle (direction)
  (let ((trail (clone =muon-trail=)))
    [orient trail direction]
    [drop self trail]))

(define-method find-target muon-particle ()
  (let ((target [category-in-direction-p *world* 
					 <row> <column> <direction>
					 '(:obstacle :target)]))
    (if target
	(progn
	  [>>move self <direction>]
	  [>>expend-default-action-points self]
	  [>>push target <direction>]
	  [>>damage target <attack-power>]
	  [>>die self])
	(multiple-value-bind (r c) 
	    (step-in-direction <row> <column> <direction>)
	  (if (not (array-in-bounds-p (field-value :grid *world*) r c))
	      [die self]
	      (progn [drop-trail self <direction>]
		     [>>move self <direction>]))))))

(define-method step muon-particle (stepper)
  [damage stepper <attack-power>]
  [die self])
  
(define-method update-tile muon-particle ()
  (setf <tile> (getf *muon-tiles* <direction>)))

(define-method run muon-particle ()
  [update-tile self]
  [find-target self]
  (decf <clock>)
  (when (zerop <clock>)
    [>>die self]))

(define-method impel muon-particle (direction)
  (assert (member direction *compass-directions*))
  (setf <direction> direction)
  ;; don't hit the player
  ;;  [move self direction]
  [play-sample self <firing-sound>]
  [find-target self])

;;; The Oscillator

(defcell oscillator 
  (tile :initform "oscillator")
  (categories :initform '(:actor :obstacle :target :enemy :opaque :oscillator :puck))
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 20))
  (default-cost :initform (make-stat :base 20))
  (direction :initform (car (one-of '(:south :west))))
  (stepping :initform t)
  (dead :initform nil)
  (name :initform "Oscillator")
  (description :initform 
"These bounce back and forth very quickly, firing muon particles if
the player gets too close."))

(define-method get-nasty oscillator ()
  [damage [get-player *world*] 1])

(define-method loadout oscillator ()
  (incf *enemies*))
  
(define-method cancel oscillator ()
  (decf *enemies*))

(define-method run oscillator ()
  (if [obstacle-in-direction-p *world* <row> <column> <direction>]
      (setf <direction> (opposite-direction <direction>))
      (progn [move self <direction>]
	     (if (and (> 8 [distance-to-player self])
		      [line-of-sight *world* <row> <column> 
				     [player-row *world*]
				     [player-column *world*]])
		 [fire self [direction-to-player self]]))))

(define-method fire oscillator (direction)
  (let ((muon (clone =muon-particle=)))
    [drop self muon]
    [expend-action-points self 100]
    [impel muon direction] ))

(define-method damage oscillator (points)
  [get-nasty self])

(define-method die oscillator ()
  (unless <dead>
    (decf *enemies*)
    (score 5000)
    [play-sample self "death-alien"]
    [parent>>die self]))
    
(define-method kick oscillator (direction)
  (setf <direction> direction)
  [move self direction])

