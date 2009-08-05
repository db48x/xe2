
(in-package :blast)

;;; The exploding mine

(defcell mine 
  (name :initform "Proximity mine")
  (categories :initform '(:item :target :actor :hidden))
  (tile :initform "mine"))

(defvar *mine-warning-sensitivity* 4)
(defvar *mine-explosion-sensitivity* 3)

(define-method run mine ()
  (let ((distance [distance-to-player *active-world* <row> <column>]))
    (if (< distance *mine-warning-sensitivity*)
	(progn
	  (when (string= <tile> "mine")
	    [>>say :narrator "You see a mine nearby!"])
	  (setf <tile> "mine-warn")
	  (when (< distance *mine-explosion-sensitivity*)
	    (when (< (random 8) 1)
	      [explode self])))
	(setf <tile> "mine"))))

(define-method explode mine ()
  (labels ((boom (r c &optional (probability 50))
	     (prog1 nil
	       (when (and (< (random 100) probability)
			  [in-bounds-p *active-world* r c])
		 [drop-cell *active-world* (clone =explosion=) r c :no-collisions nil]))))
    (dolist (dir rlx:*compass-directions*)
      (multiple-value-bind (r c)
	  (step-in-direction <row> <column> dir)
	(boom r c 100)))
    ;; randomly sprinkle some fire around edges
    (trace-rectangle #'boom 
		     (- <row> 2) 
		     (- <column> 2) 
		     5 5)
    [die self]))

(define-method step mine (stepper)
  (when [is-player stepper]	      
    [explode self]))

(define-method damage mine (damage-points)
  (declare (ignore damage-points))
  [explode self])

;;; The instantaneous-fire laser weapon

(defcell ray-caster
  (name :initform "Ray caster")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "ray-caster")
  (attack-power :initform (make-stat :base 5))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (weight :initform 3000)
  (equip-for :initform '(:robotic-arm :left-hand :right-bay :left-bay :right-hand)))

;;; Bases that manufacture said robots at high speed

(defcell bay-factory 
  (tile :initform "bay-factory")
  (name :initform "Drone Factory")
  (equipment-slots :initform '(:robotic-arm))
  (firing-with :initform :robotic-arm)
  (hit-points :initform (make-stat :base 30 :max 100 :min 0))
  (speed :initform (make-stat :base 2))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 20))
  (max-items :initform (make-stat :base 10))
  (categories :initform '(:actor :obstacle :enemy :target :boss)))

(define-method loadout bay-factory ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =missile-launcher=)]]
  (incf (field-value :factory-count *active-world*)))

(define-method die bay-factory ()
  (clon:with-fields (factory-count) *active-world*
    (decf factory-count)
    [drop self (clone =explosion=)]
    [>>say :narrator (format nil "Factory destroyed. ~d remaining." 
			    factory-count)])
  [parent>>die self])

(define-method run bay-factory ()
  [expend-action-points self 15]
  (when (< 5 [distance-to-player self] 20)
    [play-sample self "spawn"]
    [drop-cell *active-world* (clone =laser-drone=) <row> <column> :loadout t])
  (when (< [distance-to-player self] 20)
    [fire self [direction-to-player self]]))

;;; Carrier space

(defcell carrier 
  (tile :initform "carrier")
  (name :initform "Aircraft Carrier"))

;;; The laser drone robots that just keep coming. 

(defcell laser-drone 
  (tile :initform "solv") 
  (name :initform "Laser Drone")
  (hit-points :initform (make-stat :base 3 :max 3 :min 0))
  (speed :initform (make-stat :base 2))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (direction :initform (random-direction))
  (categories :initform '(:actor :obstacle :enemy :target)))

(define-method loadout laser-drone ()
  (incf (field-value :laser-drone-count *active-world*))
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =ray-caster=)]])

(define-method fire laser-drone ()
  [expend-action-points self 15]
  (let* ((world *active-world*)
	 (player [get-player *active-world*]))
    (labels ((draw-beam (image)
	       (multiple-value-bind (x0 y0) 
		   [screen-coordinates self]
		 (multiple-value-bind (x1 y1)
		     [screen-coordinates player]
		   (rlx:draw-line x0 y0 x1 y1 
				  :destination image)))))
      [damage player 1]
      [play-sample self "laser"]
      [>>add-overlay :viewport #'draw-beam])))
	     
(define-method run laser-drone ()
  (clon:with-field-values (row column) self
    (let ((dist [distance-to-player *active-world* row column])
	  (dir [direction-to-player *active-world* row column]))
      (if (< dist 15)
	  (if (< dist 6)
	      [fire self]
	      [move self dir])
	  [move self (random-direction)]))))

(define-method die laser-drone ()
  (clon:with-fields (laser-drone-count) *active-world*
    (decf laser-drone-count)
    (when (= 0 (random 2))
      ;; drop something
      (if (= 0 (random 4))
	  [drop self (clone =diamond=)]
	  [drop self (clone =energy=)]))
    [play-sample self "death-alien-short"]
    [>>say :narrator (format nil "Drone destroyed. ~d remaining." 
			    laser-drone-count)]
    [parent>>die self]))

;;; The ocean world Corva 3.

(defcell ocean
  (tile :initform "ocean")
  (name :initform "Ocean"))

(define-method step ocean (stepper)
  (when (clon:has-field :endurium stepper)
    [stat-effect stepper :endurium -0.002]))

(define-prototype ocean-dark (:parent =ocean=)
  (tile :initform "ocean2")
  (name :initform "Deep Ocean"))

(define-prototype bay (:parent rlx:=world=)
  (ambient-light :initform :total)
  (laser-drone-count :initform 0)
  (factory-count :initform 0)
  (height :initform 80)
  (width :initform 80)
  (scale :initform '(50 m))
  (edge-condition :initform :exit))

(define-method check-win-condition bay ()
  (when (= 0 <laser-drone-count> <factory-count>)
    (prog1 t (message "You Win!"))))

(define-method drop-ocean bay ()
  (clon:with-field-values (height width) self
    (let ((plasma (rlx:render-plasma height width :graininess 1.2)))
      (dotimes (i height)
	(dotimes (j width)
	  [drop-cell self (clone (if (< 0 (aref plasma i j))
				     =ocean= =ocean-dark=))
		     i j])))))

(define-method draw-carrier bay (row column size)
  (let ((mine-locations nil))
    (labels ((drop-carrier (r c)
	       (prog1 nil
		 [drop-cell self (clone =carrier=) r c]))
	     (collect-point (&rest loc)
	       (push loc mine-locations)))
      (trace-octagon #'collect-point 
		     (+ row (truncate (/ size 2)))
		     (+ column (truncate (/ size 2)))
		     (* 2 size))
      (trace-rectangle #'drop-carrier row column size size :fill)
      (dotimes (i 5)
	(let ((loc (nth (random (length mine-locations))
			mine-locations)))
	  [drop-cell self (clone =mine=) (first loc) (second loc)]))
      [drop-cell self (clone =bay-factory=) 
		 (+ row (random size))
		 (+ column (random size))
		 :loadout t])))

(define-method generate bay (&key sequence-number drones carriers)
  [create-default-grid self]
  [drop-ocean self]
  (dotimes (i drones)
    [drop-cell self (clone =laser-drone=) (random <height>) (random <width>) 
	       :loadout t])
  (dotimes (i carriers)
    [draw-carrier self (random <height>) (random <width>) (+ 3 (random 7))])
  [drop-entry-point self (random <height>) (random <width>)])
  
(define-method start bay ()
  (play-music "raid" :loop t)
  [parent>>start self])



