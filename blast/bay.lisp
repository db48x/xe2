(in-package :blast)

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
  (hit-points :initform (make-stat :base 30 :max 100 :min 0))
  (speed :initform (make-stat :base 2))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 20))
  (categories :initform '(:actor :obstacle :enemy :target :boss)))

(define-method loadout bay-factory ()
  (incf (field-value :factory-count *active-world*)))

(define-method die bay-factory ()
  (clon:with-fields (factory-count) *active-world*
    (decf factory-count)
    [>>say :narrator (format nil "Factory destroyed. ~d remaining." 
			    factory-count)])
  [parent>>die self])

(define-method run bay-factory ()
  [expend-action-points self 20]
  (when (< [distance-to-player self] 25)
    [play-sample self "spawn"]
    [drop-cell *active-world* (clone =laser-drone=) <row> <column> :loadout t]))

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
  (stepping :initform nil)
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
      (if (< dist 20)
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
  (labels ((drop-carrier (r c)
	     (prog1 nil
	       [drop-cell self (clone =carrier=) r c])))
    (trace-rectangle #'drop-carrier row column size size :fill))
  [drop-cell self (clone =bay-factory=) 
	     (+ row (random size))
	     (+ column (random size))
	     :loadout t])

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


\
