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

;;; The laser drone robots that just keep coming. 

(defcell laser-drone 
  (tile :initform "solv") 
  (hit-points :initform (make-stat :base 3 :max 3 :min 0))
  (speed :initform (make-stat :base 3))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (stepping :initform nil)
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (direction :initform (random-direction))
  (categories :initform '(:actor :obstacle :enemy :target)))

(define-method loadout laser-drone ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =ray-caster=)]])

(define-method run laser-drone ()
  (clon:with-field-values (row column) self
    (let ((dist [distance-to-player *active-world* row column])
	  (dir [direction-to-player *active-world* row column]))
      (when (< dist 20)
	[move self dir]))))

;;; The ocean world Corva 3.

(defcell ocean
  (tile :initform "ocean"))

(define-method step ocean (stepper)
  (when (clon:has-field :endurium stepper)
    [stat-effect stepper :endurium -0.002]))

(define-prototype ocean-dark (:parent =ocean=)
  (tile :initform "ocean2"))

(define-prototype bay (:parent rlx:=world=)
  (height :initform 128)
  (width :initform 128)
  (scale :initform '(50 m))
  (edge-condition :initform :exit))

(define-method drop-ocean bay ()
  (clon:with-field-values (height width) self
    (let ((plasma (rlx:render-plasma height width :graininess 1.2)))
      (dotimes (i height)
	(dotimes (j width)
	  [drop-cell self (clone (if (< 0 (aref plasma i j))
				     =ocean= =ocean-dark=))
		     i j])))))

(define-method generate bay (&rest parameters)
  [create-default-grid self]
  [drop-ocean self]
  (dotimes (i 180)
    [drop-cell self (clone =laser-drone=) (random <height>) (random <width>)])
  [drop-entry-point self (random <height>) (random <width>)])

(define-method start bay ()
  (play-music "raid" :loop t)
  [parent>>start self])
