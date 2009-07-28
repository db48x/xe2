(in-package :blast)

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
  [drop-entry-point self (random <height>) (random <width>)])

(define-method start bay ()
  (play-music "raid" :loop t)
  [parent>>start self])
