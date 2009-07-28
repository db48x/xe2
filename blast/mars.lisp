(in-package :blast)

;;; Various terrain tiles. 

(defcell mars-flat
  (tile :initform "mars-terrain-flat"))

(define-method step mars-flat (stepper)
  (when (has-field :endurium stepper)
    [>>stat-effect stepper :endurium -0.01]))

(define-prototype mars-icy (:parent =mars-flat=)
  (tile :initform "mars-terrain-icy"))

(define-prototype mars-icy2 (:parent =mars-flat=)
  (tile :initform "mars-terrain-icy2"))

(define-prototype mars-tundra (:parent =mars-flat=)
  (tile :initform "mars-tundra"))

(define-prototype mars-dark (:parent =mars-flat=)
  (tile :initform "mars-terrain-dark"))

(define-prototype mars-cracks (:parent =mars-flat=)
  (tile :initform "mars-terrain-cracks"))

;;; Terrain generation.

(define-prototype mars (:parent rlx:=world=)
  (height :initform 20)
  (width :initform 45)
  (technetium :initform 0)
  (scale :initform '(2000 km)))

(define-method draw-terrain mars ()
  (clon:with-field-values (height width) self
    (let ((dust-plasma (rlx:render-plasma height width :graininess 0.2)) 
	  (ice-plasma (rlx:render-plasma height width :graininess 0.7)) 
	  (value nil))
      (dotimes (i height)
	(dotimes (j width)
	  (setf value (aref dust-plasma i j))
	  [drop-cell self (clone (if (< 0 value)
				     (if (< 0.3 value) 
					 =mars-flat=
					 =mars-cracks=)
				     =mars-dark=))
		     i j :no-collisions t]
	  (setf value (aref ice-plasma i j))
	  (when (< 0 value)
	    [drop-cell self (clone =mars-icy2=) i j])))
      ;; draw polar ice caps
      (dotimes (j width)
	(dotimes (n (1+ (random 3)))
	  [drop-cell self (clone =mars-tundra=) n j])
	(dotimes (n (1+ (random 3)))
	  [drop-cell self (clone =mars-tundra=) (- height n 1) j]))
      ;; deposit minerals
      (dotimes (n 9)
	[drop-cell self (clone =technetium=) (random height) (random width)]))))
	
(define-method generate mars (&rest parameters)
  [create-default-grid self]
  [draw-terrain self])

(define-method start mars ()
  (play-music "black-thunder" :loop t)
  [parent>>start self])
