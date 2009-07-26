(in-package :blast)

;;; Various terrain tiles. 

(defcell mars-flat
  (tile :initform "mars-terrain-flat"))

(defcell mars-icy
  (tile :initform "mars-terrain-icy"))

(defcell mars-icy2
  (tile :initform "mars-terrain-icy2"))

(defcell mars-tundra 
  (tile :initform "mars-tundra"))

(defcell mars-dark 
  (tile :initform "mars-terrain-dark"))

(defcell mars-cracks 
  (tile :initform "mars-terrain-cracks"))

;;; Terrain generation.

(define-prototype mars (:parent rlx:=world=)
  (height :initform 20)
  (width :initform 45)
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
	  [drop-cell self (clone =mars-tundra=) n j]
	  [drop-cell self (clone =mars-tundra=) (- height n 1) j])))))
	

(define-method generate mars (&rest parameters)
  [create-default-grid self]
  [draw-terrain self])

(define-method start mars ()
  (play-music "black-thunder" :loop t)
  [parent>>start self])
