(in-package :forest)

(defcell mountain 
  (tile :initform "mountain")
  (description :initform "The walls of the passageway are slick with ice.")
  (categories :initform '(:obstacle :opaque)))

;;; Mountain passage world

(defparameter *passage-width* 49)
(defparameter *passage-height* 100)

(define-prototype passage (:parent xe2:=world=)
  (height :initform *passage-height*)
  (width :initform *passage-width*)
  (ambient-light :initform *earth-light-radius*)
  (description :initform "The air is oddly still in this pass between the crags.")
  (edge-condition :initform :block))

(define-method drop-tundra passage ()
  (dotimes (i <height>)
    (dotimes (j <width>)
      [drop-cell self (clone =tundra=) i j])))

(define-method drop-mountains passage ()
  (let* ((offset 10)
	 (right (- <width> 17 )))
    (dotimes (i <height>)
      (setf offset (min right (max 0 (incf offset (if (= 0 (random 2))
						      1 -1)))))
      (labels ((drop-mountain (r c)
		 (prog1 nil
		   [drop-cell *world* (clone =mountain=) r c])))
	(trace-row #'drop-mountain i 0 (+ offset (random 4)))
	(percent-of-time 10 [drop-cell self (clone =wolf=) i (+ offset (random 4))])
	(trace-row #'drop-mountain i (+ offset (random 4) 20) <width>)))
    ;; drop monastery gateway
    (let ((column (+ offset (random 10)))
	  (row (- <height> 2)))
      [drop-cell self (clone =monastery-gateway=) row column])))

(define-method drop-trees passage (&optional &key (object =tree=)
					    distance 
					    (row 0) (column 0)
					    (graininess 0.3)
					    (density 100)
					    (cutoff 0))
  (clon:with-field-values (height width) self
    (let* ((h0 (or distance height))
	   (w0 (or distance width))
	   (r0 (- row (truncate (/ h0 2))))
	   (c0 (- column (truncate (/ w0 2))))
	   (plasma (xe2:render-plasma h0 w0 :graininess graininess))
	   (value nil))
      (dotimes (i h0)
	(dotimes (j w0)
	  (setf value (aref plasma i j))
	  (when (< cutoff value)
	    (when (or (null distance)
		      (< (distance (+ j r0) (+ c0 i) row column) distance))
	      (percent-of-time density
		[drop-cell self (clone object) i j :no-collisions t]))))))))

(define-method begin-ambient-loop passage ()
  (play-music "passageway" :loop t)
  (play-sample "howl")
  (play-sample "thunder-big"))

(define-method generate passage (&key (height *forest-height*)
				      (width *forest-width*)
				      sequence-number)
  (setf <height> height)
  (setf <width> width)
  (setf <sequence-number> sequence-number)
  [create-default-grid self]
  [drop-tundra self]
  [drop-mountains self]
    (let ((row (1+ (random 10)) )
	  (column (+ 15 (random 6))))
      [drop-cell self (clone =drop-point=) row column
		 :exclusive t :probe t]))


