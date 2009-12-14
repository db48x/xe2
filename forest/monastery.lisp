(in-package :forest)

;;; Dandelion seeds

(defsprite dandelion
  (image :initform "dandelion")
  (speed :initform (make-stat :base 10))
  (categories :initform '(:actor))
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (movement-distance :initform 2)
  (clock :initform 10))

(define-method run dandelion ()
  (clon:with-fields (clock) self
    (if (plusp clock) 
	(progn 
	  (decf clock)
	  [move self (car (one-of '(:southeast :east :east))) 2])
	[die self])))

;;; Monastery approach world

(defcell hill-1
  (description :initform "This gentle slope heads down toward the Monastery.")
  (tile :initform "hill-1"))

(defcell hill-2
  (description :initform "This gentle slope heads down toward the Monastery.")
  (tile :initform "hill-2"))

(defcell hill-3 
  (description :initform "This gentle slope heads down toward the Monastery.")
  (tile :initform "hill-3"))

(defcell flowers-1
  (description :initform "Gorgeous wildflowers.")
  (tile :initform "flowers-1"))

(define-prototype flowers-2 (:parent =flowers-1=)
  (description :initform "Wildflowers of every description.")
  (tile :initform "flowers-2")
  (categories :initform '(:actor)))

(define-method run flowers-2 ()
  (percent-of-time 1
    (multiple-value-bind (x y) [xy-coordinates self]
      [drop-sprite self (clone =dandelion=) x y])))

(define-prototype monastery-gateway (:parent =gateway=)
  (tile :initform "monastery-gateway")
  (description :initform "The mountain pass opens here to the foothills by the Monastery.")
  (sequence-number :initform (genseq))
  (address :initform (generate-level-address 4)))

(define-method step monastery-gateway (stepper)
  [say self "The mountain pass opens to the foothills by the Monastery here."]
  [say self "Press ENTER to continue on."])

(define-prototype monastery (:parent xe2:=world=)
  (height :initform *forest-height*)
  (width :initform *forest-width*)
  (ambient-light :initform :total)
  (description :initform 
"Morning has broken, and Valisade Monastery has come into view to the
south. You can hear the monks singing in the distance.")
  (edge-condition :initform :block))

(define-method drop-hill monastery (&key (graininess 0.3)
				       (density 60)
				       distance
				       (cutoff 0.3))
  (clon:with-field-values (height width) self
    (let ((plasma (xe2:render-plasma height width :graininess graininess))
	  (plasma2 (xe2:render-plasma height width :graininess graininess))
	  (value nil))
      (dotimes (i height)
	(dotimes (j width)
	  [drop-cell self (clone =hill-3=) i j]))
      (dotimes (i height)
	(dotimes (j width)
	  (setf value (aref plasma i j))
	  (when (or (null distance)
		    (< (distance j i row column) distance))
	    (percent-of-time density
	      [drop-cell self (if (< cutoff value)
				  (clone =hill-1=)
				  (clone =hill-2=))
			 i j :no-collisions t]))))
      (dotimes (i height)
	(dotimes (j width)
	  (setf value (aref plasma i j))
	  (when (or (null distance)
		    (< (distance (+ j r0) (+ c0 i) row column) distance))
	    (percent-of-time density
	      [drop-cell self (if (< cutoff value)
				  (clone =flowers-1=)
				  (clone =flowers-2=))
			 i j :no-collisions t])))))))

(define-method generate monastery (&key (height *forest-height*)
				      (width *forest-width*)
				      sequence-number)
  (setf <height> height)
  (setf <width> width)
  (setf <sequence-number> sequence-number)
  [create-default-grid self]
  [drop-hill self]
  (let ((row (1+ (random 10)) )
	  (column (+ 15 (random 6))))
      [drop-cell self (clone =drop-point=) row column
		 :exclusive t :probe t]))

(define-method begin-ambient-loop monastery ()
  (play-music "rain" :loop t)
  (play-sample "monks"))

