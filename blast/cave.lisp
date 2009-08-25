(in-package :blast)

;;; The ground

(defcell cave-ground
  (tile :initform "cave-floor"))

(defcell cave-ground2
  (tile :initform "cave-floor2"))

;;; The slime the leeches leave behind

(defcell leech-slime
  (name :initform "Caustic leech slime")
  (clock :initform 15)
  (tile :initform "leech-slime")
  (categories :initform '(:target :actor :item :opaque))
  (hit-points :initform (make-stat :base 4 :min 0)))

(define-method step leech-slime (stepper)
  (when [is-player stepper]
    [say self "You lose 2 speed points by becoming coated in leech slime!"]
    [stat-effect stepper :speed -2]
    [damage stepper 1]))
;;    [add-category stepper :toxic]))

(define-method run leech-slime ()
  (decf <clock>)
  [expend-default-action-points self]
  (when (zerop <clock>)
    [die self]))

;;; The leeches

(defcell leech
  (tile :initform "leech")
  (name :initform "Lithoform Leech")
  (categories :initform '(:actor :enemy :target :opaque :obstacle))
  (hit-points :initform (make-stat :base 12 :max 30 :min 0))
  (speed :initform (make-stat :base 7))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 12))
  (direction :initform (random-direction)))
 
(define-method run leech ()
  (if (< [distance-to-player self] 10)	
      (progn [drop self (clone =leech-slime=)]
	     (progn [move self [direction-to-player self]]
		    (when [adjacent-to-player self]
		      [play-sample self "upwoop"]
		      [>>damage :player 1]
		      [expend-default-action-points self]
		      [drop self (clone =leech-slime=)]
		      [move self (random-direction)])))))
   ;; [drop self (clone =leech-slime=)]
  ;; [move self (random-direction)]

(define-prototype cavern (:parent rlx:=world=)
  (ambient-light :initform :total)
  (height :initform 50)
  (width :initform 50)
  (scale :initform '(5 m))
  (edge-condition :initform :block))

(define-method drop-water cavern ()
  (clon:with-field-values (height width) self
    (let ((plasma (rlx:render-plasma height width :graininess 0.8)))
      (dotimes (i height)
	(dotimes (j width)
	  (if (< 0.3 (aref plasma i j))
	      [drop-cell self (clone =ocean-dark=) i j]))))))

(define-method generate cavern (&key sequence-number)
  [create-default-grid self]
  ;; drop ground
  (clon:with-field-values (height width) self
    (let ((plasma (rlx:render-plasma height width :graininess 1.2)))
      (dotimes (i height)
	(dotimes (j width)
	  [drop-cell self (clone (if (< 0 (aref plasma i j))
				     =cave-ground= =cave-ground2=))
		     i j])))
    ;; drop water
    [drop-water self]
    ;; drop leeches
    (dotimes (i (+ 10 (random 20)))
      [drop-cell self (clone =leech=) (random height) (random width)])))
  

