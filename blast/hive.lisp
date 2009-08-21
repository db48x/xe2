
(in-package :blast)

(defcell hive-floor 
  (name :initform "Hive floor")
  (tile :initform "sprout-ground"))

(defcell pollen 
  (name :initform "Type 1 biosilicate pollen")
  (tile :initform "pollen")
  (categories :initform '(:item :sprout-food)))

(define-prototype pollen2 (:parent =pollen=)
  (name :initform "Type 2 biosilicate pollen")
  (tile :initform "pollen2"))

(defparameter *sprout-tiles*
  '("sprout" "sprout2" "sprout3" "sprout4" "sprout5" "sprout6"))

(defcell sprout 
  (tile :initform "sprout")
  (generation :initform 0)
  (hit-points :initform (make-stat :base 3 :max 3 :min 0))
  (speed :initform (make-stat :base 3))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 10))
  (direction :initform (random-direction))
  (categories :initform '(:actor :obstacle :enemy :target)))

(define-method divide sprout ()
  [drop self (clone =sprout=)])

(define-method grow sprout ()
  (incf <generation>)
  (when (= 3 <generation>)
    [divide self])
  (when (> <generation> 5)
    [die self])
  (setf <tile> (nth <generation> *sprout-tiles*)))

(define-method find-food sprout (direction)
  (let ((food [category-in-direction-p *active-world* <row> <column> direction :sprout-food]))
    (when food
      (prog1 food
	[move self direction]
	[grow self]
	[die food]))))

(define-method run sprout ()
  [move self (random-direction)]
  (block searching
    (dolist (dir rlx:*compass-directions*)
      (when [find-food self dir]
	(return-from searching)))))

(define-prototype biome (:parent rlx:=world=)
  (name :initform "Biosilicate Hive Biome")
  (ambient-light :initform :total)
  (required-modes :initform '(:vehicle :spacesuit))
  (clusters :initform 30)
  ;;  (factory-count :initform 0) ;; some enemy
  (height :initform 80)
  (width :initform 80)
  (scale :initform '(1 m))
  (edge-condition :initform :block))

;; (define-method begin-ambient-loop biome

(define-method drop-cluster biome (row column)
  (labels ((drop-pollen (r c &optional (type 1))
	     [drop-cell self (ecase type
			       (1 (clone =pollen=))
			       (2 (clone =pollen2=)))
			r c])
	   (drop-pollen2 (r c)
	     (drop-pollen r c 2)))
    (trace-octagon #'drop-pollen row column 5)
    (trace-rectangle #'drop-pollen2 
		     (- row 2) (- column 2)
		     5 5 t)
    (dotimes (i 3)
      [drop-cell self (clone =sprout=)
		 (+ (- row 3) 
		    (random 5))
		 (+ (- column 3)
		    (random 5))])))

(define-method generate biome (&key (height 80)
				    (width 80)
				    (sequence-number (random 32768))
				    (clusters 10))
  [create-default-grid self]
  (clon:with-field-values (height width clusters) self
    ;; drop floor
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =hive-floor=) i j]))
    ;; drop plasma pollen
    (let ((plasma (rlx:render-plasma height width :graininess 0.3))
	  (value nil))
      (dotimes (i height)
	(dotimes (j width)
	  (setf value (aref plasma i j))
	  (when (< 0.7 value)
	    [drop-cell self 
		       =pollen2=
		       i j]))))
    ;; drop clusters
    (dotimes (c clusters)
      [drop-cluster self (random height) (random width)])
    ;; player 
    [drop-cell self (clone =launchpad=) (random 16) (random 16)]))

      

    



  
