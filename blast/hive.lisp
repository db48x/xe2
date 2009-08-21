
(in-package :blast)

(defcell hive-floor 
  (name :initform "Hive floor")
  (tile :initform "sprout-ground"))

(defcell pollen 
  (name :initform "Type 1 biosilicate pollen")
  (tile :initform "pollen")
  (categories :initform '(:item :sprout-food)))

(defcell pollen2 
  (name :initform "Type 2 biosilicate pollen")
  (categories :initform '(:item :sprout-food))
  (tile :initform "pollen2"))

(defcell pollen3a
  (name :initform "Type 3A biosilicate pollen")
  (categories :initform '(:item))
  (tile :initform "pollen3a"))

(define-method step pollen3a (stepper)
  (when (and [is-player stepper]
	     (has-field :pollen3a stepper))
    (let ((amount (random 0.1)))
      [stat-effect stepper :pollen3a amount]
      [say self "Obtained ~d kg Type 3A biosilicate pollen." amount]
      [play-sample self "biosilicate-sound"]
      [die self])))

(defparameter *sprout-tiles*
  '("sprout" "sprout2" "sprout3" "sprout4" "sprout5" "sprout6"))

(defcell sprout 
  (tile :initform "sprout")
  (generation :initform 0)
  (hit-points :initform (make-stat :base 4 :max 7 :min 0))
  (speed :initform (make-stat :base 3))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 10))
  (direction :initform (random-direction))
  (categories :initform '(:actor :obstacle :enemy :target)))

(define-method divide sprout ()
  [play-sample self "munch1"]
  [stat-effect self :hit-points 3]
  (dotimes (i (if (zerop (random 17))
		  2 1))
    [drop self (clone =sprout=)]))

(define-method die sprout ()
  [play-sample self "biodeath"]
  (when (= 0 (random 20))
    [drop self (clone =energy=)])
  [parent>>die self])

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
	[play-sample self (if (= 0 (random 1))
			      "slurp1" "slurp2")]
	[delete-from-world food]
	[move self direction]
	[grow self]))))

(define-method run sprout ()
  [move self (random-direction)]
  (if (< [distance-to-player self] 6)
      (progn [move self [direction-to-player self]]
	     (if [adjacent-to-player self]
		 [attack self [direction-to-player self]]))
      ;; otherwise look for food
      (block searching
	(dolist (dir rlx:*compass-directions*)
	  (when (or [in-category self :dead]
		    [find-food self dir])
	    (return-from searching))))))
  
(define-method attack sprout (direction)
  (let ((player [get-player *active-world*]))
    [play-sample self "munch2"]
    [damage player 4]
    [say self "The sprout hits you."]))

(define-prototype biome (:parent rlx:=world=)
  (name :initform "Biosilicate Hive Biome")
  (ambient-light :initform :total)
  (size :initform 10)
  (required-modes :initform '(:vehicle :spacesuit))
  (clusters :initform 10)
  ;;  (factory-count :initform 0) ;; some enemy
  (height :initform 20)
  (width :initform 20)
  (scale :initform '(1 m))
  (edge-condition :initform :block))

(define-method begin-ambient-loop biome ()
  (play-music "hive-music" :loop t)) 

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

(define-method generate biome (&key height width clusters
				    (sequence-number (random 32768))
				    (size 10)
				    (pollen3a 30))
  (setf <height> (or height (+ 20 (* size (random 8)))))
  (setf <width> (or width (+ 30 (* size (random 8)))))
  (setf <clusters> (or clusters (+ 5 (* size 2))))
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
    	  (when (< 0.9 value)
    	    [drop-cell self 
    		       =pollen2=
    		       i j]))))
    ;; drop precious pollen3a
    (dotimes (p pollen3a)
      [drop-cell self (clone =pollen3a=) 
		 (+ (truncate (/ height 2))
		    (random 12))
		 (+ (truncate (/ width 2))
		    (random 12))])
    (dotimes (r 2)
      [drop-cell self (clone =rook=) 
		 (truncate (/ height 2))
		 (truncate (/ width 2)) :loadout t])
    ;; drop clusters
    (dotimes (c clusters)
      [drop-cluster self (random height) (random width)])
    ;; player 
    [drop-cell self (clone =launchpad=) (random 16) (random 16)]))

      

    



  
