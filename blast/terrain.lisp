(in-package :blast)

;;; Empty space.

(defcell space 
  (tile :initform "space"))

;;; Colored space.

(defcell space2
  (tile :initform "space2"))

;;; Radioactive gas

(defcell gas
  (tile :initform "rad")
  (clock :initform 8)
  (categories :initform '(:actor)))

(define-method step gas (stepper)
  (when [is-player stepper]
    [damage stepper 5]
    [>>say :narrator "RADIOACTIVE HAZARD!"]))

(define-method run gas ()
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      [move self (random-direction)]))

;;; A destructible wall.

(defcell wall
  (tile :initform "wall")
  (categories :initform '(:obstacle))
  (hit-points :initform (make-stat :base 20 :min 0)))

;;; Energy gas

(defcell energy-gas
  (tile :initform "energy-gas"))

(define-method step energy-gas (stepper)
  (when [is-player stepper]
    (when (has-field :energy stepper)
      [play-sample self "whoop"]
      [>>say :narrator "You absorb 5 energy points from the gas."]
      [>>stat-effect stepper :energy 5])))

;;; Magnetic space debris will slow movement

(defcell debris 
;  (categories :initform '(:obstacle))
  (tile :initform "debris"))

(define-method step debris (stepper)
  (when [is-player stepper]	
    [>>say :narrator "Your movement is slowed by the space debris."]
    [expend-action-points stepper 5]))

(define-method damage debris (points)
  (declare (ignore points))
  [die self])

;;; The inescapable game grid.

(define-prototype void-world (:parent rlx:=world=)
  (ambient-light :initform :total))

(define-method generate void-world (&key   
				    (width 20)
				    (height 20)
				    (asteroid-count 20)
				    (biclops-count 0)
				    (berserker-count 0)
				    (polaris-count 5)
				    (probe-count 5)
				    (energy-gas-cluster-count 2)
				    (box-cluster-count 4)
				    (gas-cluster-count 0)
				    (room-size 3)
				    (room-count 4)
				    (scanner-count 0)
				    (rook-count 0)
				    (energy-count 3))
  (setf <height> height <width> width)
  [create-default-grid self]
  [drop-plasma-space self]
  [drop-plasma-debris self]
  (dotimes (i polaris-count)
    [drop-cell self (clone =polaris=)
	       (random height) (random width)])
  ;; drop enemies
  (dotimes (i scanner-count)
    [drop-cell self (clone =scanner=)
	       (random height) (random width) :loadout t])
  (dotimes (i rook-count)
    [drop-cell self (clone =rook=)
	       (random height) (random width) :loadout t])
  (dotimes (i berserker-count)
    [drop-cell self (clone =berserker=)
	       (random height) (random width) :loadout t])
  (dotimes (i biclops-count)
    [drop-cell self (clone =biclops=)
	       (random height) (random width) :loadout t])
  (dotimes (i probe-count)
    [drop-cell self (clone =probe=)
	       (random height) (random width)])
  ;; drop stuff
  (dotimes (i energy-count)
    [drop-cell self (clone =energy=)
	       (random height) (random width)])
  (dotimes (i room-count)
      [drop-room self 
		 (random height)
		 (random width)
		 (+ room-size (random 3))
		 (+ room-size (random 4))])
  (dotimes (i box-cluster-count)
    (let ((r (random height))
	  (c (random width)))
      [drop-box-cluster self r c]))
  ;; (dotimes (i gas-cluster-count)
  ;;   (let ((r (random height))
  ;; 	  (c (random width)))
  ;;     [drop-gas-cluster self r c]))
  (dotimes (i energy-gas-cluster-count)
    (let ((r (random height))
	  (c (random width)))
      [drop-energy-gas-cluster self r c]))
  ;; and finally the 'roids
  [drop-random-asteroids self asteroid-count])

   
(define-method drop-random-asteroids void-world (count)
  (clon:with-field-values (height width) self
    (dotimes (i count)
      [drop-cell self (clone =asteroid= :speed (+ 3 (random 7))
			     :direction (rlx:random-direction)
			     :color (nth (random 4)
					 '(:red :blue :brown :orange)))
		 (random height) (random width)])))


(define-method drop-plasma-debris void-world ()
  (clon:with-field-values (height width) self
    (let ((plasma (rlx:render-plasma height width :graininess 0.4))
	  (value nil))
      (dotimes (i (- height 10))
	(dotimes (j (- width 10))
	  (setf value (aref plasma i j))
	  (when (< 0 value)
	    (let ((object =debris=))
	      [drop-cell self (clone object) (+ 10 i) (+ 10 j) :no-collisions t])))))))

(define-method drop-plasma-space void-world ()
  (clon:with-field-values (height width) self
    (let ((plasma (rlx:render-plasma height width :graininess 0.1))
	  (value nil))
      (dotimes (i height)
	(dotimes (j width)
	  (setf value (aref plasma i j))
	  [drop-cell self (clone (if (minusp value)
				     =space= =space2=))
		     i j])))))

(define-method drop-room void-world (row column height width)
  (let (rectangle)
    (labels ((collect-point (&rest args)
	       (prog1 nil (push args rectangle))))
      (trace-rectangle #'collect-point row column height width)
      ;; make sure there are openings
      (dotimes (i 3)
	(let ((n (random (length rectangle))))
	  (delete (nth n rectangle) rectangle)))
      (dolist (point rectangle)
	(destructuring-bind (r c) point
	  [drop-cell self (clone =wall=) r c :no-collisions t])))
    (when (> 4 (random 10))
      (dotimes (i (+ 2 (random 10)))
	[drop-cell self (clone =crystal=) 
		   (+ 1 row (random 4))
		   (+ 1 column (random 4))
		   :no-collisions t]))))

(define-method drop-box-cluster void-world (row column &key
						(height (+ 3 (random 5)))
						(width (+ 3 (random 5))))
  (labels ((drop-box (r c)
	     (prog1 nil
	       [drop-cell self (clone =blast-box=) r c])))
    (trace-rectangle #'drop-box row column height width)))

(define-method drop-energy-gas-cluster void-world (row column &key
						       (height (+ 3 (random 5)))
						       (width (+ 3 (random 5))))
  (labels ((drop-gas (r c)
	     (prog1 nil
	       [drop-cell self (clone =energy-gas=) r c])))
    (trace-rectangle #'drop-gas row column (+ 1 height) (+ 1 width) :fill)))

;;; Different challenge levels.

(defvar *void-levels* '((:width 20
			 :height 20
			 :asteroid-count 12
			 :polaris-count 2
			 :probe-count 2
			 :box-cluster-count 2
			 :room-size 4
			 :room-count 3
			 :scanner-count 0
			 :energy-count 3)
			(:width 50
			 :height 24
			 :asteroid-count 40
			 :polaris-count 12
			 :probe-count 15
			 :box-cluster-count 5
			 :room-count 14
			 :room-size 5
			 :scanner-count 3
			 :energy-count 7)
			(:width 50
			  :height 200
			  :asteroid-count 200
			  :biclops-count 10
			  :berserker-count 10
			  :polaris-count 70
			  :probe-count 50
			  :energy-gas-cluster-count 8
			  :room-size 8
			  :box-cluster-count 40
			  :room-count 65
			 :rook-count 12
			  :scanner-count 25
			  :energy-count 40)))

(defvar *level* 0)

