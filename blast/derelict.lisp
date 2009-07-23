(in-package :blast)

;;; Infested oxygenless ships 

(defcell bulkhead
  (name :initform "Wall")
  (tile :initform "grayplate")
  (categories :initform '(:obstacle)))

(defcell corridor
  (name :initform "Airless Corridor")
  (tile :initform "grayplate-dark"))

;;; Moving in a corridor uses up oxygen.

(define-method step corridor (stepper)
  (when (has-field :oxygen stepper)
    [>>stat-effect stepper :oxygen -1]))

;;; Oxygen tank replenishes your air

(defcell oxygen-tank
  (tile :initform "oxygen-tank")
  (name :initform "Oxygen tank"))

(define-method step oxygen-tank (stepper)
  (when [is-player stepper]
    (rlx:play-sample "pop-ssh")
    [>>say :narrator "You recover 40 points from the oxygen tank."]
    [>>stat-effect stepper :oxygen 40]
    [>>die self]))

;;; Dead crewmember with random oxygen or possibly health.

(defcell crew-member 
  (tile :initform "crew")
  (categories :initform '(:item :target)))

(define-method step crew-member (stepper)
  (when [is-player stepper]
    [>>say :narrator "You search the dead crewmember's body."]
    [expend-default-action-points stepper]
    (let ((oxygen (+ 10 (random 60))))
      [>>say :narrator "You recover ~D units of oxygen from the crewmember's tank."
		     oxygen]
      [>>stat-effect stepper :oxygen oxygen])
    (when (> 3 (random 10))
      [>>say :narrator "You find a medical hypo, and recover 5 hit points."]
      [>>stat-effect stepper :hit-points 5])
    (when (> 3 (random 10))
      (let ((energy (+ 5 (random 15))))
	[>>say :narrator "You recover ~D units of energy from the crewmember's battery pack." 
	       energy]
	[>>stat-effect stepper :energy energy]))
    [>>die self]))
    
(define-method damage crew-member (points)
  (declare (ignore points))
  [>>say :narrator "The crewmember's body was destroyed!"]
  [>>die self])

;;; The storage crate.

(defcell tech-box
  (tile :initform "tech-box")
  (name :initform "Storage crate")
  (categories :initform '(:obstacle :opaque :pushable :destructible :target))
  (hit-points :initform (make-stat :base 10 :min 0)))

(defcell tech-box-debris
  (tile :initform "tech-box-debris")
  (name :initform "Crate debris"))

(define-method die tech-box ()
  [>>drop self (clone =tech-box-debris=)]
  [parent>>die self])

(defcell tech-box-special 
  (name :initform "Storage crate")
  (categories :initform '(:obstacle :opaque :pushable :destructible :target))
  (hit-points :initform (make-stat :base 10 :min 0))
  (tile :initform "tech-box-special"))

(define-method die tech-box-special ()
  [>>drop self (clone (ecase (random 5)
			(0 =level-up=)
			(1 =speed-up=)
			(2 =big-crystal=)
			(3 =ion-shield=)
			(4 =diamond=)))]
  [parent>>die self])

;;; The derelict freighter.

(define-prototype freighter (:parent =world=)
  (width :initform 48)
  (height :initform 300)
  (name :initform nil)
  (ambient-light :initform :total)
  (pallet-size :initform 13))

(define-method generate freighter (&key (sequence-number (random 32768)))
  [create-default-grid self]
  (setf <name> (concatenate 'string 
			    "Freighter ID#"
			    (make-string 1 :initial-element (elt "ABCDEF" (random 6)))
			    (make-string 1 :initial-element (elt "ABCDEF" (random 6)))
			    (make-string 1 :initial-element (elt "ABCDEF" (random 6)))
			    (format nil "~D" (+ 32768 (random 32768)))))
  (clon:with-field-values (height width pallet-size) self
    ;; create airless corridor space
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =corridor=) i j]))
    ;; create walls
    (labels ((drop-bulkhead (x y)
	       (prog1 nil
		 [drop-cell self (clone =bulkhead=) y x]))
	     (drop-box (x y)
	       (prog1 nil 
		 (let ((box (if (= 0 (random 100))
				(clone =tech-box-special=)
				(clone =tech-box=))))
		   [drop-cell self box y x :no-collisions t]))))
      ;; create border around world
      (trace-rectangle #'drop-bulkhead
		       0 0 width height)
      ;; drop wall blocks ("pallets")
      (let ((imax (1+ (truncate (/ width pallet-size))))
	    (jmax (1+ (truncate (/ height pallet-size)))))
	(dotimes (i imax)
	  (dotimes (j jmax)
	    ;; don't wall in the player
	    (when (not (= 0 i j))
	      (trace-rectangle #'drop-bulkhead
			       (+ (random 3)
				  (* pallet-size i))
			       (+ (random 4) 
				  (* pallet-size j))
			       (random pallet-size)
			       (random pallet-size)
			       :fill)))))
      ;; drop groups of boxes
      (dotimes (i 34)
    	(trace-rectangle #'drop-box (random height) (random width)
			 (+ 16 (random 14)) (+ 4 (random 3)) :fill)))
    ;; drop enemies
    (dotimes (i 40)
      (let ((row (random height))
    	    (column (random width)))
    	[drop-cell self (clone =berserker=) row column :loadout t :no-collisions t]))
    (dotimes (i 10) 
      [drop-cell self (clone =rook=) (+ 60 (random (- height 80)))
    		 (random width) :loadout t :no-collisions t])
    (dotimes (i 50) 
      [drop-cell self (clone =biclops=) (+ 100 (random (- height 80)))
    		 (random width) :loadout t :no-collisions t])
    (dotimes (i 50)
      [drop-cell self (clone =scanner=) (+ 40 (random (- height 100)))
    		 (random width) :loadout t :no-collisions t])
    ;; drop dead crewmembers to ransack
    (dotimes (i 70) 
      [drop-cell self (clone =crew-member=) (random height) (random width) :loadout t :no-collisions t])
    ;; drop other stuff
    (dotimes (i 25)
      [drop-cell self (clone =oxygen-tank=) (random height) (random width) :no-collisions t])
    ;;
    (setf *station-base-count* 0)
    (loop do (paint-station-piece self (random height) (random width) 20)
	  while (< *station-base-count* 30))
    ;;
    (dotimes (i 20)
      [drop-cell self (clone =energy=) (random height) (random width) :no-collisions t])
    [drop-cell self (clone =ion-shield=) (random height) (random width) :no-collisions t]
    ;; randomly place an entry point on the hull
    (let ((entry-row 1)
	  (entry-column (1+ (random (1- width)))))
      [drop-entry-point self entry-row entry-column])))

  

(define-method start freighter ()
  (play-music "metro" :loop t)
  [parent>>start self])
