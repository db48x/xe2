(in-package :blast)

;;; Infested oxygenless ships 

(defcell bulkhead
  (name :initform "Bulkhead")
  (tile :initform "grayplate")
  (categories :initform '(:opaque :obstacle))
  (descriptions :initform "It's a bulkhead."))

(defcell corridor
  (name :initform "Airless Corridor")
  (tile :initform "grayplate-dark")
  (categories :initform '(:magnetic))
  (description :initform 
"You will use 1 unit of oxygen for each square moved,
or each turn waited. Melee combat uses 2 units per hit."))

;;; Oxygen tank replenishes your air

(defcell oxygen-tank
  (tile :initform "oxygen-tank")
  (name :initform "Oxygen tank")
  (description :initform "This tank will replenish a portion of your oxygen supply."))

(define-method step oxygen-tank (stepper)
  (when [is-player stepper]
    (rlx:play-sample "pop-ssh")
    [>>say :narrator "You recover 40 points from the oxygen tank."]
    [>>stat-effect stepper :oxygen 40]
    [>>die self]))

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
  (name :initform "Interesting storage crate")
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

;;; The obelix patrollers

(defcell obelix 
  (tile :initform "techblackmetal")
  (categories :initform '(:actor :obstacle :target :enemy :opaque))
  (direction :initform (car (one-of '(:south :west))))
  (name :initform "Obelix")
  (description :initform "Their purpose and structure is unknown. They are best avoided."))

(define-method get-nasty obelix ()
  [damage [get-player *active-world*] 3]
  [say self "The Obelix hits you."])

(define-method run obelix ()
  (if [obstacle-in-direction-p *active-world* <row> <column> <direction>]
      (setf <direction> (opposite-direction <direction>))
      (progn [move self <direction>]
	     (when [adjacent-to-player self]
	       [get-nasty self]))))

(define-prototype boom-obelix (:parent =obelix=)
  (name :initform "Exploding obelix")
  (tile :initform "tech-orangemetal")
  (description :initform "A very quick way to die."))

(define-method get-nasty boom-obelix ()
  (labels ((boom (r c &optional (probability 50))
	     (prog1 nil
	       (when (and (< (random 100) probability)
			  [in-bounds-p *active-world* r c])
		 [drop-cell *active-world* (clone =explosion=) r c :no-collisions nil]))))
    (dolist (dir rlx:*compass-directions*)
      (multiple-value-bind (r c)
	  (step-in-direction <row> <column> dir)
	(boom r c 100)))
    ;; randomly sprinkle some fire around edges
    (trace-rectangle #'boom 
		     (- <row> 3) 
		     (- <column> 3) 
		     6 6)
    (trace-rectangle #'boom 
		     (- <row> 2) 
		     (- <column> 2) 
		     5 5)
    [die self]))

(define-method damage boom-obelix (points)
  [get-nasty self])

;;; The derelict freighter.

(define-prototype freighter (:parent =world=)
  (width :initform 48)
  (height :initform 150)
  (required-modes :initform '(:spacesuit))
  (categories :initform '(:airless :weightless))
  (name :initform nil)
  (ambient-light :initform 8)
  (pallet-size :initform 13))

(define-method generate freighter (&key (sequence-number (random 32768))
					(rooms 10)
					(stations 10)
					(box-groups 25)
					(berserkers 30))
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
	    (when (not (= 0 j))
	      (trace-rectangle #'drop-bulkhead
			       (+ (random 3)
				  (* pallet-size i))
			       (+ (random 4) 
				  (* pallet-size j))
			       (random pallet-size)
			       (random pallet-size)
			       :fill)))))
      ;; drop groups of boxes
      (dotimes (i box-groups)
    	(trace-rectangle #'drop-box (random height) (random width)
			 (+ 16 (random 14)) (+ 4 (random 3)) :fill)))
    ;; drop enemies
    (dotimes (i berserkers)
      (let ((row (random height))
    	    (column (random width)))
    	[drop-cell self (clone =berserker=) row column :loadout t :no-collisions t]))
    (dotimes (i 10) 
      [drop-cell self (clone =rook=) (+ 60 (random (- height 80)))
    		 (random width) :loadout t :no-collisions t])
    (dotimes (i 50) 
      [drop-cell self (clone =biclops=) (+ 100 (random (- height 80)))
    		 (random width) :loadout t :no-collisions t])
    (dotimes (i 40)
      [drop-cell self (clone =obelix=) (random height) (random width)
		 :loadout t :no-collisions t])
    (dotimes (i 40)
      [drop-cell self (clone =boom-obelix=) (random height) (random width)
		 :loadout t :no-collisions t])
    ;; drop dead crewmembers to ransack
    (dotimes (i 65) 
      [drop-cell self (clone =crew-member=) (random height) (random width) :loadout t :no-collisions t])
    ;; drop other stuff
    (dotimes (i 15)
      [drop-cell self (clone =oxygen-tank=) (random height) (random width) :no-collisions t])
    ;;
    (dotimes (i stations)
      (paint-station-piece self (random height) (random width) 20))
    ;;
    (dotimes (i 20)
      [drop-cell self (clone =energy=) (random height) (random width) :no-collisions t])
    [drop-cell self (clone =ion-shield=) (random height) (random width) :no-collisions t]
    ;; vaults
    (dotimes (i rooms)
      [drop-room self (random (- height 20)) (random (- width 20) )
		 (+ 8 (random 5))
		 (+ 6 (random 5))])
    ;; randomly place an entry point on the hull
    (let ((entry-row 1)
	  (entry-column (1+ (random (- width 2)))))
      [drop-entry-point self entry-row entry-column])))
  
(define-method random-treasure freighter ()
  (ecase (random 4)
    (0 =bomb-ammo=)
    (1 =crystal=)
    (2 =diamond=)
    (3 =pulse-ammo=)))

(define-method drop-room freighter (row column height width)
  (let ((rectangle nil))
    (labels ((collect-point (&rest args)
	       (prog1 nil (push args rectangle)))
	     (drop-floor (r c)
	       (prog1 nil [replace-cells-at self r c (clone =corridor=)])))
      (trace-rectangle #'collect-point row column height width)
      (trace-rectangle #'drop-floor row column height width :fill)
      ;; make sure there are openings
      (dotimes (i 2)
	(let ((n (random (length rectangle))))
	  (delete (nth n rectangle) rectangle)))
      (dolist (point rectangle)
	(destructuring-bind (r c) point
	  [drop-cell self (clone =bulkhead=) r c :no-collisions t])))
    (when (> 5 (random 10))
      (let ((treasure [random-treasure self]))
	(dotimes (i (+ 2 (random 10)))
	  [drop-cell self (clone treasure)
		     (+ 1 row (random width))
		     (+ 1 column (random height))
		     :no-collisions t])))))

(define-method start freighter ()
  (play-music "metro" :loop t)
  [parent>>start self])
