(in-package :void)

;;; Radiation graviceptors leave energy behind when you kill them

(defcell graviceptor
 (tile :initform "gravicept")
 (hit-points :initform (make-stat :base 3 :max 3 :min 0))
 (speed :initform (make-stat :base 3))
 (strength :initform (make-stat :base 10))
 (defense :initform (make-stat :base 10))
 (stepping :initform t)
 (movement-cost :initform (make-stat :base 10))
 (max-items :initform (make-stat :base 2))
 (direction :initform (random-direction))
 (categories :initform '(:actor :obstacle :enemy :target))
 (description :initform 
"The deadly Graviceptor seeks out the player and explodes into a cloud
of poisonous radioactive gas."))

(define-method run graviceptor ()
  (clon:with-field-values (row column) self
    (let* ((world *world*)
	   (direction [direction-to-player world row column]))
      (if [adjacent-to-player world row column]
	  [explode self]
	  (if [obstacle-in-direction-p world row column direction]
	      (let ((target [target-in-direction-p world row column direction]))
		(if (and target (not [in-category target :enemy]))
		    [explode self]
		    (progn (setf <direction> (random-direction))
			   [>>move self direction])))
	      (progn (when (< 7 (random 10))
		       (setf <direction> (random-direction)))
		     [>>move self direction]))))))

(define-method drop-gas graviceptor (row column &key
					       (height (+ 3 (random 5)))
					       (width (+ 3 (random 5))))
  (labels ((drop-gas (r c)
	     (prog1 nil
	       [drop-cell *world* (clone =gas=) r c])))
    [play-sample self "pop-ssh"]
    (trace-rectangle #'drop-gas row column height width :fill)))

(define-method explode graviceptor ()
  ;; only when not in space debris... debris are "safe zones" from mines
  (when (notany #'(lambda (ob)
		    ;; this is ugly:
		    (eq =debris= (object-parent ob)))
		[cells-at *world* <row> <column>])

    (labels ((boom (r c &optional (probability 50))
	       (prog1 nil
		 (when (and (< (random 100) probability)
			    [in-bounds-p *world* r c])
		   [drop-cell *world* (clone =explosion=) r c :no-collisions nil]))))
      (dolist (dir xe2:*compass-directions*)
	(multiple-value-bind (r c)
	    (step-in-direction <row> <column> dir)
	  (boom r c 100)))
      ;; randomly sprinkle some fire around edges
      (trace-rectangle #'boom 
		       (- <row> 2) 
		       (- <column> 2) 
		       5 5)
      ;; release radiation
;      (when (< 3 (random 10))
      [drop-gas self (- <row> 2) (- <column> 2) :height 5 :width 5]
      [die self])))
  
(define-method damage graviceptor (points)
  (declare (ignore points))
  [stat-effect [get-player *world*] :score 5000]
  [>>say :narrator "Graviceptor destroyed. 5000 Bonus Points."]
  [explode self])

;;; A radiation probe releases a trail of toxic graviceptor particles.

(defcell radiation 
  (categories :initform '(:actor))
  (clock :initform 4)
  (description :initform 
"A radiation trail. Don't touch it. Fades after several turns."))
  
(define-method initialize radiation (&key direction clock)
  (setf <clock> clock)
  (setf <tile> (ecase direction
		 (:north "radiation-north")
		 (:south "radiation-south")
		 (:east "radiation-east")
		 (:west "radiation-west")
		 (:northeast "radiation-northeast")
		 (:northwest "radiation-northwest")
		 (:southeast "radiation-southeast")
		 (:southwest "radiation-southwest")
		 (:here "explosion"))))

(define-method run radiation ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (zerop <clock>)
    [die self]))

(define-method damage radiation (points)
  (declare (ignore points))
  [die self])

(define-method die radiation ()
  (when (> 1 (random 100))
    [drop self (clone =graviceptor=)])
  [parent>>die self])

(define-method step radiation (stepper)
  [drop self (clone =explosion=)]	       
  [damage stepper 1])
	   
(defcell probe 
  (tile :initform "probe")
  (hit-points :initform (make-stat :base 3 :max 3 :min 0))
  (speed :initform (make-stat :base 6))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (direction :initform (random-direction))
  (movement-cost :initform (make-stat :base 10))
  (categories :initform '(:actor :obstacle :enemy :target))
  (trail-length :initform (make-stat :base 10))
  (description :initform 
"This automated probe scans the area releasing graviceptor mines.
Avoid its radioactive trail and shoot it from a distance.
Watch out---they can spawn mines even after death!"))

(define-method move probe (direction)
  [drop self (clone =radiation= 
		    :direction direction 
		    :clock [stat-value self :trail-length])]
  [parent>>move self direction])

(define-method run probe ()
  (clon:with-field-values (row column) self
    (let* ((world *world*)
	   (direction [direction-to-player *world* row column])
	   (distance [distance-to-player *world* row column]))
      (if (< distance 8)
	  (progn 
	    [play-sample self "dtmf1"]
	    (setf <direction> (if (< distance 4)
				  (random-direction)
				  direction))
	    [>>move self <direction>])
	  ;; bounce around 
	  (progn 
	    (when [obstacle-in-direction-p *world* <row> <column> <direction>]
	      (setf <direction> (random-direction)))
	    [>>move self <direction>])))))

(define-method die probe ()
  [play-sample self "death-alien"]
  [drop self (clone =energy=)]
  [parent>>die self])

;;; The Canaz ship

(defcell canaz 
  (tile :initform "canaz")
  (hit-points :initform (make-stat :base 4 :max 4 :min 0))
  (speed :initform (make-stat :base 2))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (energy :initform (make-stat :base 100 :min 0 :max 100))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 6))
  (equipment-slots :initform '(:center-bay))
  (firing-with :initform :center-bay)
  (max-items :initform (make-stat :base 2))
  (direction :initform (random-direction))
  (categories :initform '(:actor :obstacle :enemy :target))
  (description :initform 
"The Canaz is a lightweight silicate-body fighter equipped with a muon cannon.
Not the typical choice of the best pilots."))

(define-method run canaz ()
  (clon:with-field-values (row column) self
    (let* ((world *world*)
	   (direction [direction-to-player *world* row column])
	   (distance [distance-to-player *world* row column]))
      (if (< distance 8)
	  (progn 
	    (setf <direction> (if (< distance 4)
				  (random-direction)
				  (if (> 3 (random 10))
				      (random-direction)
				      direction)))
	    [>>fire self direction])
	  ;; bounce around 
	  (progn 
	    (when [obstacle-in-direction-p *world* <row> <column> <direction>]
	      (setf <direction> (random-direction)))
	    [>>move self <direction>])))))

(define-method die canaz ()
  [play-sample self "death-alien"]
  [drop self (clone =energy=)]
  [parent>>die self])

(define-method loadout canaz ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =muon-cannon=)]])

;;; The Berserker is a relatively simple AI enemy.

;; Run in a straight line until hitting an obstacle.
;; Then choose a random direction and try again.
;; If the player gets close, try and attack him.

(defcell berserker 
  (name :initform "Berserker")
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
  (equipment-slots :initform '(:robotic-arm))
  (speed :initform (make-stat :base 7 :min 7))
  (max-items :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 3))
  (tile :initform "humanoid")
  (stepping :initform t)
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (direction :initform (xe2:random-direction))
  (strength :initform (make-stat :base 4 :min 0 :max 30))
  (dexterity :initform (make-stat :base 5 :min 0 :max 30))
  (intelligence :initform (make-stat :base 11 :min 0 :max 30))
  (hit-points :initform (make-stat :base 10 :min 0 :max 10))
  (description :initform 
"A protocol droid gone mad from radiation damage to the neural circuits.
Berserkers attack with a shock probe."))

(define-method initialize berserker ()
  [make-inventory self]
  [make-equipment self])

(define-method run berserker ()
  (clon:with-field-values (row column) self
    (let ((world *world*))
      (if (< [distance-to-player world row column] 5)
	  (let ((player-dir [direction-to-player world row column]))
	    (if [adjacent-to-player world row column]
		[>>attack self player-dir]
		[>>move self player-dir]))
	  (progn (when [obstacle-in-direction-p world row column <direction>]
		   (setf <direction> (xe2:random-direction)))
		 [>>move self <direction>])))))

(define-method die berserker ()
  (when (> 5 (random 10))
    [drop self (clone (random-powerup))])
  [play-sample self "blaagh"]
  [parent>>die self])

(define-method loadout berserker ()
  (let ((probe (clone =shock-probe=)))
    [equip self [add-item self probe]]))

(define-method attack berserker (target)
  [play-sample self "drill-little"]
  [parent>>attack self target])


;;; Spike beam 

; (defcell spike-beam 

;;; Guardians protect a given cell.

(defcell guardian 
  (categories :initform '(:actor :target :obstacle :opaque :enemy))
  (equipment-slots :initform '(:robotic-arm :shoulder-mount))
  (attacking-with :initform :robotic-arm)
  (firing-with :initform :robotic-arm)
  (dexterity :initform (make-stat :base 11))
  (max-items :initform (make-stat :base 1))
  (speed :initform (make-stat :base 10))
  (stepping :initform t)
  (behavior :initform :homing)
  (clock :initform 8)
  (clock-reset-value :initform 8)
  (scouting-direction :initform :north)
  (attack-distance :initform 10)
  (strength :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 7))
  (tile :initform "guardian")
  (defended-cell :initform nil)
  (hit-points :initform (make-stat :base 20 :min 0 :max 60))
  (description :initform 
"The Guardian cell patrols the area around a given protected cell,
and attacks anyone who comes near."))

(define-method defend guardian (defended-cell)
  (setf <defended-cell> defended-cell))

(define-method run guardian ()
  (ecase <behavior>
    (:homing [home self])
    (:scouting [scout self])))

(define-method home guardian ()
  (decf <clock>)
  (clon:with-field-values (row column) self
    (if (< [distance-to-player self] <attack-distance>)
	(let ((direction [direction-to-player self])
	      (world *world*))
	  (if [adjacent-to-player self]
	      (progn (format t "FOO")
		     [attack self direction])
	      (progn
		(when [obstacle-in-direction-p world row column direction]
		  (setf <direction> (random-direction)))
		[move self direction])))
	;; otherwise, move toward the defended cell until clock runs out
	(let* ((cell <defended-cell>)
	       (r0 (field-value :row cell))
	       (c0 (field-value :column cell)))
	  (if [adjacent-to-player self]
	      [attack self direction])
	  (when [obstacle-in-direction-p *world* row column (direction-to row column r0 c0)]
	    (setf <direction> (random-direction))
	    [move self <direction>])
	  [move self (direction-to row column r0 c0)]
	  (when (<= <clock> 0)
	    (setf <scouting-direction> (random-direction))
	    (setf <clock> <clock-reset-value>)
	    (setf <behavior> :scouting))))))

(define-method scout guardian ()
  (decf <clock>)
  ;; check for player 
  (if (< [distance-to-player self] <attack-distance>)
      (setf <behavior> :homing)
      ;; are we done scouting? then begin homing. 
      (if (<= <clock> 0)
	  (setf <clock> <clock-reset-value>
		<behavior> :homing)
	  ;; otherwise continue scouting
	  [move self <scouting-direction>])))

(define-method loadout guardian ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =shock-probe=)]])

;;; The GOND is a multi-warhead-launching superguardian

(define-prototype gond (:parent =guardian=)
  (tile :initform "gond")
  (attack-distance :initform 14)
  (strength :initform (make-stat :base 14))
  (hit-points :initform (make-stat :base 20 :min 0 :max 60)))

(define-method loadout gond ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =multi-missile-launcher=)]])

(define-method home gond ()
  (decf <clock>)
  (clon:with-field-values (row column) self
    (if (< [distance-to-player self] <attack-distance>)
	[fire self [direction-to-player self]]
	;; otherwise, move toward the defended cell until clock runs out
	(let* ((cell <defended-cell>)
	       (r0 (field-value :row cell))
	       (c0 (field-value :column cell)))
	  (if [obstacle-in-direction-p *world* row column 
				       (direction-to row column r0 c0)]
	      (progn (setf <direction> (random-direction))
		     [move self <direction>])
	      [move self (direction-to row column r0 c0)])
	  (when (<= <clock> 0)
	    (setf <scouting-direction> (random-direction))
	    (setf <clock> <clock-reset-value>)
	    (setf <behavior> :scouting))))))

(define-method die gond ()
  [play-sample self "blaagh3"]
  [parent>>die self])

;;; The speed-sucking Lymphocytes

(defcell lymphocyte
 (tile :initform "lymphocyte")
 (hit-points :initform (make-stat :base 12 :max 12 :min 0))
 (speed :initform (make-stat :base 4))
 (strength :initform (make-stat :base 10))
 (defense :initform (make-stat :base 10))
 (stepping :initform t)
 (movement-cost :initform (make-stat :base 10))
 (max-items :initform (make-stat :base 2))
 (direction :initform (random-direction))
 (categories :initform '(:actor :obstacle :enemy :target))
 (description :initform
"STAY AWAY! These can cause permanent speed drains, paralysis, and death."))

(define-method speedsuck lymphocyte (victim)
  [play-sample self "lymph"]
  [>>say :narrator "The speed-sucking Lymphocyte drains your speed by 2 points!"]
  [stat-effect victim :speed -2])

(define-method run lymphocyte ()
  (clon:with-field-values (row column) self
    (let ((world *world*))
      (if (< [distance-to-player world row column] 8)
	  (let ((player-dir [direction-to-player world row column]))
	    (if [adjacent-to-player world row column]
		[>>speedsuck self [resolve self player-dir]]
		[>>move self player-dir]))
	  (progn (when [obstacle-in-direction-p world row column <direction>]
		   (setf <direction> (xe2:random-direction)))
		 [>>move self <direction>])))))
