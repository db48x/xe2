(in-package :blast)

;;; A melee weapon for enemy robots: the Shock Probe

(defcell shock-probe 
  (name :initform "Shock probe")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "shock-probe")
  (attack-power :initform (make-stat :base 5))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (weight :initform 3000)
  (equip-for :initform '(:robotic-arm :left-hand :right-hand)))

;;; Radiation graviceptors leave energy behind when you kill them. 

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
 (movement-cost :initform (make-stat :base 10))
 (categories :initform '(:actor :obstacle :enemy :target)))

(define-method run graviceptor ()
  (clon:with-field-values (row column) self
    (let* ((world *active-world*)
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
	       [drop-cell *active-world* (clone =gas=) r c])))
    [play-sample self "pop-ssh"]
    (trace-rectangle #'drop-gas row column height width :fill)))

(define-method explode graviceptor ()
  ;; only when not in space debris... debris are "safe zones" from mines
  (when (notany #'(lambda (ob)
		    ;; this is ugly:
		    (eq =debris= (object-parent ob)))
		[cells-at *active-world* <row> <column>])
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
		       (- <row> 2) 
		       (- <column> 2) 
		       5 5)
      ;; release radiation
;      (when (< 3 (random 10))
      [drop-gas self (- <row> 2) (- <column> 2) :height 5 :width 5]
      [die self])))
  
(define-method damage graviceptor (points)
  (declare (ignore points))
  [stat-effect [get-player *active-world*] :score 5000]
  [>>say :narrator "Graviceptor destroyed. 5000 Bonus Points."]
  [explode self])

;;; A radiation probe releases a trail of toxic graviceptor particles.

(defcell radiation 
  (categories :initform '(:actor))
  (clock :initform 4))
  
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
  (when (eq =ship= (object-parent stepper))
    [drop self (clone =explosion=)]	       
    [damage stepper 1]))
	   
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
  (trail-length :initform (make-stat :base 10)))

(define-method move probe (direction)
  [drop self (clone =radiation= 
		    :direction direction 
		    :clock [stat-value self :trail-length])]
  [parent>>move self direction])

(define-method run probe ()
  (clon:with-field-values (row column) self
    (let* ((world *active-world*)
	   (direction [direction-to-player *active-world* row column])
	   (distance [distance-to-player *active-world* row column]))
      (if (< distance 8)
	  (progn 
	    [play-sample self "dtmf1"]
	    (setf <direction> (if (< distance 4)
				  (random-direction)
				  direction))
	    [>>move self <direction>])
	  ;; bounce around 
	  (progn 
	    (when [obstacle-in-direction-p *active-world* <row> <column> <direction>]
	      (setf <direction> (random-direction)))
	    [>>move self <direction>])))))

(define-method die probe ()
  [play-sample self "death-alien"]
  [drop self (clone =energy=)]
  [say *billboard* :destroy]
  [parent>>die self])

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
  (direction :initform (rlx:random-direction))
  (strength :initform (make-stat :base 4 :min 0 :max 30))
  (dexterity :initform (make-stat :base 5 :min 0 :max 30))
  (intelligence :initform (make-stat :base 11 :min 0 :max 30))
  (hit-points :initform (make-stat :base 10 :min 0 :max 10)))

(define-method initialize berserker ()
  [make-inventory self]
  [make-equipment self])

(define-method run berserker ()
  (clon:with-field-values (row column) self
    (let ((world *active-world*))
      (if (< [distance-to-player world row column] 5)
	  (let ((player-dir [direction-to-player world row column]))
	    (if [adjacent-to-player world row column]
		[>>attack self player-dir]
		[>>move self player-dir]))
	  (progn (when [obstacle-in-direction-p world row column <direction>]
		   (setf <direction> (rlx:random-direction)))
		 [>>move self <direction>])))))

(define-method die berserker ()
  (when (> 3 (random 10))
    [drop self (clone (random-powerup))])
  [parent>>die self])

(define-method loadout berserker ()
  (let ((probe (clone =shock-probe=)))
    [equip self [add-item self probe]]))

(define-method attack berserker (target)
  [play-sample self "drill-little"]
  [parent>>attack self target])

;;; The radar-equipped Biclops is more dangerous.  

(define-prototype biclops (:parent rlx:=cell=)
  (name :initform "Biclops")
  (strength :initform (make-stat :base 15 :min 0 :max 50))
  (dexterity :initform (make-stat :base 15 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
  (equipment-slots :initform '(:robotic-arm))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (speed :initform (make-stat :base 5))
  (movement-cost :initform (make-stat :base 5))
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (hit-points :initform (make-stat :base 25 :min 0 :max 10))
  (tile :initform "biclops"))

(define-method initialize biclops ()
  [make-inventory self]
  [make-equipment self])

(define-method loadout biclops ()
  (let ((probe (clone =shock-probe=)))
    [equip self [add-item self probe]]))

(define-method attack biclops (target)
  [play-sample self "drill-big"]
  [parent>>attack self target])

(define-method run biclops ()
  (clon:with-field-values (row column) self
    (let* ((world *active-world*)
	   (direction [direction-to-player *active-world* row column]))
      (if [adjacent-to-player world row column]
	  [>>attack self direction]
	  (if [obstacle-in-direction-p world row column direction]
	      (let ((target [target-in-direction-p world row column direction]))
		(if (and target (not [in-category target :enemy]))
		    [>>attack self direction]
		    (progn (setf <direction> (random-direction))
			   [>>move self direction])))
	      (progn (when (< 7 (random 10))
		       (setf <direction> (random-direction)))
		     [>>move self direction]))))))

(define-method die biclops ()
  (when (> 4 (random 10))
    [drop self (clone (random-stat-powerup))])
  [parent>>die self])

;;; The deadly Scanner can be avoided because it moves (mostly) predictably

(defcell scanner 
  (tile :initform "scanner")
  (name :initform "Scanner")
  (categories :initform '(:obstacle :actor :equipper :opaque))
  (direction :initform nil)
  (speed :initform (make-stat :base 5))
  (hit-points :initform (make-stat :base 20 :min 0))
  (equipment-slots :initform '(:robotic-arm))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (attacking-with :initform :robotic-arm)
  (energy :initform (make-stat :base 800 :min 0 :max 1000))
  (firing-with :initform :robotic-arm)
  (strength :initform (make-stat :base 24))
  (dexterity :initform (make-stat :base 12)))

(define-method choose-new-direction scanner ()
  (setf <direction>
	(if (= 0 (random 20))
	    ;; occasionally choose a random dir
	    (nth (random 3)
		 '(:north :south :east :west))
	    ;; otherwise turn left
	    (getf '(:north :west :west :south :south :east :east :north)
		  (or <direction> :north)))))
  
(define-method loadout scanner ()
  (let ((cannon (clone =lepton-cannon=)))
    [equip self [add-item self cannon]]
    [choose-new-direction self]))
  
(define-method initialize scanner ()
  [make-inventory self]
  [make-equipment self])

(define-method run scanner ()
  (clon:with-field-values (row column) self
    (let ((world *active-world*))
      (if (< [distance-to-player world row column] 8)
	  (let ((player-dir [direction-to-player world row column]))
	    [queue>>fire self player-dir])
	  (multiple-value-bind (r c)
	      (step-in-direction <row> <column> <direction>)
	    (when [obstacle-at-p world r c]
	      [choose-new-direction self])
	    [queue>>move self <direction>])))))

(define-method die scanner ()
  [play-sample self "death-alien"]
  [parent>>die self])

;;; Rooks are the most difficult enemies. They bomb you.

(defcell rook 
  (categories :initform '(:actor :target :obstacle :opaque :enemy))
  (equipment-slots :initform '(:robotic-arm :shoulder-mount))
  (attacking-with :initform :robotic-arm)
  (firing-with :initform :robotic-arm)
  (dexterity :initform (make-stat :base 20))
  (max-items :initform (make-stat :base 1))
  (speed :initform (make-stat :base 12))
  (stepping :initform t)
  (behavior :initform :seeking)
  (clock :initform 0)
  (last-direction :initform :north)
  (strength :initform (make-stat :base 50))
  (movement-cost :initform (make-stat :base 8))
  (tile :initform "rook")
  (target :initform nil)
  (hit-points :initform (make-stat :base 40 :min 0 :max 40)))

(define-method run rook ()
  (ecase <behavior>
    (:seeking [seek self])
    (:fleeing [flee self])))

(define-method seek rook ()
  (clon:with-field-values (row column) self
    (when (< [distance-to-player *active-world* row column] 10)
      (let ((direction [direction-to-player *active-world* row column])
	    (world *active-world*))
	(if [adjacent-to-player world row column]
	    (progn
	      [>>fire self direction]
	      (setf <clock> 6
		    <behavior> :fleeing))
	    (if [obstacle-in-direction-p world row column direction]
		(let ((target [target-in-direction-p world row column direction]))
		  (if (and target (not [in-category target :enemy]))
		      (progn
;;			[>>attack self direction]
			[play-sample self "drill-bit"])
		      (progn (setf <direction> (random-direction))
			     [>>move self direction])))
		(progn (when (< 7 (random 10))
			 (setf <direction> (random-direction)))
		       [>>move self direction])))))))

(define-method flee rook ()
  (decf <clock>)
  ;; are we done fleeing? then begin seeking. 
  (if (<= <clock> 0)
      (setf <behavior> :seeking)
      ;; otherwise, flee
      (clon:with-field-values (row column) self
	(let ((player-row [player-row *active-world*])
	      (player-column [player-column *active-world*]))
	  (labels ((neighbor (r c direction)
		     (multiple-value-bind (r0 c0)
			 (step-in-direction r c direction)
		       (list r0 c0)))
		   (all-neighbors (r c)
		     (let (ns)
		       (dolist (dir *compass-directions*)
			 (push (neighbor r c dir) ns))
		       ns))
		   (score (r c)
		     (distance player-column player-row c r)))
	    (let* ((neighbors (all-neighbors row column))
		   (scores (mapcar #'(lambda (pair)
				       (apply #'score pair))
				   neighbors))
		   (farthest (apply #'max scores))
		   (square (nth (position farthest scores)
				neighbors)))
	      (destructuring-bind (r c) square
		  [move self (rlx:direction-to row column r c)])))))))

(define-method move rook (direction)
  (setf <last-direction> direction)
  [parent>>move self direction])

(define-method loadout rook ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =bomb-cannon=)]])