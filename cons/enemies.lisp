(in-package :cons-game)

;;; Basic enemy

(defcell shocker 
  (tile :initform "shocker")
  (auto-loadout :initform t)
  (description :initform "Creeps about until catching sight of the player;
Then it fires and gives chase.")
  (team :initform :enemy)
  (color :initform :cyan)
  (waveform :initform :square)
  (hit-points :initform (make-stat :base 2 :min 0 :max 45))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (speed :initform (make-stat :base 5 :min 0 :max 25))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 15)
  (energy :initform (make-stat :base 40 :min 0 :max 40 :unit :gj))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (stepping :initform t)
  (direction :initform :north)
  (attacking-with :initform nil)
  (firing-with :initform :center-bay)
  (categories :initform '(:actor :obstacle  :target :container :light-source :vehicle :repairable))
  (equipment-slots :initform '(:left-bay :right-bay :center-bay :extension)))

(define-method loadout shocker ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =wave-cannon=)]])

(define-method hit shocker (&optional object)
  [die self])

(define-method run shocker ()
  (let ((cannon [equipment-slot self :center-bay]))
    (when cannon [recharge cannon]))
  (let ((dir [direction-to-player self])
	(dist [distance-to-player self]))
    (if (< dist 13)
	(if (> 9 dist)
	    (progn [fire self dir]
		   (xe2:percent-of-time 3 [move self dir]))
	    (if [obstacle-in-direction-p *world* <row> <column> dir]
		[move self (random-direction)]
		[move self dir]))
	(percent-of-time 3 [move self (random-direction)]))))

(define-method die shocker () 
  (dotimes (n 10)
    [drop self (clone =noise=)])
  (percent-of-time 12 [drop self (clone =health=)])
  [play-sample self "yelp"]
  [parent>>die self])  

;;; Corruption

(defcell corruption 
  (tile :initform "corruption-east")
  (description :initform "Deadly digital audio data corruption.")
  (direction :initform :east)
  (clock :initform 200)
  (categories :initform '(:actor)))
 
(define-method step corruption (stepper)
  (when [is-player stepper]
    [die stepper]))

(define-method orient corruption (&optional dir)
  (when dir (setf <direction> dir))
  (setf <tile> (if (= 0 (random 2))
		   (ecase <direction>
		     (:north "corruption-north")
		     (:south "corruption-south")
		     (:east "corruption-east")
		     (:west "corruption-west"))
		   (ecase <direction>
		     (:north "corruption2-north")
		     (:south "corruption2-south")
		     (:east "corruption2-east")
		     (:west "corruption2-west")))))

(define-method run corruption ()
  (decf <clock>)
  (percent-of-time 5 [play-sample self "datanoise"])
  (if (plusp <clock>)
      [orient self]
      [die self]))

;;; Corruptors who leave a trail of digital audio corruption 

(defcell corruptor 
  (tile :initform "corruptor")
  (description :initform "Corruptors traverse the level, leaving a trail of deadly malformed data.")
  (team :initform :enemy)
  (color :initform :cyan)
  (waveform :initform :saw)
  (direction :initform (xe2:random-direction))
  (movement-cost :initform (make-stat :base 20))
  (max-items :initform (make-stat :base 2))
  (speed :initform (make-stat :base 3 :min 0 :max 5))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 15)
  (energy :initform (make-stat :base 400 :min 0 :max 40 :unit :gj))
  (hit-points :initform (make-stat :base 8 :min 0 :max 8))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (stepping :initform t)
  (direction :initform :north)
  (attacking-with :initform nil)
  (firing-with :initform :center-bay)
  (categories :initform '(:actor :obstacle  :target :container :light-source :vehicle :repairable))
  (equipment-slots :initform '(:left-bay :right-bay :center-bay :extension)))

(define-method loadout corruptor ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =wave-cannon=)]])

(define-method hit corruptor (&optional object)
  [die self])

(define-method run corruptor ()
  (let ((cannon [equipment-slot self :center-bay]))
    (when cannon [recharge cannon]))
  (let ((dir [direction-to-player self])
	(dist [distance-to-player self]))
    (when (> 9 dist)
      [fire self dir])
    (when [obstacle-in-direction-p *world* <row> <column> <direction>]
      (setf <direction> (if (= 0 (random 4))
			    (ecase <direction>
			      (:north :west)
			      (:west :south)
			      (:south :east)
			      (:east :north))
			    (ecase <direction>
			      (:north :east)
			      (:west :north)
			      (:south :west)
			      (:east :south)))))
    (let ((corruption (clone =corruption=)))
      [orient corruption <direction>]
      [drop self corruption]
      [move self <direction>])))

(define-method die corruptor () 
  (dotimes (n 10)
    [drop self (clone =noise=)])
  [play-sample self "yelp"]
  [parent>>die self])  

;;; Drones

(defsprite drone
  (description :initform "A security drone. Manufactures attacking replicant xioforms.")
  (team :initform :enemy)
  (color :initform :magenta)
  (waveform :initform :saw)
  (alarm-clock :initform 0)
  (pulse :initform (random *pulse-delay*))
  (image :initform "drone")
  (hit-points :initform (make-stat :base 12 :min 0))
  (direction :initform (random-direction))
  (speed :initform (make-stat :base 20))
  (movement-distance :initform (make-stat :base 1))
  (movement-cost :initform (make-stat :base 20))
  (categories :initform '(:drone :actor :target)))

(define-method run drone ()
  (percent-of-time 20 [play-sample self "sense2"])
  (when (< [distance-to-player self] 10)
    (if (zerop <alarm-clock>)
	(progn [play-sample self "alarm"]
	       [say self "The drone spawns an enemy!"]
	       (let ((enemy (or (percent-of-time 5 (clone =corruptor=))
				(clone =shocker=))))
		 [drop self enemy]
		 [loadout enemy])
	       (labels ((do-circle (image)
			  (prog1 t
			    (multiple-value-bind (x y) 
				[image-coordinates self]
			      (let ((x0 (+ x 10))
				    (y0 (+ y 10)))
				(draw-circle x0 y0 25 :destination image)
				(draw-circle x0 y0 30 :destination image)
				(draw-circle x0 y0 35 :destination image)
				(draw-circle x0 y0 40 :destination image))))))
		 [>>add-overlay :viewport #'do-circle])
	       (setf <alarm-clock> 60))
	(decf <alarm-clock>)))
  [move self <direction> [stat-value self :movement-distance]])

(define-method hit drone (&optional thing)
  (if [in-category thing :wave]
      (progn [play-sample self "yelp"]
	     [damage self 1])
      [>>say :narrator "This weapon has no effect on the Drone."]))

(define-method die drone ()
  [say self "The drone is destroyed!"]
  (dotimes (n 30)
    [drop self (clone =noise=)])
  [parent>>die self])

(define-method do-collision drone (other)
  (if [is-player other]
      [die other]
      (when [in-category other :obstacle]
	;; don't get hung up on the enemies we drop.
	(unless (and (has-field :team other)
		     (eq :enemy (field-value :team other)))
	  (unless (percent-of-time 10 (setf <direction> (opposite-direction <direction>)))
	    (setf <direction> (ecase <direction>
				(:here :west)
				(:northwest :west)
				(:northeast :east)
				(:north :west)
				(:west :south)
				(:southeast :east)
				(:southwest :south)
				(:south :east)
				(:east :north))))))))

;;; The radar-equipped Biclops is more dangerous.  

(define-prototype biclops (:parent xe2:=cell=)
  (name :initform "Biclops")
  (strength :initform (make-stat :base 15 :min 0 :max 50))
  (dexterity :initform (make-stat :base 15 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
  (equipment-slots :initform '(:robotic-arm))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (speed :initform (make-stat :base 1))
  (movement-cost :initform (make-stat :base 30))
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (hit-points :initform (make-stat :base 25 :min 0 :max 10))
  (tile :initform "biclops")
  (description :initform 
"The horror of human/machine hybrids is realized here.
Two heads. One human, one droid."))

(define-method initialize biclops ()
  [make-inventory self]
  [make-equipment self])

(define-method hit biclops (&optional object)
  [play-sample self "spark-pop"]
  [damage self 4])

(define-method loadout biclops ()
  (let ((probe (clone =shock-probe=)))
    [equip self [add-item self probe]]))

(define-method attack biclops (target)
  [play-sample self "yelp"]
  [expend-action-points self 50]
  [parent>>attack self target])

(define-method run biclops ()
  (clon:with-field-values (row column) self
    (let* ((world *world*)
	   (direction [direction-to-player *world* row column]))
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
  [play-sample self "blaagh3"]
  [parent>>die self])

;;; The deadly Scanner can be avoided because it moves (mostly) predictably

(defcell scanner 
  (tile :initform "scanner")
  (name :initform "Scanner")
  (categories :initform '(:obstacle :actor :equipper :opaque))
  (auto-loadout :initform t)
  (direction :initform :north)
  (speed :initform (make-stat :base 1))
  (hit-points :initform (make-stat :base 20 :min 0))
  (equipment-slots :initform '(:robotic-arm))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (attacking-with :initform :robotic-arm)
  (energy :initform (make-stat :base 800 :min 0 :max 1000))
  (firing-with :initform :robotic-arm)
  (strength :initform (make-stat :base 24))
  (dexterity :initform (make-stat :base 12))
  (description :initform 
"Scanners move in a search pattern until an enemy comes within firing distance.
They fire powerful heat-seeking bullets, but these can be shot down."))
	       
(define-method choose-new-direction scanner ()
  (setf <direction>
	(getf xe2:*left-turn*
	      (or <direction> :north))))
  
(define-method loadout scanner ()
  [make-inventory self]
  [make-equipment self]
  (let ((cannon (clone =lepton-cannon=)))
    [equip self [add-item self cannon]]
    [choose-new-direction self]))
  
(define-method run scanner ()
  [expend-action-points self 10]
  (clon:with-field-values (row column) self
    (let ((world *world*))
      (if (< [distance-to-player world row column] 10)
	  (let ((player-dir [direction-to-player world row column]))
	    [fire self player-dir])
	  (multiple-value-bind (r c)
	      (step-in-direction row column <direction>)
	    (when [category-at-p world r c :obstacle]
	      [choose-new-direction self])
	    [move self <direction>])))))

(define-method die scanner ()
  [play-sample self "death-alien"]
  [parent>>die self])

(define-method hit scanner (&optional other)
  [damage self 1])

;;; Rooks are the most difficult enemies. They bomb you.

(defcell rook 
  (categories :initform '(:actor :target :obstacle :opaque :enemy))
  (auto-loadout :initform t)
  (equipment-slots :initform '(:robotic-arm :shoulder-mount))
  (attacking-with :initform :robotic-arm)
  (firing-with :initform :robotic-arm)
  (dexterity :initform (make-stat :base 20))
  (max-items :initform (make-stat :base 1))
  (speed :initform (make-stat :base 2))
  (chase-distance :initform 15)
  (stepping :initform t)
  (behavior :initform :seeking)
  (clock :initform 0)
  (last-direction :initform :north)
  (strength :initform (make-stat :base 50))
  (movement-cost :initform (make-stat :base 14))
  (tile :initform "rook")
  (target :initform nil)
  (hit-points :initform (make-stat :base 16 :min 0 :max 40))
  (description :initform 
"The Rook is a cargo lifter modified to chase and bomb a target.
Hard to kill because of their evasive manuevers.")) 

(define-method run rook ()
  [expend-action-points self 1]
  (ecase <behavior>
    (:seeking [seek self])
    (:fleeing [flee self])))

(define-method hit rook (&optional object)
  [play-sample self "yelp"]
  [damage self 1])

(define-method die rook ()
  [play-sample self "blaagh2"]
  [parent>>die self])

(define-method seek rook ()
  (clon:with-field-values (row column) self
    (when (< [distance-to-player *world* row column] <chase-distance>)
      (let ((direction [direction-to-player *world* row column])
	    (world *world*))
	(if [adjacent-to-player world row column]
	    (progn
	      [>>fire self direction]
	      (setf <clock> 6
		    <behavior> :fleeing))
	    (if [obstacle-in-direction-p world row column direction]
		(let ((target [target-in-direction-p world row column direction]))
		  (if (and target (not [in-category target :enemy]))
		      (progn nil)
;;			[>>attack self direction]
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
	(let ((player-row [player-row *world*])
	      (player-column [player-column *world*]))
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
		  [move self (xe2:direction-to row column r c)])))))))

(define-method move rook (direction)
  (setf <last-direction> direction)
  [parent>>move self direction])

(define-method loadout rook ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =bomb-cannon=)]])

(define-method fire rook (direction)
  [play-sample self "drill-bit"]
  [parent>>fire self direction])
