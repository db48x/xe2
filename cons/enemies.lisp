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
  (speed :initform (make-stat :base 10 :min 0 :max 25))
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
  (when [in-category thing :wave]
    [play-sample self "yelp"]
    [damage self 1]))

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

;;; Door to next level

(define-prototype exit (:parent xe2:=gateway=)
  (tile :initform "exit")
  (name :initform "Area exit ")
  (description :initform "Exit to the next area.")
  (categories :initform '(:gateway))
  (address :initform nil))

(define-method level exit (level)
  (setf <address> (generate-level-address level)))

(define-method step exit (stepper)
  (halt-sample t)
  (when [in-category stepper :player]
    [say self "You made it to the next level!"]
    [activate self]))

(define-method run exit ()
  nil)
