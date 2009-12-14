(in-package :xong)

;;; Scoring points

(defun score (points)
  [score-points [get-player *world*] points])

;;; The player's tail

(defcell tail 
  (categories :initform '(:actor))
  (clock :initform 4))
  
(define-method initialize tail (&key direction clock)
  (setf <clock> clock)
  (setf <tile> (ecase direction
		 (:north "tail-north")
		 (:south "tail-south")
		 (:east "tail-east")
		 (:west "tail-west"))))

(define-method run tail ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (< <clock> 0) (setf <clock> 0))
  (when (zerop <clock>)
    [die self]))

;; (define-method step tail (stepper)
;;   (when [in-category stepper :puck]

;;; A tail extender powerup

(defcell extender 
  (tile :initform "plus")
  (description :initform 
"This powerup extends your trail."))	      

(define-method step extender (stepper)
  (when [in-category stepper :tailed]
    [play-sample self "worp"]
    [stat-effect stepper :tail-length 7]
    [stat-effect stepper :score 2000]
    [die self]))

;;; Chevrons change the direction of the puck

(defcell chevron
  (tile :initform "chevron-east")
  (categories :initform '(:chevron))
  (description :initform 
"Chevrons change the direction of the puck and certain enemies."))

(defvar *chevron-tiles* '(:north "chevron-north"
			  :south "chevron-south"
			  :east "chevron-east"
			  :west "chevron-west"))

(define-method orient chevron (direction)
  (assert (member direction '(:north :south :east :west)))
  (setf <tile> (getf *chevron-tiles* direction))
  (setf <direction> direction))

(define-method step chevron (stepper)
  (when [in-category stepper :puck]
    [play-sample self "chevron"]
    [kick stepper <direction>]))

;;; Diamond pickup replenishes chevrons

(defcell diamond 
  (tile :initform "chevron-pickup")
  (name :initform "Chevron pack")
  (categories :initform '(:exclusive))
  (description :initform "Adds five chevrons to your inventory."))

(define-method step diamond (stepper)
  (when [in-category stepper :pointer]
    [stat-effect stepper :chevrons 5]
    [play-sample self "worp"]
    (score 1000)
    [die self]))

;;; Our hero, the player

(defvar *player-tiles* '(:purple "player-purple"
			:black "player-black"
			:red "player-red"
			:blue "player-blue"
			:orange "player-orange"
			:green "player-green"
			:white "player-white"
			:yellow "player-yellow"
			 :other "player-other"))

(defparameter *shield-time* 70)

(defcell player 
  (tile :initform "player")
  (name :initform "Player")
  (score :initform (make-stat :base 0 :min 0))
  (shield-clock :initform 0)
  (last-direction :initform :north)
  (dead :initform nil)
  (puck :initform nil)
  (chevrons :initform (make-stat :base 5 :min 0 :max 10))
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (strength :initform (make-stat :base 13))
  (tail-length :initform (make-stat :base 20 :min 0))
  (dexterity :initform (make-stat :base 13))
  (defense :initform (make-stat :base 15))
  (equipment-slots :initform '(:left-hand :right-hand))
  (hearing-range :initform 1000)
  (hit-points :initform (make-stat :base 1 :min 0 :max 2))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (stepping :initform t)
  (attacking-with :initform :right-hand)
  (light-radius :initform 3)
  (categories :initform '(:actor :tailed :player :target :above
			  :container :light-source :pointer))
  (description :initform "This is you! Move with the arrow keys or numeric keypad."))

(define-method run player ()
  (unless <dead>
    (setf <tile> (if (null <puck>)
		     "player-empty"
		     (getf *player-tiles* (if (has-field :color <puck>)
					      (field-value :color <puck>)
					      :other))))
    [run-shield self]
    [step-on-current-square self]))

(define-method run-shield player (&optional clock)
  (clon:with-fields (shield-clock row column) self
    (when clock (setf shield-clock clock))
    (decf shield-clock)
    ;; warning when about to expire
    (when (= 10 shield-clock)
      [play-sample self "shield-warning"])
    (if (plusp shield-clock)
	(labels ((draw-shield (image)
		   (prog1 t (multiple-value-bind (x y)
				[viewport-coordinates self]
			      (let ((circles (1+ (truncate (/ shield-clock 5))))
				    (radius 16))
				(dotimes (n circles)
				  (draw-circle (+ x 8) (+ y 8) radius :color ".cyan" :destination image)
				  (incf radius 2)))))))
	  [play-sample self "shield-sound"]
	  [>>add-overlay :viewport #'draw-shield]))))
	
(define-method damage player (points)
  (when (not (plusp <shield-clock>))
    [parent>>damage self points]))
   
(define-method score-points player (points)
  [stat-effect self :score points]
  [>>narrateln :narrator (format nil "Scored ~S points." points)])

(define-method quit player ()
  (xe2:quit :shutdown))

(define-method pause player ()
  [pause *world*])

(define-method step player (stepper)
  (when [in-category stepper :item]
    [grab self stepper])
  (when [in-category stepper :snake]
    [damage self 1]))

(define-method drop-tail player ()
  [drop self (clone =tail= 
		    :direction <last-direction> 
		    :clock [stat-value self :tail-length])])

(define-method restart player ()
  (let ((player (clone =player=)))
    [destroy *universe*]
    [set-player *universe* player]
    [set-character *status* player]
    [play *universe*
	  :address '(=menu-world=)]
    [loadout player]
    [play-sample self "go"]))

(define-method drop-chevron player (direction)
  (unless <dead>
    (if (zerop [stat-value self :chevrons])
	(progn [play-sample self "error"]
	       [say self "You don't have any chevrons to drop."])
	(if [category-at-p *world* <row> <column> :chevron]
	    (progn [play-sample self "error"]
		   [say self "You can't drop a chevron on top of another chevron."])
	    (let ((chevron (clone =chevron=)))
	      [drop self chevron]
	      [play-sample self "powerup"]
	      [stat-effect self :chevrons -1]
	      [orient chevron direction])))))

(define-method move player (direction)
  (unless <dead>
    (setf <last-direction> direction)
    [drop-tail self]
    [parent>>move self direction]))
  
(define-method loadout player ()
  (setf <puck> (clone =puck=)))

(define-method throw player (direction)
  (assert (member direction '(:north :south :east :west)))
  (unless <dead>
    (clon:with-fields (puck) self
      (when puck
	[drop-cell *world* puck <row> <column> :no-stepping t]
	[kick puck direction]
	(setf puck nil)
	[play-sample self "serve"]))))

(define-method grab player (puck)
  (assert [in-category puck :puck])
  (setf <puck> puck)
  [delete-from-world puck]
  [play-sample self "grab"])

(define-method pause player ()
  [pause *world*])

(define-method die player ()
  (unless <dead>
    (setf <tile> "skull")
    [play-sample self "death"]
    [say self "You died. Press ESCAPE to try again."]
    (setf <dead> t)))

;;; The puck

(defvar *puck-tiles* '(:purple "puck-purple"
			:black "puck-black"
			:red "puck-red"
			:blue "puck-blue"
			:orange "puck-orange"
			:green "puck-green"
			:white "puck-white"
			:yellow "puck-yellow"))

(defcell puck
  (tile :initform "puck")
  (description :initform "A frictionless paint-absorbent hockey puck.")
  (categories :initform '(:puck :obstacle :target :actor :paintable :item))
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform :here)
  (stepping :initform t)
  (color :initform :white))

(define-method kick puck (direction)
  (setf <phase-number> 0)
  (setf <direction> direction))

(define-method bounce puck ()
  (setf <direction> (opposite-direction <direction>))
  [play-sample self "bounce"])
  ;; ;; check player collision; this happens when shooting an adjacent wall
  ;; (when [category-at-p *world* <row> <column> :player]
  ;;   [grab [get-player *world*] self]))

(define-method paint puck (color)
  (setf <color> color)
  (setf <tile> (getf *puck-tiles* color)))

(define-method move puck (direction)
  (multiple-value-bind (r c) 
      (step-in-direction <row> <column> direction)
    (let ((obstacle [obstacle-at-p *world* r c]))
      (when obstacle
	[bounce self]
	(when (clon:object-p obstacle)
	  (if [is-player obstacle]
	      [grab obstacle self]
	      ;; it's not the player. see if we can color, or get paint
	      (progn 
		(when [in-category obstacle :paintable]
		  [paint obstacle <color>])
		(when [in-category obstacle :breakable]
		  [die obstacle])
		(when [in-category obstacle :wall]
		  [paint self (field-value :color obstacle)]
		  [die obstacle]
		  [parent>>move self direction])
		(when [in-category obstacle :gate]
		  [open obstacle]
		  (when [category-at-p *world* <row> <column> :player]
		    [parent>>move self direction]))
		(when [in-category obstacle :bulkhead]
		  [parent>>move self direction :ignore-obstacles]))))))
    (when [is-located self]
      [parent>>move self <direction>])))

(define-method run puck ()
  ;; pucks don't stop moving.
  (if (eq :here <direction>)
      [die self]
      [move self <direction>]))

(define-method die puck ()
  [say self "You lost your puck!"]
  [play-sample self "buzz"]
  [parent>>die self])

;;; Powerup mystery box

(defcell mystery-box
  (name :initform "Mystery box")
  (tile :initform "mystery-box")
  (categories :initform '(:target :obstacle :breakable :exclusive))
  (description :initform  "Break it open to find a surprise inside!"))

(define-method die mystery-box ()
  (let ((item (clone (car (one-of (list =snowflake= =shield=))))))
    [drop self item]
    [parent>>die self]))

;;; Special puck: snowflake

(defcell snowflake
  (tile :initform "snowflake")
  ;; not paintable
  (categories :initform '(:puck :target :actor :item :snowflake))
  (description :initform 
"A puck that freezes enemies for a brief time.
You must drop any other puck in order to pick this up.")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform :here)
  (stepping :initform t))

(define-method bounce snowflake ()
  (setf <direction> (opposite-direction <direction>))
  [play-sample self "bounce"])

(define-method move snowflake (direction)
  (multiple-value-bind (r c) 
      (step-in-direction <row> <column> direction)
    (let ((obstacle [obstacle-at-p *world* r c]))
      (when obstacle
	[bounce self]
	(when (clon:object-p obstacle)
	  (if [is-player obstacle]
	      [grab obstacle self]
	      ;; if it's an enemy or puck, freeze it!
	      (progn 
		(when (or [in-category obstacle :puck]
			  [in-category obstacle :enemy])
		  [freeze self obstacle])
		(when [in-category obstacle :gate]
		  [open obstacle])))))
      (when [is-located self]
	[parent>>move self <direction>]))))

(define-method kick snowflake (direction)
  (setf <direction> direction)
  (when [category-at-p *world* <row> <column> :player]
    [move self direction]))

(define-method freeze snowflake (enemy)
  [play-sample self "freeze"]
  [expend-action-points enemy 100 -100])

(define-method paint snowflake (color)
  nil)

(define-method step snowflake (stepper)
  (if (and [is-player stepper]
	   (null (field-value :puck stepper)))
      (progn (score 1000)
	     [grab stepper self])
      (when [in-category stepper :enemy]
	[bounce self]
	[freeze self stepper])))

(define-method run snowflake ()
  (unless (eq :here <direction>)
    [move self <direction>]))

(define-method die snowflake ()
  [say self "The snowflake was destroyed."]
  [play-sample self "buzz"]
  [parent>>die self])

;;; Special puck: shield

(defcell shield
  (tile :initform "shield")
  ;; not paintable
  (categories :initform '(:puck :target :item :shield))
  (player :initform nil)
  (description :initform 
"A puck that creates a shield around you when fired.
You must drop any other puck in order to pick this up.")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform :here)
  (stepping :initform t))

(define-method kick shield (direction)
  (when <player>
    [run-shield <player> *shield-time*]
    [die self]))

(define-method step shield (stepper)
  (if (and [is-player stepper]
	   (null (field-value :puck stepper)))
      (progn (score 1000)
	     [grab stepper self]
	     (setf <player> stepper)
	     [say self "You picked up the shield puck. Fire it to become invulnerable!"])))

