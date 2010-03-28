(in-package :void)

;;; The rusty wrench; basic melee weapon

(define-prototype wrench (:parent xe2:=cell=)
  (name :initform "Rusty wrench")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "rusty-wrench")
  (attack-power :initform (make-stat :base 4)) ;; points of raw damage
  (attack-cost :initform (make-stat :base 5))
  (accuracy :initform (make-stat :base 80 :unit :percent))
  (weight :initform 10000) ;; grams
  (equip-for :initform '(:left-hand :right-hand)))

(define-method step wrench (stepper)
  (when [is-player stepper]
    [>>take stepper :direction :here :category :item]))

;;; Death icon.

(defparameter *death-message* "You are dead.")
(defparameter *game-over-message* "GAME OVER.")

(define-prototype skull (:parent xe2:=cell=)
  (tile :initform "skull")
  (player :initform nil)
  (categories :initform '(:dead :player :actor))
  (action-points :initform 0))

(define-method run skull ()
  (xe2:show-widgets))

(define-method forward skull (&rest args)
  (declare (ignore args))
  [say self *death-message*])
  
(define-method move skull (&rest args)
  (declare (ignore args))
 [say self *death-message*])

(define-method attack skull (&rest args)
  (declare (ignore args))
 [say self *death-message*])

(define-method fire skull (&rest args)
  (declare (ignore args))
 [say self *death-message*])

(define-method quit skull ()
  (xe2:quit :shutdown))

(define-method initialize skull (player)
  [say self *death-message*]
  [say self *game-over-message*]
  [say self "Press Control-PERIOD (i.e. \".\") to restart."])

(define-method restart skull ()
  (let ((dude (clone =contractor=))
	(ship (clone =olvac=)))
    [destroy *active-universe*]
    [set-character *ship-status* ship]
    [set-character *dude-status* dude]
    [set-player *active-universe* ship]
    [play *active-universe*
	  :address '(=star-sector= :width 80 :height 80 
		     :stars 80 :freighters 6 :sequence-number (genseq))]
    [proxy ship dude]
    [loadout dude]
    [loadout ship]
    [play-sample self "go"]))

;;; Pulse particle

(defcell pulse
  (tile :initform "pulse")
  (categories :initform '(:actor :target))
  (hit-points :initform (make-stat :base 10 :min 0))
  (clock :initform (+ 4 (random 4))))

(define-method run pulse ()
  (when (zerop <clock>)
    [die self])
  (decf <clock>)
  (xe2:do-cells (cell [cells-at *world* <row> <column>])
    (when (not [is-player cell])
      [damage cell 5])))

;;; Pulse wave cannon damages anything near you.

(defcell pulse-cannon 
  (categories :initform '(:item :weapon :equipment))
  (attack-power :initform (make-stat :base 10))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (weight :initform 3000)
  (equip-for :initform '(:left-bay))
  (tile :initform "pulse-cannon"))

(define-method activate pulse-cannon ()
  (let* ((world *world*)
	 (row [player-row world])
	 (column [player-column world]))
    (if (plusp [stat-value <equipper> :pulse-ammo])
	(progn 
	  [say self "Activating pulse cannon."]
	  [play-sample <equipper> "activate"]
	  (labels ((drop-pulse (r c)
		     (prog1 nil 
		       [drop-cell world (clone =pulse=) r c])))
	    (trace-rectangle #'drop-pulse (- row 2) (- column 2)
			     5 5 :fill)))
	[say self "Out of pulse ammo."])))

;;; The Contractor.

(defcell contractor 
  (tile :initform "voyager")
  (mode :initform :spacesuit)
  (name :initform "Contractor")
  (speed :initform (make-stat :base 12 :min 0 :max 25))
  (strength :initform (make-stat :base 13))
  (dexterity :initform (make-stat :base 13))
  (piloting :initform (make-stat :base 15))
  (defense :initform (make-stat :base 15))
  (equipment-slots :initform '(:left-hand :right-hand :belt :extension :feet))
  (hearing-range :initform 18)
  (energy :initform (make-stat :base 20.0 :min 0.0 :max 20.0 :unit :gj))
  (endurium :initform (make-stat :base 0 :min 0 :max 20 :unit :kg))
  (credits :initform (make-stat :base 512 :min 0 :unit :cr))
  (technetium :initform (make-stat :base 0 :min 0 :unit :ug))
  (biosilicate :initform (make-stat :base 0 :min 0 :unit :g))
  (hit-points :initform (make-stat :base 28 :min 0 :max 28))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 20))
  (oxygen :initform (make-stat :base 200 :min 0 :max 200))
  (invincibility-clock :initform 0)
  (stepping :initform t)
  (experience-points :initform 0)
  (attacking-with :initform :right-hand)
  (firing-with :initform :left-hand)
  (light-radius :initform 1)
  (categories :initform '(:actor :player :target :container :light-source)))

(defparameter *pain-sounds* '("unh1" "unh2" "unh3" "oghh"))

(define-method damage contractor (points)
  [play-sample self (nth (random (length *pain-sounds*)) *pain-sounds*)]
  [parent>>damage self points])

(define-method activate-extension contractor ()
  (if [equipment-slot self :extension]
      [>>activate [equipment-slot self :extension]]
      [>>say :narrator "No extension part equipped."]))

(define-method enter contractor ()
  (let ((gateway [category-at-p *world* <row> <column> :gateway]))
    (if (null gateway)
	[>>say :narrator "No gateway to enter."]
	[activate gateway])))

(define-method loadout contractor ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =wrench=)]])

(define-method wait contractor ()
  [say self "Skipped one turn."]
  (when (not [in-category self :proxied])
    [stat-effect self :oxygen -1])
  [expend-action-points self <action-points>])

(define-method quit contractor ()
  (xe2:quit :shutdown))

(defparameter *contractor-oxygen-warning-level* 20)
(defparameter *contractor-low-hp-warning-level* 10)

(define-method phase-hook contractor ()
  ;; possibly breathe
  (when (and [in-category *world* :airless]
	     (null <proxy>)
	     (has-field :oxygen self))
    [stat-effect self :oxygen -1]
    [say self "You expend one unit of oxygen."])) 

(define-method run contractor ()
  ;; check stats
  (when (< [stat-value self :oxygen] *contractor-oxygen-warning-level*)
    [>>println :narrator "WARNING: OXYGEN SUPPLY CRITICAL!" :foreground ".yellow" :background ".red"]
    [play-sample self (if (= 0 (random 2))
			  "breath1" "breath2")])
  (when (< [stat-value self :hit-points] *contractor-low-hp-warning-level*)
    [>>println :narrator "WARNING: LOW HIT POINTS!" :foreground ".yellow" :background ".red"])
  (cond ((<= [stat-value self :oxygen] 0)
	 [>>say :narrator "Your oxygen runs out, suffocating you."]
	 [die self])
	((<= [stat-value self :speed] 0)
	 [>>say :narrator "You are paralyzed. You suffocate and die."]
	 [die self]))
  [update *dude-status*])
  
(define-method die contractor ()
  [play-sample self "death"]
  (let ((skull (clone =skull= self)))
    [drop-cell *world* skull <row> <column> :loadout t :no-collisions nil]
    [set-player *world* skull]
    [parent>>die self]))
  ;; (let ((textbox (clone =textbox=)))
  ;;   [resize textbox :height 400 :width 200]
  ;;   [auto-center textbox]
  ;;   [set-buffer textbox '("You are dead! Press RET to close.")]))

(define-method embark contractor ()
  (let ((vehicle [category-at-p *world* <row> <column> :vehicle]))
    (when vehicle
      [parent>>embark self]
      [set-character *ship-status* vehicle]
      ;; refill oxygen
      [play-sample vehicle "swoosh"]
      [say vehicle "Suit oxygen refilled to maximum."]
      [stat-effect self :oxygen [stat-value self :oxygen :max]])))

(define-method do-post-unproxied contractor ()
  [say self "Commencing EVA (extra-vehicular activity)."]
  [say self "Use ALT-<direction> to melee attack with the wrench."])

(define-method attack contractor (target)
  (xe2:play-sample "knock")
  [parent>>attack self target]
  (when [in-category *world* :airless]
    [stat-effect self :oxygen -2]))

(define-method move contractor (direction)
  (if (member <mode> (field-value :required-modes *world*))
      (if [in-category *world* :weightless] 
	  (if (and (clon:object-p [equipment-slot self :feet])
		   [in-category [equipment-slot self :feet] :gravboots])
	      (if [category-in-direction-p *world* 
					   <row> <column> direction 
					   :magnetic]
		  [parent>>move self direction]
		  [say self "Your magnetic boots won't stick to that location."])
	      [say self "You cannot move in zero-gravity without magnetic boots."])
	  [parent>>move self direction])
      [say self "You cannot do EVA (Extra-Vehicular Activity) in this environment."]))

(define-method show-location contractor ()
  (labels ((do-circle (image)
	     (multiple-value-bind (x y) 
		 [viewport-coordinates self]
	       (draw-circle x y 30 :destination image)
	       (draw-circle x y 25 :destination image))))
    [>>add-overlay :viewport #'do-circle]))

;;; Ship repair modules

(defcell repair-module
  (name :initform "Nanorepair module")
  (tile :initform "repair")
  (description :initform "This nanorepair module can restore hit points to machines and other repairable items."))

(define-method step repair-module (stepper)
  (when [is-player stepper]
    (if (member (field-value :mode stepper) '(:vehicle :vomac))
	(progn 
	  [play-sample self "technetium-sound"]
	  [stat-effect stepper :hit-points 5]
	  [say self "The nanorepair module fixed some damage to your vehicle."]
	  [die self])
	[say self "You cannot use nano-repairs on your current transport mode."])))


;;; Your ship.

(defcell olvac 
  (tile :initform "voidrider-north")
  (mode :initform :vehicle)
  (name :initform "Olvac 3")
  (last-direction :initform :here)
  (speed :initform (make-stat :base 10 :min 0 :max 25))
  (crystals :initform (make-stat :base 0 :min 0))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 15)
  (energy :initform (make-stat :base 40 :min 0 :max 40 :unit :gj))
  (pollen3a :initform (make-stat :base 0 :min 0 :max 10 :unit :kg))
  (endurium :initform (make-stat :base 200 :min 0 :max 200 :unit :kg))
  (technetium :initform (make-stat :base 0 :min 0 :unit :ug))
  (biosilicate :initform (make-stat :base 0 :min 0 :unit :g))
  (hit-points :initform (make-stat :base 45 :min 0 :max 45))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (trail-length :initform (make-stat :base 12 :min 0))
  (pulse-ammo :initform (make-stat :base 4 :min 0 :max 4))
  (bomb-ammo :initform (make-stat :base 10 :min 0 :max 10))
  (oxygen :initform (make-stat :base 200 :min 0 :max 200))
  (invincibility-clock :initform 0)
  (stepping :initform t)
  (attacking-with :initform nil)
  (firing-with :initform :center-bay)
  (categories :initform '(:actor :player :target :container :light-source :vehicle :repairable))
  (equipment-slots :initform '(:left-bay :right-bay :center-bay :extension))
  (boost-clock :initform 0)
  (description :initform 
"The General Products OLVAC 3 Void Rider is a fine complement to any
contractor's arsenal. Equipped with a basic muon cannon and bomb
capability, the dependable Olvac covers the basics and gets the job
done."))
      
(define-method is-disabled olvac ()
  (< [stat-value self :hit-points]
     6))

(define-method loadout olvac ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =muon-cannon=)]]
  [equip self [add-item self (clone =pulse-cannon=)]]
  [equip self [add-item self (clone =bomb-cannon=)]])

(define-method disembark olvac ()
  (if (null [in-category self :proxied])
      (progn 
	[unproxy self]
	[set-character *ship-status* nil])
      [>>say :narrator "Cannot disembark without a vehicle."]))

(define-method run olvac ()
  (when <occupant> 
    [run <occupant>])
    ;;(setf (field-value :phase-number <occupant>) <phase-number>))
  (cond ((<= [stat-value self :hit-points] 0)
	 [die self])
	((<= [stat-value self :endurium] 0)
	 [say self "You run out of endurium in the deeps of interstellar space."]
	 [say self "Your oxygen runs out, suffocating you."]
	 [die self])
	((<= [stat-value self :oxygen] 0)
	 (progn [say self "Your oxygen runs out, suffocating you."]
		[die self]))
	((<= [stat-value self :speed] 0)
	 (progn [say self "You are paralyzed. You suffocate and die."]
		[die self]))
	([in-category self :toxic]
	 [say self "You suffer toxic hazard damage. You must find a tox hypo."]
	 [damage self 1])
	(t 
	 (when <occupant>
	   (when (zerop [stat-value <occupant> :hit-points])
	     [>>narrateln :narrator "PILOT KILL! YOU DIE." :foreground ".yellow" :background ".red"]
	     [die self]))
	 [update-tile self]
	 [update-react-shield self]
	 [update *ship-status*]
	 [update *dude-status*])))

(define-method wait olvac ()
  [say self "Skipped one turn."]
  [expend-action-points self <action-points>])

(define-method activate-pulse-cannon olvac ()
  (when (plusp [stat-value self :pulse-ammo])
    [activate [equipment-slot self :left-bay]]
    [stat-effect self :pulse-ammo -1]))

(define-method activate-bomb-cannon olvac ()
  (when (plusp [stat-value self :bomb-ammo])
    [activate [equipment-slot self :right-bay]]
    [stat-effect self :bomb-ammo -1]))

(define-method activate-extension olvac ()
  (if [equipment-slot self :extension]
      [>>activate [equipment-slot self :extension]]
      [>>say :narrator "No extension part equipped."]))

(define-method update-react-shield olvac ()
  (when (not (<= <invincibility-clock> 0))
    (decf <invincibility-clock>)))
;;    [>>say :narrator "React shield up with ~D turns remaining." <invincibility-clock>]))

(define-method drop-trail olvac ()
  [drop self (clone =trail= 
		    :direction <last-direction> 
		    :clock [stat-value self :trail-length])])

(define-method move olvac (direction)
  (if [is-disabled self]
      (progn
	[say self "Your vehicle is disabled, and cannot move."]
	[say self "Press Control-P to disembark, but this may kill you."])
      (let ((modes (field-value :required-modes *world*)))
	(if (and modes (not (member <mode> modes)))
	    (progn 
	      [say self "Your vehicle is docked, and cannot move."]
	      [say self "Press Control-P to disembark, or RETURN to launch."])
	    (progn 
	      (setf <last-direction> direction)
	      [update-react-shield self]
	      [drop-trail self]
	      [parent>>move self direction]
	      [update-tile self]
	      [update *ship-status*])))))

(defvar *olvac-tiles* '(:north "voidrider-north" 
			:east "voidrider-east" 
		       :south "voidrider-south" 
		       :west "voidrider-west" 
		       :northeast "voidrider-northeast"
		       :southeast "voidrider-southeast"
		       :southwest "voidrider-southwest"
		       :northwest "voidrider-northwest"))

(define-method update-tile olvac ()
  (setf <tile> 
	(if [is-disabled self]
	    "voidrider-disabled"
	    (getf *olvac-tiles* <last-direction> "voidrider-north"))))

(define-method scan-terrain olvac ()
  [cells-at *world* <row> <column>])
		 
(defparameter *piloting-skill-die* 20)

(define-method damage olvac (points)
  (let ((was-disabled [is-disabled self]))
    (if (= 0 <invincibility-clock>)
	(progn (when <occupant>
		 [play-sample self "warn"])
	       [stat-effect self :hit-points (- points)]
	       ;; possibly damage pilot
	       (let ((occupant <occupant>))
		 (when occupant 
		   (percent-of-time (* 5 [stat-value occupant :dexterity])
		     (let ((piloting [stat-value occupant :piloting]))
		       (unless (roll-under piloting *piloting-skill-die*)
			 (let ((amount (max 1 (truncate (/ *piloting-skill-die* (/ piloting 2))))))
			   [damage occupant amount]
			   [expend-action-points self <action-points>]
			   [>>println :narrator (format nil "WARNING: PILOT INJURY OF ~A HIT POINTS" amount)
				      :foreground ".red"]))))))
	       (when (and (null was-disabled)
			  [is-disabled self])
		 [do-disable self]
		 (when (= 0 [stat-value self :hit-points])
		   [die self]))
	       (setf <invincibility-clock> 5)
	       [update-tile self]
	       [>>say :narrator "React Shield up with 5 turns remaining."])
	(progn 
	  [>>say :narrator "React shield blocks 1 damage."]
	  (decf <invincibility-clock>)
	  [update-tile self]))))
  
(define-method do-disable olvac ()
  [play-sample self "powerdown"]
  (when (and <occupant> [is-player <occupant>])
    [>>say :narrator "Your Olvac-3 Void Rider is now disabled."]))

(define-method step olvac (stepper)
  (when (eq =asteroid= (object-parent stepper))
    [damage self 1]
    [>>say :narrator "You were damaged by a floating asteroid!"]))

(define-method die olvac ()
  (let ((occupant <occupant>))
    (if (and occupant [is-player occupant])
	(progn 
	  [disembark self]
	  [die occupant])
	(progn 
	  [drop self (clone =explosion=)]
	  [>>delete-from-world self]))))

  ;; [play-sample self "death"]
  ;; (let ((skull (clone =skull= self)))
  ;;   [drop-cell *world* skull <row> <column> :loadout t :no-collisions nil]
  ;;   (setf <action-points> 0)
  ;;   [add-category self :dead]
  ;;   [>>delete-from-world self]
  ;;   [set-player *world* skull]))

(define-method show-location olvac ()
  (labels ((do-circle (image)
	     (multiple-value-bind (x y) 
		 [viewport-coordinates self]
	       (draw-circle x y 30 :destination image)
	       (draw-circle x y 25 :destination image))))
    [>>add-overlay :viewport #'do-circle]))

(define-method revive olvac ()
  [drop-cell *world* self (random 10) (random 10)]
  [stat-effect self :hit-points 20]	       
  [stat-effect self :energy 40]	       
  [update-tile self]
  [delete-category self :dead]
;;  [stat-effect self :trail-length (- (1+ [stat-value self :trail-length]))]
  [set-player *world* self])

(define-method start olvac ()
  [update-tile self])

(define-method restart olvac ()
  nil)

(define-method enter olvac()
  (let ((gateway [category-at-p *world* <row> <column> :gateway]))
    (if (null gateway)
	[>>say :narrator "No gateway to enter."]
	[activate gateway])))

