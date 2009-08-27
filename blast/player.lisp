(in-package :blast)

;;; The rusty wrench; basic melee weapon

(define-prototype wrench (:parent rlx:=cell=)
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

;;; A melee weapon: the Shock Probe

(defcell shock-probe 
  (name :initform "Shock probe")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "shock-probe")
  (attack-power :initform (make-stat :base 5))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (stepping :initform t)
  (weight :initform 3000)
  (equip-for :initform '(:robotic-arm :left-hand :right-hand)))

(define-prototype shock-prod (:parent =shock-probe=)
  (name :initform "Shock prod")
  (attack-power :initform (make-stat :base 7))
  (attack-cost :initform (make-stat :base 12))
  (accuracy :initform (make-stat :base 80)))

;;; A bomb with countdown display.

(defvar *bomb-tiles* '("bomb-1" "bomb-2" "bomb-3" "bomb-4"))

(defun bomb-tile (n)
  (nth (- n 1) *bomb-tiles*))

(defcell bomb 
  (categories :initform '(:actor))
  (clock :initform 4)
  (speed :initform (make-stat :base 10))
  (tile :initform (bomb-tile 4)))

(define-method run bomb () 
  (clon:with-fields (clock) self	       
    (if (zerop clock) 
	[explode self]
	(progn 
	  [expend-action-points self 10]		    
	  (setf <tile> (bomb-tile clock))
	  (decf clock)))))

(define-method explode bomb ()  
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
    [die self]))

;;; Bomb bay cannon.

(defcell bomb-cannon
  (categories :initform '(:item :weapon :equipment))
  (attack-cost :initform (make-stat :base 5))
  (weight :initform 3000)
  (equip-for :initform '(:right-bay :robotic-arm)))

(define-method activate bomb-cannon ()
  ;; leave bomb on top of ship
  (clon:with-field-values (row column) <equipper>
    [drop-cell *active-world* (clone =bomb=) row column]))

(define-method fire bomb-cannon (direction)
  (clon:with-field-values (last-direction row column) <equipper>
    (multiple-value-bind (r c) 
	(step-in-direction row column direction)
      [drop-cell *active-world* (clone =bomb=) r c])))
  
;;; Your explosive vapor trail. 

(defcell trail 
  (categories :initform '(:actor))
  (clock :initform 4))
  
(define-method initialize trail (&key direction clock)
  (setf <clock> clock)
  (setf <tile> (ecase direction
		 (:north "trail-north")
		 (:south "trail-south")
		 (:east "trail-east")
		 (:west "trail-west")
		 (:northeast "trail-northeast")
		 (:northwest "trail-northwest")
		 (:southeast "trail-southeast")
		 (:southwest "trail-southwest"))))

(define-method run trail ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (< <clock> 0) (setf <clock> 0))
  (when (zerop <clock>)
    [die self]))

(define-method step trail (stepper)
  ;; don't damage player
  (unless [is-player stepper]
    [drop self (clone =explosion=)]	       
    [damage stepper 2]))

;;; Death icon.

(defparameter *death-message* "You are dead.")
(defparameter *game-over-message* "GAME OVER.")

(define-prototype skull (:parent rlx:=cell=)
  (tile :initform "skull")
  (player :initform nil)
  (categories :initform '(:dead :player :actor))
  (action-points :initform 0))

(define-method run skull ()
  (rlx:show-widgets))

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
  (rlx:quit :shutdown))

(define-method initialize skull (player)
  [say self *death-message*]
  [say self *game-over-message*]
  [say self "Press Control-PERIOD (i.e. \".\") to restart."])

(define-method restart skull ()
  (let ((dude (clone =contractor=))
	(ship (clone =olvac=)))
    [destroy *active-universe*]
    [set-character *status* ship]
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
  (rlx:do-cells (cell [cells-at *active-world* <row> <column>])
    (when (not [is-player cell])
      [damage cell 5])))

;;; Muon particles, trails, and pistols

(defvar *muon-tiles* '(:north "muon-north"
		       :south "muon-south"
		       :east "muon-east"
		       :west "muon-west"
		       :northeast "muon-northeast"
		       :southeast "muon-southeast"
		       :southwest "muon-southwest"
		       :northwest "muon-northwest"))

(defvar *trail-middle-tiles* '(:north "bullet-trail-middle-north"
			       :south "bullet-trail-middle-south"
			       :east "bullet-trail-middle-east"
			       :west "bullet-trail-middle-west"
			       :northeast "bullet-trail-middle-northeast"
			       :southeast "bullet-trail-middle-southeast"
			       :southwest "bullet-trail-middle-southwest"
			       :northwest "bullet-trail-middle-northwest"))

(defvar *trail-end-tiles* '(:north "bullet-trail-end-north"
			       :south "bullet-trail-end-south"
			       :east "bullet-trail-end-east"
			       :west "bullet-trail-end-west"
			       :northeast "bullet-trail-end-northeast"
			       :southeast "bullet-trail-end-southeast"
			       :southwest "bullet-trail-end-southwest"
			       :northwest "bullet-trail-end-northwest"))

(defvar *trail-tile-map* (list *trail-end-tiles* *trail-middle-tiles* *trail-middle-tiles*))

(defcell muon-trail
  (categories :initform '(:actor))
  (clock :initform 2)
  (speed :initform (make-stat :base 10))
  (default-cost :initform (make-stat :base 10))
  (tile :initform ".gear")
  (direction :initform :north))

(define-method orient muon-trail (direction)
  (setf <direction> direction)
  (setf <tile> (getf *trail-middle-tiles* direction)))

(define-method run muon-trail ()
  (setf <tile> (getf (nth <clock> *trail-tile-map*)
		     <direction>))
  [expend-default-action-points self]
  (decf <clock>)
  (when (minusp <clock>)
    [die self]))

(defcell muon-particle 
  (categories :initform '(:actor))
  (speed :initform (make-stat :base 20))
  (default-cost :initform (make-stat :base 5))
  (tile :initform "muon")
  (direction :initform :here)
  (clock :initform 12))


(define-method drop-trail muon-particle (direction)
  (let ((trail (clone =muon-trail=)))
    [orient trail direction]
    [drop self trail]))

(define-method find-target muon-particle ()
  (let ((target [category-in-direction-p *active-world* 
					 <row> <column> <direction>
					 '(:obstacle :target)]))
    (if target
	(progn
	  [>>move self <direction>]
	  [>>expend-default-action-points self]
	  [>>drop target (clone =flash=)]
	  [>>damage target 5]
	  [>>die self])
	(multiple-value-bind (r c) 
	    (step-in-direction <row> <column> <direction>)
	  (if (not (array-in-bounds-p (field-value :grid *active-world*) r c))
	      [die self]
	      (progn [drop-trail self <direction>]
		     [>>move self <direction>]))))))

(define-method step muon-particle (stepper)
  [damage stepper 5]
  [die self])
  
(define-method update-tile muon-particle ()
  (setf <tile> (getf *muon-tiles* <direction>)))

(define-method run muon-particle ()
  [update-tile self]
  [find-target self]
  (decf <clock>)
  (when (zerop <clock>)
    [>>die self]))

(define-method impel muon-particle (direction)
  (assert (member direction *compass-directions*))
  (setf <direction> direction)
  ;; don't hit the player
  [move self direction]
  [find-target self])

(defcell muon-cannon
  (name :initform "Muon energy cannon")
  (tile :initform "gun")
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:center-bay))
  (weight :initform 7000)
  (accuracy :initform (make-stat :base 100))
  (attack-power :initform (make-stat :base 12))
  (attack-cost :initform (make-stat :base 10))
  (energy-cost :initform (make-stat :base 1)))

(define-method fire muon-cannon (direction)
  (if [expend-energy <equipper> [stat-value self :energy-cost]]
      (let ((muon (clone =muon-particle=)))
	[play-sample <equipper> "dtmf2"]
	[>>drop <equipper> muon]
	[>>impel muon direction])
      [say self "Not enough energy to fire!"]))

(define-method step muon-cannon (stepper)
  (when [is-player stepper]
    [>>take stepper :direction :here :category :item]))

;;; Pulse wave cannon destroys anything near you.

(defcell pulse-cannon 
  (categories :initform '(:item :weapon :equipment))
  (attack-power :initform (make-stat :base 10))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (weight :initform 3000)
  (equip-for :initform '(:left-bay))
  (tile :initform "pulse-cannon"))

(define-method activate pulse-cannon ()
  (let* ((world *active-world*)
	 (row [player-row world])
	 (column [player-column world]))
    (if (plusp [stat-value <equipper> :pulse-ammo])
	(progn 
	  [say self "Activating pulse cannon."]
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
  (defense :initform (make-stat :base 15))
  (equipment-slots :initform '(:left-hand :right-hand :belt :extension))
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
  (let ((gateway [category-at-p *active-world* <row> <column> :gateway]))
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
  (rlx:quit :shutdown))

(defparameter *contractor-energy-warning-level* 15)

(define-method run contractor ()
  (when (< [stat-value self :oxygen] *contractor-energy-warning-level*)
    [>>println :narrator "OXYGEN SUPPLY CRITICAL!" :foreground ".yellow" :background ".red"]
    [play-sample self (if (= 0 (random 2))
			  "breath1" "breath2")])
  (cond ((not (member :spacesuit (field-value :required-modes *active-world*)))
	 [>>say :narrator "You cannot survive in this environment! You die."]
	 [die self])
	((<= [stat-value self :oxygen] 0)
	 [>>say :narrator "Your oxygen runs out, suffocating you."]
	 [die self])
	((<= [stat-value self :speed] 0)
	 [>>say :narrator "You are paralyzed. You suffocate and die."]
	 [die self])))

(define-method die contractor ()
  [play-sample self "death"]
  (let ((skull (clone =skull= self)))
    [drop-cell *active-world* skull <row> <column> :loadout t :no-collisions nil]
    [set-player *active-world* skull]
    [parent>>die self]))
  ;; (let ((textbox (clone =textbox=)))
  ;;   [resize textbox :height 400 :width 200]
  ;;   [auto-center textbox]
  ;;   [set-buffer textbox '("You are dead! Press RET to close.")]))

(define-method embark contractor ()
  (let ((vehicle [category-at-p *active-world* <row> <column> :vehicle]))
    (when vehicle
      [set-character *status* vehicle]
      [parent>>embark self])))

(define-method do-post-unproxied contractor ()
  [say self "Commencing EVA (extra-vehicular activity)."]
  [say self "Use ALT-<direction> to melee attack with the wrench."])

(define-method attack contractor (target)
  (rlx:play-sample "knock")
  [>>stat-effect self :oxygen -2]
  [parent>>attack self target])

(define-method show-location contractor ()
  (labels ((do-circle (image)
	     (multiple-value-bind (x y) 
		 [screen-coordinates self]
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
  (name :initform "Olvac 2")
  (last-direction :initform :here)
  (speed :initform (make-stat :base 10 :min 0 :max 25))
  (crystals :initform (make-stat :base 0 :min 0))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 15)
  (energy :initform (make-stat :base 40 :min 0 :max 40 :unit :gj))
  (pollen3a :initform (make-stat :base 0 :min 0 :max 10 :unit :kg))
  (endurium :initform (make-stat :base 50 :min 0 :max 100 :unit :kg))
  (technetium :initform (make-stat :base 0 :min 0 :unit :ug))
  (biosilicate :initform (make-stat :base 0 :min 0 :unit :g))
  (hit-points :initform (make-stat :base 45 :min 0 :max 45))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (trail-length :initform (make-stat :base 12 :min 0))
  (pulse-ammo :initform (make-stat :base 5 :min 0 :max 5))
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
  [set-character *status* <occupant>]
  (if (null [in-category self :proxied])
      [unproxy self]
      [>>say :narrator "Cannot disembark without a vehicle."]))

(define-method run olvac ()
  ;; (when <occupant> 
  ;;   (setf (field-value :phase-number <occupant>) <phase-number>))
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
	 [update-tile self]
	 [update-react-shield self]
	 [update *status*])))

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
      (let ((modes (field-value :required-modes *active-world*)))
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
	      [update *status*])))))

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
  [cells-at *active-world* <row> <column>])
		 
(define-method damage olvac (points)
  (let ((was-disabled [is-disabled self]))
    (if (= 0 <invincibility-clock>)
	(progn [play-sample self "warn"]
	       [stat-effect self :hit-points (- points)]
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
  ;;   [drop-cell *active-world* skull <row> <column> :loadout t :no-collisions nil]
  ;;   (setf <action-points> 0)
  ;;   [add-category self :dead]
  ;;   [>>delete-from-world self]
  ;;   [set-player *active-world* skull]))

(define-method show-location olvac ()
  (labels ((do-circle (image)
	     (multiple-value-bind (x y) 
		 [screen-coordinates self]
	       (draw-circle x y 30 :destination image)
	       (draw-circle x y 25 :destination image))))
    [>>add-overlay :viewport #'do-circle]))

(define-method revive olvac ()
  [drop-cell *active-world* self (random 10) (random 10)]
  [stat-effect self :hit-points 20]	       
  [stat-effect self :energy 40]	       
  [update-tile self]
  [delete-category self :dead]
;;  [stat-effect self :trail-length (- (1+ [stat-value self :trail-length]))]
  [set-player *active-world* self])

(define-method start olvac ()
  [update-tile self])

(define-method restart olvac ()
  nil)

(define-method enter olvac()
  (let ((gateway [category-at-p *active-world* <row> <column> :gateway]))
    (if (null gateway)
	[>>say :narrator "No gateway to enter."]
	[activate gateway])))

;;; Lepton Seeker Cannon

(defvar *lepton-tiles* '(:north "lepton-north"
		       :south "lepton-south"
		       :east "lepton-east"
		       :west "lepton-west"
		       :northeast "lepton-northeast"
		       :southeast "lepton-southeast"
		       :southwest "lepton-southwest"
		       :northwest "lepton-northwest"))

(defvar *lepton-trail-middle-tiles* '(:north "bullet-trail-middle-thin-north"
			       :south "bullet-trail-middle-thin-south"
			       :east "bullet-trail-middle-thin-east"
			       :west "bullet-trail-middle-thin-west"
			       :northeast "bullet-trail-middle-thin-northeast"
			       :southeast "bullet-trail-middle-thin-southeast"
			       :southwest "bullet-trail-middle-thin-southwest"
			       :northwest "bullet-trail-middle-thin-northwest"))

(defvar *lepton-trail-end-tiles* '(:north "bullet-trail-end-thin-north"
			       :south "bullet-trail-end-thin-south"
			       :east "bullet-trail-end-thin-east"
			       :west "bullet-trail-end-thin-west"
			       :northeast "bullet-trail-end-thin-northeast"
			       :southeast "bullet-trail-end-thin-southeast"
			       :southwest "bullet-trail-end-thin-southwest"
			       :northwest "bullet-trail-end-thin-northwest"))

(defvar *lepton-trail-tile-map* (list *lepton-trail-end-tiles* *lepton-trail-middle-tiles* *lepton-trail-middle-tiles*))

(define-prototype lepton-trail (:parent rlx:=cell=)
  (categories :initform '(:actor))
  (clock :initform 2)
  (speed :initform (make-stat :base 10))
  (default-cost :initform (make-stat :base 10))
  (tile :initform ".gear")
  (direction :initform :north))

(define-method initialize lepton-trail (direction)
  (setf <direction> direction)
  (setf <tile> (getf *lepton-trail-middle-tiles* direction)))

(define-method run lepton-trail ()
  (setf <tile> (getf (nth <clock> *lepton-trail-tile-map*)
		     <direction>))
  [expend-default-action-points self]
  (decf <clock>)
  (when (minusp <clock>)
    [die self]))

(define-prototype lepton-particle (:parent rlx:=cell=)
  (categories :initform '(:actor :target))
  (speed :initform (make-stat :base 14))
  (stepping :initform t)
  (hit-damage :initform (make-stat :base 7))
  (default-cost :initform (make-stat :base 2))
  (hit-points :initform (make-stat :base 5))
  (movement-cost :initform (make-stat :base 4))
  (tile :initform "lepton")
  (direction :initform :here)
  (clock :initform 10))

(define-method find-target lepton-particle ()
  (let ((target [category-in-direction-p *active-world* 
					 <row> <column> <direction>
					 '(:obstacle :target)]))
    (if target
	(progn	
	  [>>drop target (clone =flash=)]
	  [>>damage target [stat-value self :hit-damage]]
	  [>>die self])
	(progn 
	  [>>drop self (clone =lepton-trail= <direction>)]
	  [>>move self <direction>]))))

(define-method update-tile lepton-particle ()
  (setf <tile> (getf *lepton-tiles* <direction>)))
  
(define-method run lepton-particle ()
  [update-tile self]
  (clon:with-field-values (row column) self
    (let* ((world *active-world*)
	   (direction [direction-to-player *active-world* row column]))
      (setf <direction> direction)
      [find-target self])
    (decf <clock>)
    (when (and (zerop <clock>) 
	       (not [in-category self :dead]))
      [>>die self])))

(define-method damage lepton-particle (points)
  (declare (ignore points))
  [drop self (clone =sparkle=)]
  [die self])
      
(define-method impel lepton-particle (direction)
  (assert (member direction *compass-directions*))
  (setf <direction> direction)
  ;; don't hit the player
  [find-target self])

(define-prototype lepton-cannon (:parent rlx:=cell=)
  (name :initform "Xiong Les Fleurs Lepton(TM) energy cannon")
  (tile :initform "lepton-cannon")
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:robotic-arm))
  (weight :initform 14000)
  (accuracy :initform (make-stat :base 60))
  (attack-power :initform (make-stat :base 16))
  (attack-cost :initform (make-stat :base 25))
  (energy-cost :initform (make-stat :base 32)))

(define-method fire lepton-cannon (direction)
  (if [expend-energy <equipper> [stat-value self :energy-cost]]
      (let ((lepton (clone =lepton-particle=)))
	[play-sample <equipper> "bloup"]
	[>>drop <equipper> lepton]
	[>>impel lepton direction]
	[expend-action-points <equipper> [stat-value self :attack-cost]]
      (message "Not enough energy to fire."))))

;;; An exploding missile.

(defvar *missile-trail-tile-map* (list *lepton-trail-end-tiles* *lepton-trail-middle-tiles* *lepton-trail-middle-tiles*))

(defvar *missile-tiles* '(:north "missile-north"
		       :south "missile-south"
		       :east "missile-east"
		       :west "missile-west"
		       :northeast "missile-northeast"
		       :southeast "missile-southeast"
		       :southwest "missile-southwest"
		       :northwest "missile-northwest"))

(define-prototype missile (:parent =lepton-particle=)
  (speed :initform (make-stat :base 25))
  (hit-damage :initform (make-stat :base 10))
  (hit-points :initform (make-stat :base 10))
  (tile :initform "missile-north")
  (clock :initform 20))

(define-method update-tile missile ()
  (setf <tile> (or (getf *missile-tiles* <direction>)
		   "missile-north")))

(define-method die missile ()
  [drop self (clone =explosion=)]
  [parent>>die self])

;;; Multi-warhead missile

(defvar *multi-missile-tiles* '(:north "multi-missile-north"
		       :south "multi-missile-south"
		       :east "multi-missile-east"
		       :west "multi-missile-west"
		       :northeast "multi-missile-northeast"
		       :southeast "multi-missile-southeast"
		       :southwest "multi-missile-southwest"
		       :northwest "multi-missile-northwest"))

(define-prototype multi-missile (:parent =missile=)
  (tile :initform "multi-missile-north")
  (clock :initform 12)
  (hit-damage :initform (make-stat :base 18))
  (hit-points :initform (make-stat :base 20)))

(define-method update-tile multi-missile ()
  (setf <tile> (or (getf *multi-missile-tiles* <direction>)
		   "multi-missile-north")))

(define-method run multi-missile ()
  [update-tile self]
  (if (or (= 0 <clock>)
	  (> 7 [distance-to-player self]))
      ;; release warheads
      (progn 
	(dolist (dir (list :northeast :southeast :northwest :southwest))
	  (multiple-value-bind (r c) 
	      (step-in-direction <row> <column> dir)
	    [drop-cell *active-world* (clone =missile=) r c]))
	[die self])
      ;; move toward player
      (progn (decf <clock>)
	     [parent>>run self])))

(define-method die multi-missile ()
  [drop self (clone =flash=)]
  [parent>>die self])
  
;;; Missile launchers

(define-prototype missile-launcher (:parent =lepton-cannon=)
  (ammo :initform =missile=)
  (attack-cost :initform (make-stat :base 20)))

(define-method fire missile-launcher (direction)
  (let ((missile (clone <ammo>)))
    [play-sample <equipper> "bloup"]
    [>>drop <equipper> missile]
    [>>impel missile direction]
    [expend-action-points <equipper> [stat-value self :attack-cost]]))

(define-prototype multi-missile-launcher (:parent =missile-launcher=)
  (ammo :initform =multi-missile=)
  (attack-cost :initform (make-stat :base 80)))

