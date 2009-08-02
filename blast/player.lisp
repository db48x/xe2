(in-package :blast)

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
  ;; leave bomb behind ship
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

(define-method forward skull (&rest args)
  (declare (ignore args))
  [queue>>narrateln :narrator *death-message*])
  
(define-method move skull (&rest args)
  (declare (ignore args))
 [queue>>narrateln :narrator *death-message*])

(define-method attack skull (&rest args)
  (declare (ignore args))
 [queue>>narrateln :narrator *death-message*])

(define-method fire skull (&rest args)
  (declare (ignore args))
 [queue>>narrateln :narrator *death-message*])

(define-method quit skull ()
  (rlx:quit :shutdown))

(define-method initialize skull (player)
  [>>say :narrator *death-message*]
  [>>say :narrator *game-over-message*]
  [>>say :narrator "Press Control-BACKSLASH to restart."])

(define-method restart skull ()
  (let ((new-player (clone =ship=)))
    [set-character *status* new-player]
    [play-sample self "go"]
    [play *active-universe* 
	  :player new-player
	  :address '(=star-sector= :width 80 :height 80 :stars 80 :freighters 6)]
    [loadout new-player]))

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

(define-method initialize muon-trail (direction)
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
  (speed :initform (make-stat :base 15))
  (default-cost :initform (make-stat :base 5))
  (tile :initform "muon")
  (direction :initform :here)
  (clock :initform 12))

(define-method find-target muon-particle ()
  (let ((target [category-in-direction-p *active-world* 
					 <row> <column> <direction>
					 '(:obstacle :target)]))
    (if target
	(progn
	  [queue>>move self <direction>]
	  [queue>>expend-default-action-points self]
	  [queue>>drop target (clone =flash=)]
	  [queue>>damage target 5]
	  [queue>>die self])
	(progn 
	  [queue>>drop self (clone =muon-trail= <direction>)]
	  [queue>>move self <direction>]))))
  
(define-method run muon-particle ()
  (setf <tile> (getf *muon-tiles* <direction>))
  [find-target self]
  (decf <clock>)
  (when (zerop <clock>)
    [queue>>die self]))

(define-method impel muon-particle (direction)
  (assert (member direction *compass-directions*))
  (setf <direction> direction)
  ;; don't hit the player
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
      [>>say :narrator "Not enough energy to fire!"]))

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
	  [>>say :narrator "Activating pulse cannon."]
	  (labels ((drop-pulse (r c)
		     (prog1 nil 
		       [drop-cell world (clone =pulse=) r c])))
	    (trace-rectangle #'drop-pulse (- row 2) (- column 2)
			     5 5 :fill)))
	[>>say :narrator "Out of pulse ammo."])))

;;; Your ship.

(defcell ship 
  (tile :initform "player-ship-north-shield")
  (name :initform "Olvac 2")
  (last-direction :initform :here)
  (speed :initform (make-stat :base 10 :min 0 :max 25))
  (crystals :initform (make-stat :base 0 :min 0))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 15)
  (energy :initform (make-stat :base 40 :min 0 :max 40 :unit :gj))
  (endurium :initform (make-stat :base 50 :min 0 :max 100 :unit :kg))
  (credits :initform (make-stat :base 512 :min 0 :unit :cr))
  (technetium :initform (make-stat :base 0 :min 0 :unit :ug))
  (biosilicate :initform (make-stat :base 0 :min 0 :unit :g))
  (hit-points :initform (make-stat :base 35 :min 0 :max 35))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (trail-length :initform (make-stat :base 12 :min 0))
  (pulse-ammo :initform (make-stat :base 5 :min 0 :max 5))
  (bomb-ammo :initform (make-stat :base 10 :min 0 :max 10))
  (oxygen :initform (make-stat :base 200 :min 0 :max 200))
  (invincibility-clock :initform 0)
  (stepping :initform t)
  (score :initform (make-stat :base 0))
  (attacking-with :initform :right-bay)
  (firing-with :initform :center-bay)
  (categories :initform '(:actor :player :target :container :light-source))
  (equipment-slots :initform '(:left-bay :right-bay :center-bay :extension))
  (boost-clock :initform 0))

(define-method loadout ship ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =muon-cannon=)]]
  [equip self [add-item self (clone =pulse-cannon=)]]
  [equip self [add-item self (clone =bomb-cannon=)]])

(define-method quit ship ()
  (rlx:quit :shutdown))

(define-method run ship ()
  (cond ((<= [stat-value self :endurium] 0)
	 [>>say :narrator "You run out of endurium in the deeps of interstellar space."]
	 [>>say :narrator "Your oxygen runs out, suffocating you."]
	 [die self])
	((<= [stat-value self :oxygen] 0)
	 (progn [>>narrateln :narrator "Your oxygen runs out, suffocating you."]
		[die self]))
	((<= [stat-value self :speed] 0)
	 (progn [>>narrateln :narrator "You are paralyzed. You suffocate and die."]
		[die self]))
	(t 
	 [update-tile self]
	 [update-react-shield self]
	 [update *status*])))

(define-method wait ship ()
  [expend-action-points self <action-points>])

(define-method activate-pulse-cannon ship ()
  (when (plusp [stat-value self :pulse-ammo])
    [activate [equipment-slot self :left-bay]]
    [stat-effect self :pulse-ammo -1]))

(define-method activate-bomb-cannon ship ()
  (when (plusp [stat-value self :bomb-ammo])
    [activate [equipment-slot self :right-bay]]
    [stat-effect self :bomb-ammo -1]))

(define-method activate-extension ship ()
  (if [equipment-slot self :extension]
      [>>activate [equipment-slot self :extension]]
      [>>say :narrator "No extension part equipped."]))

(define-method update-react-shield ship ()
  (when (not (<= <invincibility-clock> 0))
    (decf <invincibility-clock>)
    [>>say :narrator "React shield up with ~D turns remaining." <invincibility-clock>]))

(define-method move ship (direction)
  (setf <last-direction> direction)
  [update-react-shield self]
  [drop self (clone =trail= 
		    :direction direction 
		    :clock [stat-value self :trail-length])]
  [parent>>move self direction]
  [update-tile self]
  [update *status*])

(define-method update-tile ship ()
  (setf <tile> 
	(if  (plusp <invincibility-clock>)
	     "player-ship-invincible"
	     (if (> 5 [stat-value self :hit-points])
		 "player-ship-north-dying"
		 "player-ship-north-shield"))))
		 
(define-method damage ship (points)
  (if (= 0 <invincibility-clock>)
    (progn [play-sample self "warn"]
	   [parent>>damage self points]
	   (setf <invincibility-clock> 5)
	   [update-tile self]
	   [>>say :narrator "React Shield up with 5 turns remaining."])
    (progn 
      [>>say :narrator "React shield blocks 1 damage."]
      (decf <invincibility-clock>)
      [update-tile self])))
  
(define-method step ship (stepper)
  (when (eq =asteroid= (object-parent stepper))
    [damage self 1]
    [>>say :narrator "You were damaged by a floating asteroid!"]))

(define-method die ship ()
  [play-sample self "death"]
  (let ((skull (clone =skull= self)))
    [drop-cell *active-world* skull <row> <column> :loadout t :no-collisions nil]
    (setf <action-points> 0)
    [add-category self :dead]
    [>>delete-from-world self]
    [set-player *active-world* skull]))

(define-method enter ship ()
  (let ((gateway [category-at-p *active-world* <row> <column> :gateway]))
    (if (null gateway)
	[>>narrate :narrator "No gateway to enter."]
	[activate gateway])))

(define-method show-location ship ()
  (labels ((do-circle (image)
	     (multiple-value-bind (x y) 
		 [screen-coordinates self]
	       (draw-circle x y 30 :destination image)
	       (draw-circle x y 25 :destination image))))
    [>>add-overlay :viewport #'do-circle]))

(define-method revive ship ()
  [drop-cell *active-world* self (random 10) (random 10)]
  [stat-effect self :hit-points 20]	       
  [stat-effect self :energy 40]	       
  [update-tile self]
  [delete-category self :dead]
;;  [stat-effect self :trail-length (- (1+ [stat-value self :trail-length]))]
  [set-player *active-world* self])

(define-method restart ship ()
  nil)

;; see also skull restart

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
	  [queue>>drop target (clone =flash=)]
	  [queue>>damage target [stat-value self :hit-damage]]
	  [queue>>die self])
	(progn 
	  [queue>>drop self (clone =lepton-trail= <direction>)]
	  [queue>>move self <direction>]))))
  
(define-method run lepton-particle ()
  (setf <tile> (getf *lepton-tiles* <direction>))
  (clon:with-field-values (row column) self
    (let* ((world *active-world*)
	   (direction [direction-to-player *active-world* row column]))
      (setf <direction> direction)
      [find-target self])
    (decf <clock>)
    (when (and (zerop <clock>) 
	       (not [in-category self :dead]))
      [queue>>die self])))

(define-method damage lepton-particle (points)
  (declare (ignore points))
  [>>drop self (clone =sparkle=)]
  [>>die self])
      
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
	[queue>>drop <equipper> lepton]
	[queue>>impel lepton direction]
	[expend-action-points <equipper> [stat-value self :attack-cost]]
      (message "Not enough energy to fire."))))

