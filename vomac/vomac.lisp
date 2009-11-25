;;; vomac.lisp --- an 80's style scrolling sci-fi shoot em up

;; Copyright (C) 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Packaging

(defpackage :vomac
  (:documentation "VOMAC is a colorful puzzle game in Common Lisp.")
  (:use :rlx :common-lisp)
  (:export vomac))

(in-package :vomac)

;;; Scoring points

(defun score (points)
  [score-points [get-player *active-world*] points])

;;; There are also energy tanks for replenishing ammo.

(defcell energy 
  (tile :initform "energy")
  (description :initform 
"Refills part of your energy store, for energy ammo weapons."))

(define-method step energy (stepper)
  (when [is-player stepper]
    (when (has-field :energy stepper)
      [play-sample self "whoop"]
      [stat-effect stepper :energy 7]
      [die self])))

(defcell energy-tank
  (tile :initform "energy-max-up")
  (description :initform 
"Increases maximum energy store by 5."))

(define-method step energy-tank (stepper)
  (when [is-player stepper]
    (when (has-field :energy stepper)
      [play-sample self "fanfare"]
      [stat-effect stepper :energy 5 :max]
      [>>narrateln :narrator "Increased max energy by 5" :foreground ".yellow" :background ".blue"]
      [die self])))

;;; Stars

(defcell star
  (tile :initform "star")
  (name :initform "Naked star")
  (description :initform "A lonely star."))

(defcell void
  (tile :initform "void")
  (name :initform "Interstellar space")
  (description :initform "The infinite cosmos beckons."))

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
  (attack-distance :initform (make-stat :base 10))
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
    (if (< [distance-to-player self] [stat-value self :attack-distance])
	(let ((direction [direction-to-player self])
	      (world *active-world*))
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
	  (when [obstacle-in-direction-p *active-world* row column (direction-to row column r0 c0)]
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
  (if (< [distance-to-player self] [stat-value self :attack-distance])
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
  (speed :initform (make-stat :base 5))
  (movement-cost :initform (make-stat :base 5))
  (attack-distance :initform (make-stat :base 1))
  (strength :initform (make-stat :base 14))
  (hit-points :initform (make-stat :base 20 :min 0 :max 60)))

(define-method loadout gond ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =multi-missile-launcher=)]])

(define-method home gond ()
  (decf <clock>)
  (clon:with-field-values (row column) self
    (if (< [distance-to-player self] [stat-value self :attack-distance])
	[fire self [direction-to-player self]]
	;; otherwise, move toward the defended cell until clock runs out
	(let* ((cell <defended-cell>)
	       (r0 (field-value :row cell))
	       (c0 (field-value :column cell)))
	  (if [obstacle-in-direction-p *active-world* row column 
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

;;; Basic muon particle

(defcell muon-particle 
  (categories :initform '(:actor :muon :target))
  (speed :initform (make-stat :base 22))
  (default-cost :initform (make-stat :base 3))
  (attack-power :initform 5)
  (tile :initform "muon")
  (firing-sound :initform "dtmf2")
  (direction :initform :here)
  (clock :initform 12))

(define-method initialize muon-particle (&key attack-power)
  (when attack-power
    (setf <attack-power> attack-power)))

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
	  [>>push target <direction>]
	  [>>damage target <attack-power>]
	  [>>die self])
	(multiple-value-bind (r c) 
	    (step-in-direction <row> <column> <direction>)
	  (if (not (array-in-bounds-p (field-value :grid *active-world*) r c))
	      [die self]
	      (progn [drop-trail self <direction>]
		     [>>move self <direction>]))))))

(define-method step muon-particle (stepper)
  [damage stepper <attack-power>]
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
  ;;  [move self direction]
  [play-sample self <firing-sound>]
  [find-target self])

;;; Beta-muons

(define-prototype beta-muon (:parent =muon-particle=)
  (speed :initform (make-stat :base 24))
  (attack-power :initform 8)
  (firing-sound :initform "dtmf3")
  (tile :initform "beta-muon")
  (clock :initform 15))
  
(defvar *beta-muon-tiles* '(:north "beta-muon-north"
			    :south "beta-muon-south"
			    :east "beta-muon-east"
			    :west "beta-muon-west"
			    :northeast "beta-muon-northeast"
			    :southeast "beta-muon-southeast"
			    :southwest "beta-muon-southwest"
			    :northwest "beta-muon-northwest"))

(define-method update-tile beta-muon ()
  (setf <tile> (getf *beta-muon-tiles* <direction>)))

;;; Muon cannon

(defcell muon-cannon
  (name :initform "Muon energy cannon")
  (tile :initform "gun")
  (ammo :initform =muon-particle=)
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:center-bay))
  (weight :initform 7000)
  (accuracy :initform (make-stat :base 100))
  (attack-power :initform (make-stat :base 12))
  (attack-cost :initform (make-stat :base 10))
  (energy-cost :initform (make-stat :base 1)))

(define-method change-ammo muon-cannon (ammo)
  (assert (clon:object-p ammo))
  (setf <ammo> ammo))

(define-method fire muon-cannon (direction)
  (if [expend-energy <equipper> [stat-value self :energy-cost]]
      (let ((bullet (clone <ammo>)))
	[>>drop <equipper> bullet]
	[>>impel bullet direction])
      [say <equipper> "Not enough energy to fire!"]))

(define-method step muon-cannon (stepper)
  (when [is-player stepper]
    [>>take stepper :direction :here :category :item]))

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
  (let* ((world *active-world*)
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

;;; Endurium crystals to collect.

(defcell crystal
  (tile :initform "crystal")
  (categories :initform '(:target :endurium))
  (hit-points :initform (make-stat :base 1 :min 0))
  (description :initform "A one-kilogram chunk of endurium. Highly fragile."))

(define-method step crystal (stepper)
  (when [is-player stepper]
   [play-sample self "bip"]
   [stat-effect stepper :endurium 1]
   [stat-effect stepper :score 1000]
   [die self]))

(defcell big-crystal
  (tile :initform "big-crystal")
  (categories :initform '(:endurium))
  (hit-points :initform (make-stat :base 2 :min 0))
  (description :initform "A massive chunk of endurium. Highly fragile."))

(define-method step big-crystal (stepper)
  (when [is-player stepper]
   [play-sample self "bip"]
   [stat-effect stepper :endurium 10]
   [stat-effect stepper :score 10000]
   [die self]))

(defcell small-crystal 
  (tile :initform "small-crystal")
  (categories :initform '(:endurium))
  (description :initform "Fragments of valuable endurium."))

(define-method step small-crystal (stepper)
  (when [is-player stepper]
   [play-sample self "bip"]
   [stat-effect stepper :endurium 0.3]
   [stat-effect stepper :score 100]
   [die self]))

;;; A trail extender powerup.

(defcell extender 
  (tile :initform "plus")
  (description :initform "Extends Olvac-3 trail."))

(define-method step extender (stepper)
  (when [is-player stepper]
    [play-sample self "powerup"]
    [>>say :narrator "Trail extend!"]
    [stat-effect stepper :trail-length 4]
    [stat-effect stepper :score 2000]
    [die self]))

;;; Death icon.

(defparameter *death-message* "You are dead.")
(defparameter *game-over-message* "GAME OVER.")

(define-prototype skull (:parent rlx:=cell=)
  (tile :initform "skull")
  (name :initform "SKULL")
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
    [set-player *active-universe* ship]
    [play *active-universe*
	  :address '(=star-sector= :width 80 :height 80 
		     :stars 80 :freighters 6 :sequence-number (genseq))]
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

;;; Glittering flash gives clues on locations of explosions/damage

(defcell flash 
  (clock :initform 2)
  (tile :initform "flash-1")
  (categories :initform '(:actor))
  (speed :initform (make-stat :base 10)))

(define-method run flash ()
  [expend-action-points self 10]
  (case <clock>
    (1 (setf <tile> "flash-2"))
    (0 [>>die self]))
  (decf <clock>))

;;; Sparkle is a bigger but faster flash.

(defcell sparkle 
  (clock :initform 1)
  (tile :initform "sparkle")
  (categories :initform '(:actor))
  (speed :initform (make-stat :base 10)))

(define-method run sparkle ()
  [expend-action-points self 10]
  (case <clock>
    (1 (setf <tile> "sparkle"))
    (0 [>>die self]))
  (decf <clock>))

;;; An explosion.

(defcell explosion 
  (name :initform "Explosion")
  (categories :initform '(:actor))
  (tile :initform "explosion")
  (speed :initform (make-stat :base 10))
  (damage-per-turn :initform 5)
  (clock :initform 2))

(define-method run explosion ()
  (if (zerop <clock>)
      [die self]
      (progn
	[play-sample self "crunch"]
	(decf <clock>)
	[expend-action-points self 10]
	(rlx:do-cells (cell [cells-at *active-world* <row> <column>])
	  [damage cell <damage-per-turn>]))))

;;; Your ship.

(defcell olvac 
  (tile :initform "voidrider-north")
  (mode :initform :vehicle)
  (name :initform "Olvac 3")
  (score :initform (make-stat :base 0))
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
  (zerop [stat-value self :hit-points]))

(define-method quit olvac ()
  (rlx:quit :shutdown))

(define-method loadout olvac ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =muon-cannon=)]]
  [equip self [add-item self (clone =pulse-cannon=)]]
  [equip self [add-item self (clone =bomb-cannon=)]])

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
	 [update-react-shield self])))

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
	      [update-tile self])))))


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
		 [viewport-coordinates self]
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

;;; Rooks are the most difficult enemies. They bomb you.

(defcell rook 
  (categories :initform '(:actor :target :obstacle :opaque :enemy))
  (equipment-slots :initform '(:robotic-arm :shoulder-mount))
  (attacking-with :initform :robotic-arm)
  (firing-with :initform :robotic-arm)
  (dexterity :initform (make-stat :base 20))
  (max-items :initform (make-stat :base 1))
  (speed :initform (make-stat :base 12))
  (chase-distance :initform 10)
  (stepping :initform t)
  (behavior :initform :seeking)
  (clock :initform 0)
  (last-direction :initform :north)
  (strength :initform (make-stat :base 50))
  (movement-cost :initform (make-stat :base 8))
  (tile :initform "rook")
  (target :initform nil)
  (hit-points :initform (make-stat :base 40 :min 0 :max 40))
  (description :initform 
"The Rook is a cargo lifter modified to chase and bomb a target.
Hard to kill because of their evasive manuevers.")) 

(define-method run rook ()
  (ecase <behavior>
    (:seeking [seek self])
    (:fleeing [flee self])))

(define-method die rook ()
  [drop self (clone =repair-module=)]
  [play-sample self "blaagh2"]
  [parent>>die self])

(define-method seek rook ()
  (clon:with-field-values (row column) self
    (when (< [distance-to-player *active-world* row column] <chase-distance>)
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

(define-method fire rook (direction)
  [play-sample self "drill-bit"]
  [parent>>fire self direction])

;;; enemy ships a la tac scan

(define-prototype xr7 (:parent =rook=)
  (name :initform "XR-7 Phalanx Interdictor")
  (tile :initform "xr7")
  (speed :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 3))
  (description :initform "The deadly XR7 can fire lasers from a distance."))

(define-method fire xr7 (direction)
  [expend-action-points self 15]
  (let* ((world *active-world*)
	 (player [get-player *active-world*]))
    (labels ((draw-beam (image)
	       (multiple-value-bind (x0 y0) 
		   [viewport-coordinates self]
		 (multiple-value-bind (x1 y1)
		     [viewport-coordinates player]
		   (rlx:draw-line x0 y0 x1 y1 
				  :destination image)))))
      [damage player 2]
      [say self "You sustain 2 damage from the laser."]
      [expend-action-points self 80]
      [play-sample self "laser2"]
      [>>add-overlay :viewport #'draw-beam])))

(define-method die xr7 ()
  [drop self (clone (if (= 0 (random 2))
			=energy= =crystal=))]
  [delete-from-world self])

(define-method seek xr7 ()
  (clon:with-field-values (row column) self
    (if (< [distance-to-player *active-world* row column] <chase-distance>)
	(let ((direction [direction-to-player *active-world* row column])
	      (world *active-world*))
	  (if (< [distance-to-player self] 7)
	      (progn
		[>>fire self direction]
		(setf <clock> 6
		      <behavior> :fleeing))
	      (if [obstacle-in-direction-p world row column direction]
		  (let ((target [target-in-direction-p world row column direction]))
		    (if (and target (not [in-category target :enemy]))
			(progn nil)
			;;		      (progn (setf <direction> (random-direction))
			[>>move self direction]))))
	  (progn (when (< 7 (random 10))
		   (setf <direction> (random-direction)))
		 [>>move self direction])))
	;; otherwise just wait
	[expend-action-points self 5]))

;;; bresenham's rail gun

(defcell rail-trail 
  (tile :initform "ring")
  (categories :initform '(:ephemeral :actor))
  (clock :initform 6))

(define-method run rail-trail ()
  (decf <clock>)
  (when (minusp <clock>)
    [die self]))

(define-prototype rail-particle (:parent =muon-particle=)
  (path :initform nil)
  (clock :initform 60)
  (speed :initform (make-stat :base 6))
  (categories :initform '(:actor :target))
  (tile :initform "rail-particle"))

(define-method orient rail-particle ()
  (if <path>
      (destructuring-bind (row column) (pop <path>)
	(setf <direction> (rlx:direction-to <row> <column> row column)))
      [die self]))

(define-method impel rail-particle (row column)
  (let (path)
    (labels ((collect-point (x y)
	       (prog1 nil (push (list y x) path))))
      ;; (trace-line #'collect-point <row> <column> row column)
      ;; (when (equal (list row column) (first path))
      ;; 	(setf path (nreverse path)))
      ;; (setf <path> path)
      ;; [orient self]
      (setf <direction> (direction-to <row> <column> row column))
      [move self <direction>]
      [drop-trail self nil]
      [find-target self])))

(define-method run rail-particle ()
  [find-target self]
  (decf <clock>)
  (when (zerop <clock>)
    [die self])
  ;; (setf <direction> (direction-to <row> <column> [player-row *active-world*]
  ;; 				  [player-column *active-world*]))
  [drop-trail self nil]
  [move self <direction>])

(define-method drop-trail rail-particle (direction)
  (declare (ignore direction))
  [drop self (clone =rail-trail=)])

(defcell rail-cannon
  (name :initform "Rail gun cannon")
  (tile :initform "gun")
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:center-bay))
  (weight :initform 7000)
  (accuracy :initform (make-stat :base 100))
  (attack-power :initform (make-stat :base 18))
  (attack-cost :initform (make-stat :base 40))
  (energy-cost :initform (make-stat :base 1)))

(define-method fire rail-cannon (row column)
  [expend-action-points <equipper> [stat-value self :attack-cost]]
  (if [expend-energy <equipper> [stat-value self :energy-cost]]
      (let ((particle (clone =rail-particle=)))
	[play-sample <equipper> "bip"]
	[drop <equipper> particle]
	[impel particle row column])
      [say self "Not enough energy to fire!"]))

;;; the eyeboss

(defparameter *guardic-eye-open-time* 5)
(defparameter *guardic-eye-closed-time* 8)

(defcell guardic-eye
  (name :initform "Guardic eye")
  (tile :initform "guardic")
  (hit-points :initform (make-stat :base 4 :max 4 :min 0))
  (open :initform nil)
  (clock :initform (random *guardic-eye-closed-time*))
  (speed :initform (make-stat :base 6))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (energy :initform (make-stat :base 100 :min 0 :max 100))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 6))
  (equipment-slots :initform '(:center-bay))
  (firing-with :initform :center-bay)
  (max-items :initform (make-stat :base 2))
  (categories :initform '(:actor :obstacle :enemy :target))
  (description :initform "Invulnerable until red eye opens. Fires particle weapons."))

(define-method loadout guardic-eye ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =rail-cannon=)]])

(define-method run guardic-eye ()
  ;; open or close eye
  (decf <clock>)
  (if (zerop <clock>)
      (if <open>
	  (progn
	    (setf <open> nil)
	    (setf <tile> "guardic")
	    (setf <clock> *guardic-eye-closed-time*))
	  (progn
	    (setf <open> t)
	    (setf <tile> "guardic-open")
	    (setf <clock> *guardic-eye-open-time*))))
  ;; attack!
  (if (< [distance-to-player self] 15)
      (let ((cannon [equipment-slot self :center-bay]))
	[expend-default-action-points self]
	(when <open> [fire cannon [player-row *active-world*]
			   [player-column *active-world*]]))))
					  
(define-method damage guardic-eye (points)
  ;; only damage when open
  (if <open>
    [parent>>damage self points]
    [say self "Cannot damage closed eye."]))

(defcell guardic 
  (name :initform "Electric eye")
  (tile :initform "godseye"))
  
;;; the bases

(defcell vomac-base 
  (tile :initform "vomac-base")
  (description :initform "Platform for Guardic eye bases."))

(define-method explode vomac-base ()
  [drop self (clone =explosion=)]
  [die self])

(defcell vomac-wires 
  (tile :initform "vomac-wires")
  (description :initform "Deadly live defense wires."))

(define-method step vomac-wires (stepper)
  (when [is-player stepper]
    [play-sample self "spawn"]
    [damage stepper 5]
    [say self "You are shocked by the guard wires!"]))

;;; the vomac ship

(define-prototype vomac (:parent =olvac=)
  (tile :initform "vomac")
  (mode :initform :vehicle)
  (name :initform "Vomac XLUX Fighter")
  (last-direction :initform :here)
  (speed :initform (make-stat :base 9 :min 0 :max 25))
  (strength :initform (make-stat :base 12))
  (defense :initform (make-stat :base 15))
  (hearing-range :initform 15)
  (energy :initform (make-stat :base 70 :min 0 :max 70 :unit :gj))
  (pollen3a :initform (make-stat :base 0 :min 0 :max 30 :unit :kg))
  (endurium :initform (make-stat :base 70 :min 0 :max 140 :unit :kg))
  (technetium :initform (make-stat :base 0 :min 0 :unit :ug))
  (biosilicate :initform (make-stat :base 0 :min 0 :unit :g))
  (hit-points :initform (make-stat :base 70 :min 0 :max 70))
  (movement-cost :initform (make-stat :base 8))
  (max-items :initform (make-stat :base 2))
  (trail-length :initform (make-stat :base 12 :min 0))
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
"The Vomac XLUX Fighter is Arch Gamma's newest mid-range fighter model
with 8-way fire and heavy armor."))

(define-method damage vomac (points)
  [play-sample self "vomac-damage"]
  [parent>>damage self points])

(define-method update-tile vomac ()
  nil)

(define-method drop-trail vomac ()
  nil)

(define-prototype defleptor-trail (:parent =muon-trail=)
  (speed :initform (make-stat :base 20))
  (tile :initform "defleptor-trail")
  (clock :initform 3))

(define-method initialize defleptor-trail ()
  (setf <direction> :north))

(define-method run defleptor-trail ()
  [expend-default-action-points self]
  (decf <clock>)
  (when (minusp <clock>)
    [die self]))

(define-prototype defleptor-wave (:parent =muon-particle=)
  (name :initform "Defleptor wave")
  (speed :initform (make-stat :base 90))
  (tile :initform "defleptorwave")
  (clock :initform 20))

(define-method update-tile defleptor-wave ()
  (setf <tile> (case <direction> 
		 (:north "defleptorwave")
		 (:south "defleptorwave-south")
		 (:west "defleptorwave-west")
		 (:east "defleptorwave-east")
		 (:northeast "defleptorwave-northeast")
		 (:northwest "defleptorwave-northwest")
		 (:southeast "defleptorwave-southeast")
		 (:southwest "defleptorwave-southwest")
		 (otherwise ".gear"))))
  
(define-method drop-trail defleptor-wave (direction)
  (declare (ignore direction))
  [drop self (clone =defleptor-trail=)])

(define-prototype vomac-cannon (:parent =muon-cannon=)
  (name :initform "Vomac defleptor wave cannon")
  (energy-cost :initform (make-stat :base 1))
  (tile :initform "defleptorwave"))

(define-method fire vomac-cannon (direction)
  (if [expend-energy <equipper> [stat-value self :energy-cost]]
      (let (wave)
	(dolist (dir (delete :here rlx:*compass-directions*))
	  (setf wave (clone =defleptor-wave=))
	  [drop <equipper> wave]
	  [play-sample <equipper> "defleptor3"]
	  [impel wave dir])
	[expend-default-action-points self])
      [say <equipper> "Not enough energy to fire!"]))

(define-method loadout vomac ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =vomac-cannon=)]])

;;; The vaxodrones

(defcell vaxodrone 
  (name :initform "VAXodrone")
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
  (equipment-slots :initform '(:robotic-arm))
  (speed :initform (make-stat :base 7 :min 7))
  (max-items :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 14))
  (tile :initform "vaxodrone")
  (stepping :initform t)
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (direction :initform (rlx:random-direction))
  (strength :initform (make-stat :base 4 :min 0 :max 30))
  (dexterity :initform (make-stat :base 5 :min 0 :max 30))
  (intelligence :initform (make-stat :base 11 :min 0 :max 30))
  (hit-points :initform (make-stat :base 10 :min 0 :max 10))
  (description :initform "Swarms of vaxodrones will trap and kill you."))
 
(define-method run vaxodrone ()
  (when (< [distance-to-player self] 18)
    (let ((dir [direction-to-player self]))
      (if (= 0 (random 2))
	  [move self dir]
	  [move self (random-direction)])
      (when [adjacent-to-player self]
	[play-sample self "scree"]
	[expend-action-points self 20]
	[attack self dir]))))

(define-method die vaxodrone ()
  [play-sample self "aagh2"]
  [parent>>die self])

(define-method loadout vaxodrone ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =shock-probe=)]])

;;; Addresses

(defun generate-level-address (n)
  (list '=star-corridor=))

;;; an arena for vomac combat

(defcell vomac-starfield 
  (name :initform "Star corridor")
  (tile :initform "vomac-starfield"))

(defcell vomac-starfield2
  (name :initform "Star corridor with turbulence")
  (tile :initform "vomac-starfield2"))

(define-prototype star-corridor (:parent rlx:=world=)
  (ambient-light :initform :total)
  (required-modes :initform '(:vomac :vehicle :spacesuit))
  (scale :initform '(100 m))
  (edge-condition :initform :block))

(define-method begin-ambient-loop star-corridor ()
  (play-music "saga" :loop t))

(define-method drop-base star-corridor (row column &optional (size 5))
  (labels ((drop-panel (r c)
	     (prog1 nil [drop-cell self (clone =vomac-base=) r c])))
    (trace-rectangle #'drop-panel row column size size :fill)
    (dotimes (i 8)
      [drop-cell self (clone =guardic-eye=) 
		 (+ row (random size)) (+ column (random size)) :loadout t])
    (dotimes (i (* 2 size))
      [drop-cell self (clone =vomac-wires=)
		 (+ row (random size)) (+ column (random size))])))

(define-method generate star-corridor (&key (height 200)
					    (width 30)
					    sequence-number)
  (setf <height> height <width> width)
  [create-default-grid self]
  (dotimes (i height)
    (dotimes (j width)
      [drop-cell self (clone (if (zerop (random 7))
				 =vomac-starfield2= 
				 =vomac-starfield=))
		 i j]))
  (dotimes (i 20)
    [drop-cell self (clone =star=) (random height) (random width)]
    [drop-base self (random height) (random width) (+ 5 (random 10))])
  (dotimes (i 20)
    (let ((gond (clone =gond=))
	  (xr7 (clone =xr7=))
	  (row (random height))
	  (column (random width)))
      [drop-cell self xr7 row column :loadout t]
      [drop-cell self gond (+ 1 row) (+ 1 column) :loadout t]
      (dotimes (i 2)
	[drop-cell self (clone =vaxodrone=) row column])
      [defend gond xr7]))
  [drop-cell self (clone =launchpad=) (- height 8) 5])

;;; Splash screen
  
(defvar *pager* nil)

(define-prototype splash (:parent =widget=))

(define-method render splash ()
  (rlx:draw-resource-image "splash" 0 0 
			   :destination <image>))

(defvar *space-bar-function*)

(define-method dismiss splash ()
  [select *pager* :game]
  (when (functionp *space-bar-function*)
    (funcall *space-bar-function*))
  ;; TODO ugh this is a hack!
  (rlx:show-widgets))

(define-prototype splash-prompt (:parent =prompt=)
  (default-keybindings :initform '(("SPACE" nil "dismiss ."))))

;;; Player status

(defvar *status* nil)

(define-prototype status (:parent rlx:=formatter=)
  (character :documentation "The character cell."))

(define-method set-character status (character)
  (setf <character> character))

(define-method print-stat status (stat-name &key warn-below show-max)
  (let* ((stat (field-value stat-name <character>))
	 (value [stat-value <character> stat-name]))
    (destructuring-bind (&key min max base delta unit) stat
      (let ((color (if (and (numberp warn-below)
			    (< value warn-below))
		       ".red"
		       ".gray40")))
	[print self (symbol-name stat-name)
	       :foreground ".white"]
	[print self ":["]
	[print self (format nil "~S" value) 
	       :foreground ".yellow"
	       :background color]
	(when show-max
	  [print self (format nil "/~S" max)
		 :foreground ".yellow"
		 :background color])
	(when unit 
	  [print self " "]
	  [print self (symbol-name unit)])
	[print self "]"]
	))))

(defparameter *status-bar-character* " ")

(define-method print-stat-bar status (stat &key 
					   (color ".yellow")
					   (background-color ".gray40")
					   (divisor 1))
  (let ((value (truncate (/ [stat-value <character> stat] divisor)))
	(max (truncate (/ [stat-value <character> stat :max] divisor))))
    (dotimes (i max)
      [print self *status-bar-character*
	     :foreground ".yellow"
	     :background (if (< i value)
			     color
			   background-color)])))

(define-method update status ()
  [delete-all-lines self]
  (let* ((char <character>))
    (when char
	[print-stat self :hit-points :warn-below 10 :show-max t]
	[print-stat-bar self :hit-points :color ".red" :divisor 2]
	[print-stat self :energy :warn-below 10 :show-max t]
	[print-stat-bar self :energy :color ".yellow" :divisor 2]

	[print self (format nil "   SCORE:~S" [stat-value char :score])]
	[newline self])))


;;; Custom bordered viewport

(define-prototype view (:parent rlx:=viewport=))

(define-method render view ()
  [parent>>render self]
  (rlx:draw-rectangle 0 0 
		      <width>
		      <height>
		      :color ".blue" :destination <image>))

(defvar *view* (clone =view=))

;;; Controlling the game.

(define-prototype vomac-prompt (:parent rlx:=prompt=))

(defparameter *basic-keybindings* 
  '(("KP7" nil "move :northwest .")
    ("KP8" nil "move :north .")
    ("KP9" nil "move :northeast .")
    ("KP4" nil "move :west .")
    ("KP6" nil "move :east .")
    ("KP1" nil "move :southwest .")
    ("KP2" nil "move :south .")
    ("KP3" nil "move :southeast .")
    ;;
    ("KP7" (:control) "fire :northwest .")
    ("KP8" (:control) "fire :north .")
    ("KP9" (:control) "fire :northeast .")
    ("KP4" (:control) "fire :west .")
    ("KP6" (:control) "fire :east .")
    ("KP1" (:control) "fire :southwest .")
    ("KP2" (:control) "fire :south .")
    ("KP3" (:control) "fire :southeast .")
    ;;
    ("KP7" (:alt) "attack :northwest .")
    ("KP8" (:alt) "attack :north .")
    ("KP9" (:alt) "attack :northeast .")
    ("KP4" (:alt) "attack :west .")
    ("KP6" (:alt) "attack :east .")
    ("KP1" (:alt) "attack :southwest .")
    ("KP2" (:alt) "attack :south .")
    ("KP3" (:alt) "attack :southeast .")
    ;;
    ("KP7" (:meta) "attack :northwest .")
    ("KP8" (:meta) "attack :north .")
    ("KP9" (:meta) "attack :northeast .")
    ("KP4" (:meta) "attack :west .")
    ("KP6" (:meta) "attack :east .")
    ("KP1" (:meta) "attack :southwest .")
    ("KP2" (:meta) "attack :south .")
    ("KP3" (:meta) "attack :southeast .")
    ;;
    ("JOYSTICK" (:north :circle) "attack :north .")
    ("JOYSTICK" (:northeast :circle) "attack :northeast .")
    ("JOYSTICK" (:northwest :circle) "attack :northwest .")
    ("JOYSTICK" (:east :circle) "attack :east .")
    ("JOYSTICK" (:west :circle) "attack :west .")
    ("JOYSTICK" (:south :circle) "attack :south .")
    ("JOYSTICK" (:southwest :circle) "attack :southwest .")
    ("JOYSTICK" (:southeast :circle) "attack :southeast .")
    ;;
    ("JOYSTICK" (:north :cross) "move :north .")
    ("JOYSTICK" (:northeast :cross) "move :northeast .")
    ("JOYSTICK" (:northwest :cross) "move :northwest .")
    ("JOYSTICK" (:east :cross) "move :east .")
    ("JOYSTICK" (:west :cross) "move :west .")
    ("JOYSTICK" (:south :cross) "move :south .")
    ("JOYSTICK" (:southwest :cross) "move :southwest .")
    ("JOYSTICK" (:southeast :cross) "move :southeast .")
    ;;
    ("JOYSTICK" (:north :square) "fire :north .")
    ("JOYSTICK" (:northeast :square) "fire :northeast .")
    ("JOYSTICK" (:northwest :square) "fire :northwest .")
    ("JOYSTICK" (:east :square) "fire :east .")
    ("JOYSTICK" (:west :square) "fire :west .")
    ("JOYSTICK" (:south :square) "fire :south .")
    ("JOYSTICK" (:southwest :square) "fire :southwest .")
    ("JOYSTICK" (:southeast :square) "fire :southeast .")))

(defparameter *qwerty-keybindings*
  (append *basic-keybindings*
	  '(("Y" nil "move :northwest .")
	    ("K" nil "move :north .")
	    ("U" nil "move :northeast .")
	    ("H" nil "move :west .")
	    ("L" nil "move :east .")
	    ("B" nil "move :southwest .")
	    ("J" nil "move :south .")
	    ("N" nil "move :southeast .")
	    ;;
	    ("Y" (:alt) "attack :northwest .")
	    ("K" (:alt) "attack :north .")
	    ("U" (:alt) "attack :northeast .")
	    ("H" (:alt) "attack :west .")
	    ("L" (:alt) "attack :east .")
	    ("B" (:alt) "attack :southwest .")
	    ("J" (:alt) "attack :south .")
	    ("N" (:alt) "attack :southeast .")
	    ;;
	    ("Y" (:meta) "attack :northwest .")
	    ("K" (:meta) "attack :north .")
	    ("U" (:meta) "attack :northeast .")
	    ("H" (:meta) "attack :west .")
	    ("L" (:meta) "attack :east .")
	    ("B" (:meta) "attack :southwest .")
	    ("J" (:meta) "attack :south .")
	    ("N" (:meta) "attack :southeast .")
	    ;;
	    ("Y" (:control) "fire :northwest .")
	    ("K" (:control) "fire :north .")
	    ("U" (:control) "fire :northeast .")
	    ("H" (:control) "fire :west .")
	    ("L" (:control) "fire :east .")
	    ("B" (:control) "fire :southwest .")
	    ("J" (:control) "fire :south .")
	    ("N" (:control) "fire :southeast .")
	    ;;
	    ("W" nil "wait .")
	    ("SPACE" nil "wait .")
	    ("PERIOD" (:control) "restart .")
	    ("KP-ENTER" nil "enter .")
	    ("RETURN" nil "enter .")
	    ("ESCAPE" (:control) "show-location .")
	    ("3" nil "activate-extension .")
	    ("2" nil "activate-pulse-cannon .")
	    ("1" nil "activate-bomb-cannon .")
	    ("Q" (:control) "quit ."))))
  
(defparameter *alternate-qwerty-keybindings*
  (append *basic-keybindings*
	  '(("Q" nil "move :northwest .")
	    ("W" nil "move :north .")
	    ("E" nil "move :northeast .")
	    ("A" nil "move :west .")
	    ("D" nil "move :east .")
	    ("Z" nil "move :southwest .")
	    ("X" nil "move :south .")
	    ("C" nil "move :southeast .")
	    ;;
	    ("Q" (:alt) "attack :northwest .")
	    ("W" (:alt) "attack :north .")
	    ("E" (:alt) "attack :northeast .")
	    ("A" (:alt) "attack :west .")
	    ("D" (:alt) "attack :east .")
	    ("Z" (:alt) "attack :southwest .")
	    ("X" (:alt) "attack :south .")
	    ("C" (:alt) "attack :southeast .")
	    ;;
	    ("Q" (:meta) "attack :northwest .")
	    ("W" (:meta) "attack :north .")
	    ("E" (:meta) "attack :northeast .")
	    ("A" (:meta) "attack :west .")
	    ("D" (:meta) "attack :east .")
	    ("Z" (:meta) "attack :southwest .")
	    ("X" (:meta) "attack :south .")
	    ("C" (:meta) "attack :southeast .")
	    ;;
	    ("Q" (:control) "fire :northwest .")
	    ("W" (:control) "fire :north .")
	    ("E" (:control) "fire :northeast .")
	    ("A" (:control) "fire :west .")
	    ("D" (:control) "fire :east .")
	    ("Z" (:control) "fire :southwest .")
	    ("X" (:control) "fire :south .")
	    ("C" (:control) "fire :southeast .")
	    ;;
	    ("S" nil "wait .")
	    ("ESCAPE" (:control) "show-location .")
	    ("SPACE" nil "wait .")
	    ("PERIOD" (:control) "restart .")
	    ("3" nil "activate-extension .")
	    ("2" nil "activate-pulse-cannon .")
	    ("1" nil "activate-bomb-cannon .")
	    ("P" (:control) "quit ."))))
  
;; g c r
;;  \|/
;; h-.-n
;;  /|\ 
;; m w v

(defparameter *dvorak-keybindings*
  (append *basic-keybindings*
	  '(("G" nil "move :northwest .")
	    ("C" nil "move :north .")
	    ("R" nil "move :northeast .")
	    ("H" nil "move :west .")
	    ("N" nil "move :east .")
	    ("M" nil "move :southwest .")
	    ("W" nil "move :south .")
	    ("V" nil "move :southeast .")
	    ;;
	    ("G" (:alt) "attack :northwest .")
	    ("C" (:alt) "attack :north .")
	    ("R" (:alt) "attack :northeast .")
	    ("H" (:alt) "attack :west .")
	    ("N" (:alt) "attack :east .")
	    ("M" (:alt) "attack :southwest .")
	    ("W" (:alt) "attack :south .")
	    ("V" (:alt) "attack :southeast .")
	    ;;
	    ("G" (:meta) "attack :northwest .")
	    ("C" (:meta) "attack :north .")
	    ("R" (:meta) "attack :northeast .")
	    ("H" (:meta) "attack :west .")
	    ("N" (:meta) "attack :east .")
	    ("M" (:meta) "attack :southwest .")
	    ("W" (:meta) "attack :south .")
	    ("V" (:meta) "attack :southeast .")
	    ;;
	    ("G" (:control) "fire :northwest .")
	    ("C" (:control) "fire :north .")
	    ("R" (:control) "fire :northeast .")
	    ("H" (:control) "fire :west .")
	    ("N" (:control) "fire :east .")
	    ("M" (:control) "fire :southwest .")
	    ("W" (:control) "fire :south .")
	    ("V" (:control) "fire :southeast .")
	    ;;
	    ("S" nil "wait .")
	    ("SPACE" nil "wait .")
	    ("KP-ENTER" nil "enter .")
	    ("RETURN" nil "enter .")
	    ("ESCAPE" (:control) "show-location .")
	    ("PERIOD" (:control) "restart .")
	    ("3" nil "activate-extension .")
	    ("2" nil "activate-pulse-cannon .")
	    ("1" nil "activate-bomb-cannon .")
	    ("Q" (:control) "quit ."))))

(define-method install-keybindings vomac-prompt ()
  (let ((keys (ecase rlx:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:alternate-qwerty *alternate-qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k)))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *active-world* :timer])])

;;; Main program. 

(defparameter *vomac-window-width* 800)
(defparameter *vomac-window-height* 600)

(defvar *viewport*)

(defun vomac ()
  (rlx:message "Initializing Vomac...")
  (setf rlx:*window-title* "Vomac")
  (setf clon:*send-parent-depth* 2) 
  (rlx:set-screen-height *vomac-window-height*)
  (rlx:set-screen-width *vomac-window-width*)
  ;; go!
  (let* ((prompt (clone =vomac-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (player (clone =olvac=))
	 (splash (clone =splash=))
	 (help (clone =formatter=))
	 (quickhelp (clone =formatter=))
	 (viewport (clone =viewport=))
	 (status (clone =status=))
	 (splash-prompt (clone =splash-prompt=))
	 (terminal (clone =narrator=))
	 (stack (clone =stack=)))
    ;;
    (setf *viewport* viewport)
    (setf *status* status)
    ;;
    [resize splash :height (- *vomac-window-height* 20) :width *vomac-window-width*]
    [move splash :x 0 :y 0]
    [resize splash-prompt :width 10 :height 10]
    [move splash-prompt :x 0 :y 0]
    [hide splash-prompt]
    [set-receiver splash-prompt splash]
    ;;
    [resize *status* :height 20 :width *vomac-window-width*]
    [move *status* :x 0 :y 0]
    ;;
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    ;;
    (labels ((spacebar ()
	       ;;
	       ;; enable pseudo timing
	       (rlx:enable-timer)
	       (rlx:set-frame-rate 30)
	       (rlx:set-timer-interval 1)
	       (rlx:enable-held-keys 1 3)
	       ;;
	       [set-player universe player]
	       [play universe
	       	     :address (generate-level-address 1)
	       	     :prompt prompt
	       	     :narrator terminal
	       	     :viewport viewport]
	       [loadout player]
	       ;;
	       [set-character *status* player]
	       ;;
	       [set-tile-size viewport 16]
	       [resize viewport :height 470 :width *vomac-window-width*]
	       [move viewport :x 0 :y 0]
	       [set-origin viewport :x 0 :y 0 
			   :height (truncate (/ (- *vomac-window-height* 130) 16))
			   :width (truncate (/ *vomac-window-width* 16))]
	       [adjust viewport]))
      (setf *space-bar-function* #'spacebar))
    ;;
    [resize help :height 540 :width 800] 
    [move help :x 0 :y 0]
    (let ((text	(find-resource-object "help-message")))
      (dolist (line text)
	(dolist (string line)
	  (funcall #'send nil :print-formatted-string help string))
	[newline help]))
    ;;
    [resize quickhelp :height 85 :width 250] 
    [move quickhelp :y (- *vomac-window-height* 130) :x (- *vomac-window-width* 250)]
    (let ((text	(find-resource-object "quickhelp-message")))
      (dolist (line text)
	(dolist (string line)
	  (funcall #'send nil :print-formatted-string quickhelp string))
	[newline quickhelp]))
    ;;
    (play-music "vomac-theme" :loop t)
    (set-music-volume 255)	       
    ;;
    [resize stack :width *vomac-window-width* :height (- *vomac-window-height* 20)]
    [move stack :x 0 :y 0]
    [set-children stack (list viewport terminal status)]
    ;;
    [resize terminal :height 80 :width *vomac-window-width*]
    [move terminal :x 0 :y (- *vomac-window-height* 80)]
    [set-verbosity terminal 0]
    ;;
    ;; HACK
    ;; (labels ((light-hack (sr sc r c &optional (color ".white"))
    ;; 	       (labels ((hack-overlay (image)
    ;; 			  (multiple-value-bind (sx sy)
    ;; 			      [get-viewport-coordinates *viewport* sr sc]
    ;; 			    (multiple-value-bind (x y)
    ;; 				[get-viewport-coordinates *viewport* r c]
    ;; 			      (draw-line x y sx sy :destination image
    ;; 					 :color color)
    ;; 			      (draw-circle x y 5 :destination image)))))
    ;; 		 [add-overlay *viewport* #'hack-overlay]))))
;;      (setf rlx::*lighting-hack-function* #'light-hack))
    ;; END HACK
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    (rlx:install-widgets splash-prompt splash)
    [add-page *pager* :game prompt stack viewport terminal *status* quickhelp]
    [add-page *pager* :help help]))

(vomac)

