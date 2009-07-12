;;; blast.lisp --- a micro anti-shmup in common lisp

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

;;; Commentary 

;; Blast Tactics is a mini space-roguelike puzzler incorporating
;; elements of the classic Asteroids, plus gravity and a unique weapon
;; system. Shoot asteroids and enemies, or sweep your trail across
;; them. Powerups extend your trail's length and enable higher scores.

;;; Packaging

(defpackage :blast
  (:documentation "Blast Tactics: A sci-fi roguelike for Common Lisp.")
  (:use :rlx :common-lisp)
  (:export blast))

(in-package :blast)

;;; Billboard shows most recent attention data.

(defvar *billboard*)

(defvar *billboard-strings* '(:go ("GO!" :foreground ".black" :background ".yellow" 
				   :font "display-font")
			      :react ("REACT!" :foreground ".red" :background ".navy blue"
				      :font "display-font")
			      :extend ("EXTEND!" :foreground ".yellow" :background ".blue"
				       :font "display-font")
			      :pulse-ammo ("PULSE +2" :foreground ".red" :background ".black"
				       :font "display-font")
			      :bomb-ammo ("BOMB +2" :foreground ".red" :background ".black"
				       :font "display-font")
			      :shield ("SHIELD +1" :foreground ".cyan" :background ".blue"
				       :font "display-font")
			      :warning ("WARNING!" :foreground ".yellow" :background ".red"
					:font "display-font")
			      :hit ("HIT!" :foreground ".yellow" :background ".purple" 
				    :font "display-font")
			      :dead ("YOU DIE!" :foreground ".yellow" :background ".red"
				     :font "display-font")
			      :destroy ("DESTROY!" :foreground ".white" :background ".red"
					:font "display-font") 
			      :probe-kill ("DESTROY!" :foreground ".white" :background ".forestgreen"
					:font "display-font")
                             :sweep ("SWEEP!" :foreground ".yellow" :background ".forest green"
				      :font "display-font")))

(defun billboard-string (key)
  (getf *billboard-strings* key '("STANDBY")))

(define-prototype billboard (:parent rlx:=formatter=)
  (font :initform "display-font"))

(define-method say billboard (key)
  [delete-all-lines self]
  [print-formatted-string self (billboard-string key)]
  [newline self])

;;; Empty space.

(defcell space 
  (tile :initform "space"))

;;; Colored space.

(defcell space2
  (tile :initform "space2"))

;;; Radioactive gas

(defcell gas
  (tile :initform "rad"))

(define-method step gas (stepper)
  (when [is-player stepper]
    [damage stepper 4]
    [>>say :narrator "RADIOACTIVE HAZARD!"]))

;;; A destructible wall.

(defcell wall
  (tile :initform "wall")
  (categories :initform '(:obstacle))
  (hit-points :initform (make-stat :base 20 :min 0)))

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
  (equip-for :initform '(:right-bay)))

(define-method activate bomb-cannon ()
  ;; leave bomb behind ship
  (clon:with-field-values (last-direction row column) <equipper>
    (multiple-value-bind (r c) 
	(step-in-direction row column 
			   (opposite-direction last-direction))
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

;;; Strength powerup

(defcell level-up 
  (categories :initform '(:item))
  (tile :initform "levelup")
  (name :initform "Strength power-up"))

(define-method step level-up (stepper)
  (when [is-player stepper] 
    (play-sample "worp")
    [>>say :narrator "LEVEL UP! Max hit points +4"]
    [>>stat-effect stepper :hit-points 4 :max]
    [>>die self]))

;;; Speed powerup

(defcell speed-up 
  (categories :initform '(:item))
  (tile :initform "speedup")
  (name :initform "Speed power-up"))

(define-method step speed-up (stepper)
  (when [is-player stepper]
    (play-sample "worp")
    [>>say :narrator "SPEED +2!"]
    [>>stat-effect stepper :speed 2]
    [>>die self]))

(defun random-stat-powerup ()
  (clone (case (random 2)
	   (0 =level-up=)
	   (1 =speed-up=))))

;;; Death icon.

(defparameter *death-message* "You are dead. Press SPACE BAR to respawn.")
(defparameter *game-over-message* "No lives remaining. GAME OVER.")

(define-prototype skull (:parent rlx:=cell=)
  (tile :initform "skull")
  (player :initform nil)
  (lives :initform nil)
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

(define-method initialize skull (player lives)
  [say *billboard* :dead]
  (setf <player> player
	<lives> lives)
  (if (plusp lives)
      (progn [>>say :narrator *death-message*]
	     [>>say :narrator "You have ~D lives remaining." lives])
      [>>say :narrator *game-over-message*]))

(define-method respawn skull ()
  (if (plusp <lives>)
      (progn
	[>>say :narrator "Respawning."]
        [die self]
	[play-sample self "go"]
	[revive <player>])
      (progn
	[>>say :narrator *game-over-message*])))

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

;;; There are also energy tanks for replenishing ammo.

(defcell energy 
  (tile :initform "energy"))

(define-method step energy (stepper)
  (when [is-player stepper]
    (when (has-field :energy stepper)
      [play-sample self "whoop"]
      [>>stat-effect stepper :energy 5]
      [>>die self])))

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
  (clock :initform 9))

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
  (accuracy :initform (make-stat :base 90))
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
  (speed :initform (make-stat :base 10))
  (crystals :initform (make-stat :base 0 :min 0))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 15)
  (energy :initform (make-stat :base 40 :min 0 :max 40))
  (hit-points :initform (make-stat :base 20 :min 0 :max 20))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (trail-length :initform (make-stat :base 12 :min 0))
  (pulse-ammo :initform (make-stat :base 3 :min 0 :max 5))
  (bomb-ammo :initform (make-stat :base 3 :min 0 :max 5))
  (invincibility-clock :initform 0)
  (stepping :initform t)
  (lives :initform (make-stat :min 0 :base 3 :max 3))
  (score :initform (make-stat :base 0))
  (attacking-with :initform :right-bay)
  (firing-with :initform :center-bay)
  (categories :initform '(:actor :player :target :container :light-source))
  (equipment-slots :initform '(:left-bay :right-bay :center-bay))
  (boost-clock :initform 0))

(define-method initialize ship ()
  [say *billboard* :go])

(define-method loadout ship ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =muon-cannon=)]])

(define-method quit ship ()
  (rlx:quit :shutdown))

(define-method run ship ()
  [update-tile self]
  [update-react-shield self]
  [update *status*])	       

(define-method wait ship ()
  [expend-action-points self <action-points>])

(define-method respawn ship ()
  nil)

(define-method activate-pulse-cannon ship ()
  (when (plusp [stat-value self :pulse-ammo])
    [activate [equipment-slot self :left-bay]]
    [stat-effect self :pulse-ammo -1]))

(define-method activate-bomb-cannon ship ()
  (when (plusp [stat-value self :bomb-ammo])
    [activate [equipment-slot self :right-bay]]
    [stat-effect self :bomb-ammo -1]))

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
		 (prog1 "player-ship-north-shield"
		   [say *billboard* :warning])))))
		 
(define-method damage ship (points)
  (if (= 0 <invincibility-clock>)
    (progn [play-sample self "warn"]
	   [parent>>damage self points]
	   [say *billboard* :react]
	   (setf <invincibility-clock> 5)
	   [update-tile self]
	   [>>say :narrator "React Shield up with 5 turns remaining."])
    (progn 
      [>>say :narrator "React shield blocks 1 damage."]
      (decf <invincibility-clock>)
      [update-tile self])))
  
(define-method loadout ship ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =muon-cannon=)]]
  [equip self [add-item self (clone =pulse-cannon=)]]
  [equip self [add-item self (clone =bomb-cannon=)]])

(define-method step ship (stepper)
  (when (eq =asteroid= (object-parent stepper))
    [say *billboard* :hit]
    [damage self 1]
    [>>say :narrator "You were damaged by a floating asteroid!"]))

(define-method die ship ()
  [play-sample self "death"]
  [stat-effect self :lives -1]
  (let ((skull (clone =skull= self [stat-value self :lives])))
    [drop-cell *active-world* skull <row> <column> :loadout t :no-collisions nil]
    (setf <action-points> 0)
    [add-category self :dead]
    [>>delete-from-world self]
    [set-player *active-world* skull]))

(define-method revive ship ()
  [say *billboard* :go]
  [drop-cell *active-world* self (random 10) (random 10)]
  [stat-effect self :hit-points 20]	       
  [stat-effect self :energy 40]	       
  [update-tile self]
  [delete-category self :dead]
  [stat-effect self :trail-length (- (1+ [stat-value self :trail-length]))]
  [set-player *active-world* self])

;;; A life powerup.

(defcell diamond
  (tile :initform "diamond"))

(define-method step diamond (stepper)
  (when [is-player stepper]
   [play-sample self "powerup"]
   [say *billboard* :shield]
   [stat-effect stepper :hit-points 8]
   [stat-effect stepper :score 2000]
   [die self]))

;;; Mineral crystals to collect.

(defcell crystal
  (tile :initform "crystal")
  (categories :initform '(:target))
  (hit-points :initform (make-stat :base 1 :min 0)))

(define-method step crystal (stepper)
  (when [is-player stepper]
   [play-sample self "bip"]
   [stat-effect stepper :crystals 1]
   [stat-effect stepper :score 1000]
   [die self]))

;;; A trail extender powerup.

(defcell extender 
  (tile :initform "plus"))

(define-method step extender (stepper)
  (when [is-player stepper]
    [play-sample self "powerup"]
    [say *billboard* :extend]
    [>>say :narrator "Trail extend!"]
    [stat-effect stepper :trail-length 4]
    [stat-effect stepper :score 2000]
    [die self]))

;;; Extra ammo for pulse protector

(defcell pulse-ammo 
  (tile :initform "pulse-ammo"))

(define-method step pulse-ammo (stepper)
  (when [is-player stepper]
    [play-sample self "powerup"]
    [say *billboard* :pulse-ammo]
    [>>say :narrator "PULSE +2!"]
    [stat-effect stepper :pulse-ammo 2]
    [stat-effect stepper :score 2000]
    [die self]))

;;; Extra bomb ammo

(defcell bomb-ammo
  (tile :initform "bomb-ammo"))

(define-method step bomb-ammo (stepper)
  (when [is-player stepper]
    [play-sample self "powerup"]
    [say *billboard* :bomb-ammo]
    [>>say :narrator "BOMB +2!"]
    [stat-effect stepper :bomb-ammo 2]
    [stat-effect stepper :score 2000]
    [die self]))

;;; Random powerup function

(defun random-powerup ()
  (clone (ecase (random 5)
	   (0 =diamond=)
	   (1 =pulse-ammo=)
	   (2 =extender=)
	   (3 =bomb-ammo=)
	   (4 =diamond=))))


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

(define-method explode graviceptor ()
  ;; only when not in space debris... debris are "safe zones" from mines
  (when (notany #'(lambda (ob)
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
  (when (> 1 (random 230))
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

;;; Magnetic space debris will slow movement

(defcell debris 
;  (categories :initform '(:obstacle))
  (tile :initform "debris"))

(define-method step debris (stepper)
  (when [is-player stepper]	
    [>>say :narrator "Your movement is slowed by the space debris."]
    [expend-action-points stepper 5]))

(define-method damage debris (points)
  (declare (ignore points))
  [die self])

;;; An asteroid.

(defvar *asteroid-count* 0)

(defcell asteroid
  (categories :initform '(:actor :sticky :target))
  (hit-points :initform (make-stat :base 1 :min 0))
  (movement-cost :initform (make-stat :base 10))
  (stuck-to :initform nil)
  (direction :initform :north)
  (stepping :initform t))

(define-method is-stuck asteroid ()
  <stuck-to>)

(define-method die asteroid ()
  (decf *asteroid-count*)
  (when (zerop *asteroid-count*)
    [>>say :narrator "YOU WIN!!!"])
  ;;
  [>>say :narrator "You destroyed an asteroid!"]
  [say *billboard* :destroy]
  [play-sample self "bleep"]
  (when (< (random 3) 1)
    [drop self (random-powerup)])
  [stat-effect [get-player *active-world*] :score 120]
  (when <stuck-to>
    [unstick <stuck-to> self])
  [parent>>die self])

(define-method initialize asteroid (&key speed direction color)
  (incf *asteroid-count*)
  (setf <speed> (make-stat :base speed))
  (setf <direction> direction)
  (setf <tile>
	(ecase color
	  (:orange "asteroid-orange")
	  (:red "asteroid-red")
	  (:blue "asteroid-blue")
	  (:brown "asteroid-brown"))))

(define-method run asteroid ()
  (when (eq :here <direction>)
    (setf <direction> :north))
  (if (<= [stat-value self :hit-points] 0)
      [die self]
      ;; if free, float
      (if (and (not <stuck-to>)  
	       (not [obstacle-in-direction-p *active-world* <row> <column> <direction>]))
	  [move self <direction>]
	  ;; otherwise bounce (when free)
	  (unless <stuck-to>
	    (setf <direction> (rlx:random-direction))))))

(define-method step asteroid (stepper)
  (when [in-category stepper :player]
    [damage stepper 3]
    [say *billboard* :hit]
    [>>say :narrator "You took a hit!"]
    [die self]))

;;; Polaris collects asteroids

(defcell polaris
  (tile :initform "polaris")
  (asteroids :initform '())
  (stepping :initform t)
  (categories :initform '(:actor :target))
  (hit-points :initform (make-stat :base 5 :min 0 :max 5))
  (direction :initform (rlx:random-direction)))

(define-method scan-neighborhood polaris ()
  (dolist (dir *compass-directions*)
    (multiple-value-bind (r c) (rlx:step-in-direction <row> <column> dir)
      (do-cells (cell [cells-at *active-world* r c])
	(when (and cell [in-category cell :sticky])
	  [stick self cell])))))

(define-method change-direction polaris (direction)
  (dolist (asteroid <asteroids>)
    (assert (clon:object-p asteroid))
    (setf (field-value :direction asteroid) direction))
  (setf <direction> direction))

(define-method move-as-group polaris (direction)
  ;; move self first so that nobody steps on us
  [move self direction]
  ;; now move the stuck asteroids
  (dolist (a <asteroids>)
    [move a direction]))

(define-method run polaris ()
  [scan-neighborhood self]	       
  ;; reset direction for stuck mines
  (when (eq <direction> :here)
    (setf <direction> :north))
  (let ((direction <direction>))	       
    (labels ((obstructed (asteroid)
	       [obstacle-in-direction-p *active-world*
					(field-value :row asteroid)
					(field-value :column asteroid)
					direction]))
      (let ((timeout 8)) 
	(loop while (and (plusp timeout)
			 (or (some #'obstructed <asteroids>)
			     (obstructed self)))
	   do [change-direction self (rlx:random-direction)]
	     (decf timeout))
	(unless (zerop timeout)
	  ;; it's safe. move as a group. 
	  [move-as-group self <direction>])))))
	      
(define-method stick polaris (asteroid)
  (when (and [in-category asteroid :sticky]
	     (not [is-stuck asteroid]))
    (setf (field-value :stuck-to asteroid) self)
    (setf (field-value :direction asteroid) <direction>)
    ;; put it back where it was
    [move asteroid (rlx:opposite-direction (field-value :direction asteroid))]
    (pushnew asteroid <asteroids>)))

(define-method unstick polaris (asteroid)
  (setf <asteroids> (delete asteroid <asteroids>))
  (when (= 0 (length <asteroids>))
    [stat-effect [get-player *active-world*] :score 2000]
    [play-sample self "sweep"]
    [say *billboard* :sweep]
    [>>say :narrator "You get 2000 extra points for wiping the polaris mine clean of asteroids."]))

(define-method explode polaris ()
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
		     5 5)))

(define-method die polaris ()
  [explode self]
  [parent>>die self])

;;; Some destructible blocks

(defcell blast-box
  (tile :initform "blast-box")
  (name :initform "Storage crate")
  (categories :initform '(:obstacle :opaque :pushable :destructible :target))
  (hit-points :initform (make-stat :base 1 :min 0)))

(defcell blast-box-debris
  (tile :initform "blast-box-debris")
  (name :initform "Crate debris"))

(define-method die blast-box ()
  [>>drop self (clone =blast-box-debris=)]
  [parent>>die self])

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

;;; The ion shield

(defcell ion-shield-wall 
  (tile :initform "ion-shield-wall")
  (categories :initform '(:obstacle :actor :target))
  (hit-points :initform (make-stat :base 10 :min 0))
  (clock :initform (+ 12 (random 4))))

(define-method die ion-shield-wall ()
  [queue>>drop-cell *active-world* (clone =flash=) <row> <column>]
  [parent>>die self])

(define-method run ion-shield-wall ()
  (when (zerop <clock>)
    [die self])
  (decf <clock>))

(defcell ion-shield 
  (categories :initform '(:item :equipment))
  (name :initform "Ion shield belt")
  (tile :initform "ion-shield")
  (equip-for :initform '(:belt))
  (size :initform 5))

(defparameter *ion-shield-energy-cost* 60)

(define-method activate ion-shield ()
  (let* ((world *active-world*)
	 (row [player-row world])
	 (column [player-column world])
	 (size <size>))
    (if [expend-energy [get-player world] *ion-shield-energy-cost*]
      (labels ((drop-ion (r c)
		 (prog1 nil
		   [drop-cell world (clone =ion-shield-wall=) r c :no-collisions nil])))
	[>>say :narrator "Activating ion shield."]
	(trace-rectangle #'drop-ion 
			 (- row (truncate (/ size 2)))
			 (- column (truncate (/ size 2)))
			 size size))
      [say :narrator "Not enough energy to activate shield."])))

(define-method step ion-shield (stepper)
  (when [is-player stepper]
    [>>say :narrator "You've found the Ion Shield Belt."]
    [>>take stepper :direction :here :category :item]))

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

;;; The inescapable game grid.

(define-prototype void-world (:parent rlx:=world=)
  (ambient-light :initform :total))

(define-method generate void-world (&key   
				    (width 20)
				    (height 20)
				    (asteroid-count 20)
				    (biclops-count 0)
				    (berserker-count 0)
				    (polaris-count 5)
				    (probe-count 5)
				    (box-cluster-count 4)
				    (gas-cluster-count 0)
				    (room-size 3)
				    (room-count 4)
				    (scanner-count 0)
				    (energy-count 3))
  (setf <height> height <width> width)
  [create-default-grid self]
  [drop-plasma-space self]
  [drop-plasma-debris self]
  (dotimes (i polaris-count)
    [drop-cell self (clone =polaris=)
	       (random height) (random width)])
  ;; drop enemies
  (dotimes (i scanner-count)
    [drop-cell self (clone =scanner=)
	       (random height) (random width) :loadout t])
  (dotimes (i berserker-count)
    [drop-cell self (clone =berserker=)
	       (random height) (random width) :loadout t])
  (dotimes (i biclops-count)
    [drop-cell self (clone =biclops=)
	       (random height) (random width) :loadout t])
  (dotimes (i probe-count)
    [drop-cell self (clone =probe=)
	       (random height) (random width)])
  ;; drop stuff
  (dotimes (i energy-count)
    [drop-cell self (clone =energy=)
	       (random height) (random width)])
  (dotimes (i room-count)
      [drop-room self 
		 (random height)
		 (random width)
		 (+ room-size (random 3))
		 (+ room-size (random 4))])
  (dotimes (i box-cluster-count)
    (let ((r (random height))
	  (c (random width)))
      [drop-box-cluster self r c]))
  (dotimes (i gas-cluster-count)
    (let ((r (random height))
	  (c (random width)))
      [drop-gas-cluster self r c]))
  ;; and finally the 'roids
  [drop-random-asteroids self asteroid-count])

   
(define-method drop-random-asteroids void-world (count)
  (clon:with-field-values (height width) self
    (dotimes (i count)
      [drop-cell self (clone =asteroid= :speed (+ 3 (random 7))
			     :direction (rlx:random-direction)
			     :color (nth (random 4)
					 '(:red :blue :brown :orange)))
		 (random height) (random width)])))


(define-method drop-plasma-debris void-world ()
  (clon:with-field-values (height width) self
    (let ((plasma (rlx:render-plasma height width :graininess 0.4))
	  (value nil))
      (dotimes (i (- height 10))
	(dotimes (j (- width 10))
	  (setf value (aref plasma i j))
	  (when (< 0 value)
	    (let ((object =debris=))
	      [drop-cell self (clone object) (+ 10 i) (+ 10 j) :no-collisions t])))))))

(define-method drop-plasma-space void-world ()
  (clon:with-field-values (height width) self
    (let ((plasma (rlx:render-plasma height width :graininess 0.1))
	  (value nil))
      (dotimes (i height)
	(dotimes (j width)
	  (setf value (aref plasma i j))
	  [drop-cell self (clone (if (minusp value)
				     =space= =space2=))
		     i j])))))

(define-method drop-room void-world (row column height width)
  (let (rectangle)
    (labels ((collect-point (&rest args)
	       (prog1 nil (push args rectangle))))
      (trace-rectangle #'collect-point row column height width)
      ;; make sure there are openings
      (dotimes (i 3)
	(let ((n (random (length rectangle))))
	  (delete (nth n rectangle) rectangle)))
      (dolist (point rectangle)
	(destructuring-bind (r c) point
	  [drop-cell self (clone =wall=) r c :no-collisions t])))
    (when (> 4 (random 10))
      (dotimes (i (+ 2 (random 10)))
	[drop-cell self (clone =crystal=) 
		   (+ 1 row (random 4))
		   (+ 1 column (random 4))
		   :no-collisions t]))))

(define-method drop-box-cluster void-world (row column &key
						(height (+ 3 (random 5)))
						(width (+ 3 (random 5))))
  (labels ((drop-box (r c)
	     (prog1 nil
	       [drop-cell self (clone =blast-box=) r c])))
    (trace-rectangle #'drop-box row column height width)))

(define-method drop-gas-cluster void-world (row column &key
						(height (+ 3 (random 5)))
						(width (+ 3 (random 5))))
  (labels ((drop-gas (r c)
	     (prog1 nil
	       [drop-cell self (clone =gas=) r c])))
    (trace-rectangle #'drop-gas row column (+ 1 height) (+ 1 width) :fill)))

;;; Different challenge levels.

(defvar *void-levels* '((:width 20
			 :height 20
			 :asteroid-count 12
			 :polaris-count 2
			 :probe-count 2
			 :box-cluster-count 2
			 :room-size 4
			 :room-count 3
			 :scanner-count 0
			 :energy-count 3)
			(:width 50
			 :height 24
			 :asteroid-count 40
			 :polaris-count 12
			 :probe-count 15
			 :box-cluster-count 5
			 :gas-cluster-count 4
			 :room-count 14
			 :room-size 5
			 :scanner-count 3
			 :energy-count 7)
			(:width 50
			  :height 200
			  :asteroid-count 200
			  :biclops-count 10
			  :berserker-count 10
			  :polaris-count 70
			  :probe-count 50
			  :gas-cluster-count 25
			  :room-size 8
			  :box-cluster-count 40
			  :room-count 65
			  :scanner-count 25
			  :energy-count 40)))

(defvar *level* 0)

;;; Custom bordered viewport

(define-prototype view (:parent rlx:=viewport=))

(define-method render view ()
  [parent>>render self]
  (rlx:draw-rectangle 0 0 
		      <width>
		      <height>
		      :color ".blue" :destination <image>))

;;; Controlling the game.

(define-prototype blast-prompt (:parent rlx:=prompt=))

(defparameter *numpad-keybindings* 
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
    ("KP3" (:control) "fire :southeast .")))

(defparameter *qwerty-keybindings*
  (append *numpad-keybindings*
	  '(("Y" nil "move :northwest .")
	    ("K" nil "move :north .")
	    ("U" nil "move :northeast .")
	    ("H" nil "move :west .")
	    ("L" nil "move :east .")
	    ("B" nil "move :southwest .")
	    ("J" nil "move :south .")
	    ("N" nil "move :southeast .")
	    ;;
	    ;; ("Y" (:alt) "attack :northwest .")
	    ;; ("K" (:alt) "attack :north .")
	    ;; ("U" (:alt) "attack :northeast .")
	    ;; ("H" (:alt) "attack :west .")
	    ;; ("L" (:alt) "attack :east .")
	    ;; ("B" (:alt) "attack :southwest .")
	    ;; ("J" (:alt) "attack :south .")
	    ;; ("N" (:alt) "attack :southeast .")
	    ;; ;;
	    ;; ("Y" (:meta) "attack :northwest .")
	    ;; ("K" (:meta) "attack :north .")
	    ;; ("U" (:meta) "attack :northeast .")
	    ;; ("H" (:meta) "attack :west .")
	    ;; ("L" (:meta) "attack :east .")
	    ;; ("B" (:meta) "attack :southwest .")
	    ;; ("J" (:meta) "attack :south .")
	    ;; ("N" (:meta) "attack :southeast .")
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
	    ("SPACE" nil "respawn .")
	    ("2" nil "activate-pulse-cannon .")
	    ("1" nil "activate-bomb-cannon .")
	    ("Q" (:control) "quit ."))))
  
(defparameter *alternate-qwerty-keybindings*
  (append *numpad-keybindings*
	  '(("Q" nil "move :northwest .")
	    ("W" nil "move :north .")
	    ("E" nil "move :northeast .")
	    ("A" nil "move :west .")
	    ("D" nil "move :east .")
	    ("Z" nil "move :southwest .")
	    ("X" nil "move :south .")
	    ("C" nil "move :southeast .")
	    ;;
	    ;; ("Q" (:alt) "attack :northwest .")
	    ;; ("W" (:alt) "attack :north .")
	    ;; ("E" (:alt) "attack :northeast .")
	    ;; ("A" (:alt) "attack :west .")
	    ;; ("D" (:alt) "attack :east .")
	    ;; ("Z" (:alt) "attack :southwest .")
	    ;; ("X" (:alt) "attack :south .")
	    ;; ("C" (:alt) "attack :southeast .")
	    ;; ;;
	    ;; ("Q" (:meta) "attack :northwest .")
	    ;; ("W" (:meta) "attack :north .")
	    ;; ("E" (:meta) "attack :northeast .")
	    ;; ("A" (:meta) "attack :west .")
	    ;; ("D" (:meta) "attack :east .")
	    ;; ("Z" (:meta) "attack :southwest .")
	    ;; ("X" (:meta) "attack :south .")
	    ;; ("C" (:meta) "attack :southeast .")
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
	    ("ESCAPE" nil "activate-pulse-cannon .")
	    ("SPACE" nil "respawn .")
	    ("1" nil "activate-bomb-cannon .")
	    ("P" (:control) "quit ."))))
  
;; g c r
;;  \|/
;; h-.-n
;;  /|\ 
;; m w v

(defparameter *dvorak-keybindings*
  (append *numpad-keybindings*
	  '(("G" nil "move :northwest .")
	    ("C" nil "move :north .")
	    ("R" nil "move :northeast .")
	    ("H" nil "move :west .")
	    ("N" nil "move :east .")
	    ("M" nil "move :southwest .")
	    ("W" nil "move :south .")
	    ("V" nil "move :southeast .")
	    ;;
	    ;; ("G" (:alt) "attack :northwest .")
	    ;; ("C" (:alt) "attack :north .")
	    ;; ("R" (:alt) "attack :northeast .")
	    ;; ("H" (:alt) "attack :west .")
	    ;; ("N" (:alt) "attack :east .")
	    ;; ("M" (:alt) "attack :southwest .")
	    ;; ("W" (:alt) "attack :south .")
	    ;; ("V" (:alt) "attack :southeast .")
	    ;; ;;
	    ;; ("G" (:meta) "attack :northwest .")
	    ;; ("C" (:meta) "attack :north .")
	    ;; ("R" (:meta) "attack :northeast .")
	    ;; ("H" (:meta) "attack :west .")
	    ;; ("N" (:meta) "attack :east .")
	    ;; ("M" (:meta) "attack :southwest .")
	    ;; ("W" (:meta) "attack :south .")
	    ;; ("V" (:meta) "attack :southeast .")
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
	    ("SPACE" nil "respawn .")
	    ("ESCAPE" nil "activate-pulse-cannon .")
	    ("1" nil "activate-bomb-cannon .")
	    ("Q" (:control) "quit ."))))

(define-method install-keybindings blast-prompt ()
  (let ((keys (ecase rlx:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:alternate-qwerty *alternate-qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k)))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *active-world* :timer])])

;;; A shield status and score widget.

(define-prototype status (:parent rlx:=formatter=)
  (character :documentation "The character cell whose status is shown."))

(define-method set-character status (char)
  (setf <character> char))

(defparameter *status-bar-character* "-")

(define-method update status ()
  [delete-all-lines self]
  (let* ((char <character>)
	 (hits [stat-value char :hit-points])
	 (lives [stat-value char :lives])
	 (energy [stat-value char :energy])
	 (pulse-ammo [stat-value char :pulse-ammo])
	 (bomb-ammo [stat-value char :bomb-ammo]))
    [print self " HITS: "]
    (dotimes (i [stat-value char :hit-points :max])
      [print self *status-bar-character* 
	     :foreground ".yellow"
	     :background (if (< i hits)
			     ".red"
			     ".gray20")])
    [newline self]
    ;; energy display
    [print self " ENERGY: "]
    (dotimes (i 40)
      [print self *status-bar-character* 
	     :foreground ".red"
	     :background (if (< i energy)
			     ".cyan"
			     ".gray20")])
    [newline self]
    [print self " LIVES: "]
    (dotimes (i 3)
      [print self *status-bar-character* 
	     :foreground ".yellow"
	     :background (if (< i lives)
			     ".blue"
			     ".gray20")])
    [space self]
    [print self " PULSE: "]
    (dotimes (i 6)
      [print self *status-bar-character* 
	     :foreground ".red"
	     :background (if (< i pulse-ammo)
			     ".yellow"
			     ".gray20")])
    [space self]
    [print self " BOMBS: "]
    (dotimes (i 6)
      [print self *status-bar-character* 
	     :foreground ".red"
	     :background (if (< i bomb-ammo)
			     ".green"
			     ".gray20")])
    [space self]
    [print self " DEPTH: "]
    [print self (format nil "~D" (field-value :row char))]
    [print self " LEVEL: "]
    [print self (format nil "~D" *level*)]
    [print self " SCORE: "]
    [println self (format nil "~D" [stat-value char :score])]
    [space self]
    [print self " ASTEROIDS REMAINING: "]
    [print self (format nil "~D" *asteroid-count*)]
    [print self " CRYSTALS: "]
    [print self (format nil "~D" [stat-value char :crystals])]
    [newline self]))

(defvar *status*)

;;; Splash screen

(defvar *play-widgets*)

(define-prototype splash (:parent =widget=))

(define-method render splash ()
  (rlx:draw-resource-image "splash" 0 0 
			   :destination <image>))

(define-method dismiss splash ()
  (play-sample "go")
  (apply #'rlx:install-widgets *play-widgets*))

(define-prototype splash-prompt (:parent =prompt=)
  (default-keybindings :initform '(("SPACE" nil "dismiss ."))))

;;; Main program. 

(defun blast ()
  (setf clon:*send-parent-depth* 2)
  (rlx:set-screen-height 600)
  (rlx:set-screen-width 800)
;  (rlx:set-frame-rate 30)
  ;; (rlx:set-timer-interval 20)
  ;; (rlx:enable-timer)
  (rlx:enable-held-keys 1 15)
  (setf *billboard* (clone =billboard=))
  (setf *asteroid-count* 0)
  (setf *level 0)
  (let* ((prompt (clone =blast-prompt=))
	 (world (clone =void-world=))
	 (narrator (clone =narrator=))
	 (status (clone =status=))
	 (player (clone =ship=))
	 (viewport (clone =view=))
	 (splash (clone =splash=))
	 (splash-prompt (clone =splash-prompt=)))
    (setf *active-world* world)
    ;;
    [resize splash :height 600 :width 600]
    [move splash :x 0 :y 0]
    [resize splash-prompt :width 10 :height 10]
    [move splash-prompt :x 0 :y 0]
    [hide splash-prompt]
    [set-receiver splash-prompt splash]
    ;;
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    [set-receiver prompt world]
    ;;
    [create-default-grid world]
    [generate-with world (nth 2 *void-levels*)]
    [set-player world player]
    [drop-cell world player 5 5 :loadout t]
    ;;
    [resize narrator :height 80 :width 800]
    [move narrator :x 0 :y 520]
    [set-narrator world narrator]
    [set-verbosity narrator 0]
    ;;
    [resize status :height 60 :width 700]
    [move status :x 10 :y 0]
    [set-character status player]
    (setf *status* status)
    [update status]
    ;;
    [resize *billboard* :height 20 :width 100]
    [move *billboard* :x 700 :y 0]
   ;;
    (setf (clon:field-value :tile-size viewport) 16)
    [set-world viewport world]
    [resize viewport :height 432 :width 800]
    [move viewport :x 0 :y 70]
    [set-origin viewport :x 0 :y 0 :height 24 :width 50]
    [adjust viewport]
    ;;
    [start world]
    (play-music "xiomacs" :loop t)
    (set-music-volume 255)	       

    (setf *play-widgets* (list prompt status viewport narrator *billboard*))
    (install-widgets splash-prompt splash)))

(blast)

