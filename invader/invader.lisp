;; invader.lisp --- you against the robots

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

;; Invader is a mini roguelike intended as an example game module for
;; RLX, which will (hopefully) also be fun to play. 

;; Basic features:

;;   - Oldschool Atari 5200-style graphics.
;;   - Infiltrate an airless enemy installation and destroy all the robots.
;;   - Wreak total destruction on fully destructible environments
;;   - Level gen params: color scheme, size, complexity, enemy/object density...
;;   - Melee combat with wrench.
;;   - Ranged combat with energy-using particle gun. Limited ammo.
;;   - Some enemies have particle shields and require melee hits to kill.
;;   - Rooks require ranged hits, their armor is too strong for wrenches.
;;   - Oxygen is constantly depleting, refills are required.
;;   - Minimal inventory management, one slot, all pickups are "activate-on-step"

;;; Packaging

(defpackage :invader
  (:documentation "A sci-fi roguelike for Common Lisp.")
  (:use :rlx :common-lisp)
  (:export invader))

(in-package :invader)

;;; Every space is either a wall or a corridor. 

(defcell wall
  (name :initform "Wall")
  (tile :initform "wall")
  (categories :initform '(:obstacle)))

(defcell corridor
  (name :initform "Airless Corridor")
  (tile :initform "corridor"))

;;; Moving in a corridor uses up oxygen.

(define-method step corridor (stepper)
  (when (has-field :oxygen stepper)
    [>>stat-effect stepper :oxygen -1]))

;;; You can refill your oxygen stores with these tanks.

(defcell oxygen-tank
  (tile :initform "oxygen-tank")
  (name :initform "Oxygen tank"))

(define-method step oxygen-tank (stepper)
  (when [is-player stepper]
    [>>say :narrator "You recover 40 points from the oxygen tank."]
    [>>stat-effect stepper :oxygen 40]
    [>>die self]))

;;; There are also energy tanks for replenishing ammo.

(defcell energy 
  (tile :initform "energy")
  (name :initform "Energy Tank"))

(define-method step energy (stepper)
  (when [is-player stepper]
    (when (has-field :energy stepper)
      [>>say :narrator "You recover 100 units of energy from the tank."]
      [>>stat-effect stepper :energy 100]
      [>>die self])))

;;; The player is depicted as a red diamond.

(defcell player 
  (tile :initform "player")
  (name :initform "Player")
  (categories :initform '(:actor :player :obstacle :target :container :light-source))
  ;; lighting
  (light-radius :initform 4)
  ;; action points and movement
  (speed :initform (make-stat :base 7 :min 1 :max 20))
  (movement-cost :initform (make-stat :base 7))
  ;; vital stats
  (hit-points :initform (make-stat :base 125 :min 0 :max 125)) 
  (dexterity :initform (make-stat :base 11 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (strength :initform (make-stat :base 14 :min 0 :max 40))
  (defense :initform (make-stat :base 14 :min 0 :max 40))
  ;; inventory-related data
  (max-items :initform (make-stat :base 2))
  ;; equipment-related slots
  (attacking-with :initform :right-hand)
  (firing-with :initform :left-hand)
  ;; other stats
  (energy :initform (make-stat :base 220 :min 0 :max 220))
  (oxygen :initform (make-stat :base 140 :min 0 :max 140))
  ;; default is do not generate step events; this turns it on
  (stepping :initform t))

(define-method loadout player ()
  [make-inventory self]
  [make-equipment self]
  (let ((wrench (clone =rusty-wrench=)))
    [equip self [add-item self wrench]]))

(define-method activate-equipment player (slot)
  (let ((item [equipment-slot self slot]))
    (if (clon:object-p item)
	[activate item]
	[>>narrateln :narrator "Nothing to activate."])))

;; (define-method damage player (damage-points)
;;   [>>say :narrator "You take ~D points of damage." damage-points]
;;   [parent>>damage self damage-points])

;;; To wait a turn without moving, hit W.

(define-method wait player ()
  [>>stat-effect self :oxygen -1]
  [expend-action-points self <action-points>])

;;; When you run out of oxygen, you die. 

(define-method run player ()
  (when (<= [stat-value self :oxygen] 0)
    [>>narrateln :narrator "Your oxygen runs out, suffocating you."]
    [die self]))

;;; When you fight a monster close-up, you use more oxygen.

(define-method attack player (target)
  [>>stat-effect self :oxygen -2]
  [parent>>attack self target])

(define-method damage player (damage-points)
  [parent>>damage self (- damage-points (truncate (/ [stat-value self :defense]
						     3)))])

;;; The player's remains are a skull and crossbones. 

(define-prototype skull (:parent rlx:=cell=)
  (tile :initform "skull")
  (categories :initform '(:dead :player :actor))
  (action-points :initform 0))

(define-method forward skull (&rest args)
  (declare (ignore args))
  [queue>>narrateln :narrator "You are dead. You can't do anything!"])
  
(define-method move skull (&rest args)
  (declare (ignore args))
 [queue>>narrateln :narrator "You are dead. You can't do anything!"])

(define-method attack skull (&rest args)
  (declare (ignore args))
 [queue>>narrateln :narrator "You are dead. You can't do anything!"])

(define-method fire skull (&rest args)
  (declare (ignore args))
 [queue>>narrateln :narrator "You are dead. You can't do anything!"])

(define-method die player ()
  (let ((skull (clone =skull=)))
    [drop-cell *active-world* skull <row> <column> :loadout t :no-collisions nil]
    (setf <action-points> 0)
    [add-category self :dead]
    [>>delete-from-world self]
    [>>narrateln :narrator "You die."]
    [set-player *active-world* skull]))

;;; About quitting the game

(define-method quit player ()
  (rlx:quit :shutdown))

(define-method quit skull ()
  (rlx:quit :shutdown))

;;; The medical healing hypo restores hit points.

(defcell med-hypo 
  (categories :initform '(:item))
  (tile :initform "med-hypo")
  (name :initform "Medical Hypo"))

(define-method step med-hypo (stepper)
  (when [is-player stepper]
    [>>say :narrator "You recover 30 hit points from the med hypo."]
    [>>stat-effect stepper :hit-points 30]
    [>>die self]))

;;; The med pack restores even more hit points.

(defcell med-pack 
  (categories :initform '(:item))
  (tile :initform "med-pack")
  (name :initform "Medical Hypo"))

(define-method step med-pack (stepper)
  (when [is-player stepper]
    [>>say :narrator "You recover 90 hit points from the med pack."]
    [>>stat-effect stepper :hit-points 90]
    [>>die self]))

;;; Strength powerup

(defcell level-up 
  (categories :initform '(:item))
  (tile :initform "levelup")
  (name :initform "Strength power-up"))

(define-method step level-up (stepper)
  (when [is-player stepper]
    [>>say :narrator "LEVEL UP! You feel stronger."]
    [>>stat-effect stepper :strength 5]
    [>>die self]))

;;; Defense powerup

(defcell defense-up 
  (categories :initform '(:item))
  (tile :initform "defenseup")
  (name :initform "Defense power-up"))

(define-method step defense-up (stepper)
  (when [is-player stepper]
    [>>say :narrator "DEFENSE UP!"]
    [>>stat-effect stepper :defense 5]
    [>>die self]))

;;; Speed powerup

(defcell speed-up 
  (categories :initform '(:item))
  (tile :initform "speedup")
  (name :initform "Speed power-up"))

(define-method step speed-up (stepper)
  (when [is-player stepper]
    [>>say :narrator "SPEED UP!"]
    [>>stat-effect stepper :speed 2]
    [>>die self]))

(defun random-powerup ()
  (clone (case (random 3)
	   (0 =defense-up=)
	   (1 =level-up=)
	   (2 =speed-up=))))

;;; A melee weapon for enemy robots: the Shock Probe

(defcell shock-probe 
  (name :initform "Shock probe")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "shock-probe")
  (attack-power :initform (make-stat :base 10))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (weight :initform 3000)
  (equip-for :initform '(:robotic-arm :left-hand :right-hand)))

;;; The rusty wrench; basic melee weapon

(define-prototype rusty-wrench (:parent rlx:=cell=)
  (name :initform "Rusty wrench")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "rusty-wrench")
  (attack-power :initform (make-stat :base 4)) ;; points of raw damage
  (attack-cost :initform (make-stat :base 5))
  (accuracy :initform (make-stat :base 60)) ;; percent
  (weight :initform 10000) ;; grams
  (equip-for :initform '(:left-hand :right-hand)))

(define-method step rusty-wrench (stepper)
  (when [is-player stepper]
    [>>take stepper :direction :here :category :item]))

;;; An explosion

(define-prototype explosion (:parent rlx:=cell=)
  (name :initform "Explosion")
  (categories :initform '(:actor))
  (tile :initform "explosion")
  (speed :initform (make-stat :base 10))
  (damage-per-turn :initform 20)
  (clock :initform 3))

(define-method run explosion ()
  (if (zerop <clock>)
      [die self]
      (progn
	(decf <clock>)
	[expend-action-points self 10]
	(let* ((cells [cells-at *active-world* <row> <column>])
	       (x (1- (fill-pointer cells))))
	  (loop while (not (minusp x))
	       do (progn 
		    [>>damage (aref cells x) <damage-per-turn>]
		    (decf x)))))))

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
  (strength :initform (make-stat :base 12 :min 0 :max 30))
  (dexterity :initform (make-stat :base 9 :min 0 :max 30))
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
    [drop self (clone (case (random 3)
			(0 =energy=)
			(1 =oxygen-tank=)
			(2 =med-hypo=)))])
  [parent>>die self])

(define-method loadout berserker ()
  (let ((probe (clone =shock-probe=)))
    [equip self [add-item self probe]]))

;;; The radar-equipped Biclops is more dangerous.  

(define-prototype biclops (:parent rlx:=cell=)
  (name :initform "Biclops")
  (strength :initform (make-stat :base 20 :min 0 :max 50))
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
  (hit-points :initform (make-stat :base 14 :min 0 :max 10))
  (tile :initform "biclops"))

(define-method initialize biclops ()
  [make-inventory self]
  [make-equipment self])

(define-method loadout biclops ()
  (let ((probe (clone =shock-probe=)))
    [equip self [add-item self probe]]))

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
    (if (> 3 (random 10))
	(if (> 5 (random 10))
	    [drop self (random-powerup)]
	    [drop self (clone =energy=)])
	[drop self (clone =med-hypo=)]))
  (if (> 2 (random 10))
      [drop self (clone =explosion=)])
  [parent>>die self])
 
;;; Rooks are the most difficult enemies. Slow, but powerful.

(defcell rook 
  (categories :initform '(:actor :target :obstacle :opaque :enemy))
  (equipment-slots :initform '(:robotic-arm :shoulder-mount))
  (attacking-with :initform :robotic-arm)
  (dexterity :initform (make-stat :base 20))
  (max-items :initform (make-stat :base 1))
  (speed :initform (make-stat :base 5))
  (stepping :initform t)
  (strength :initform (make-stat :base 50))
  (movement-cost :initform (make-stat :base 8))
  (tile :initform "rook")
  (target :initform nil)
  (hit-points :initform (make-stat :base 40 :min 0 :max 40)))

(define-method run rook ()
  (clon:with-field-values (row column) self
    (when (< [distance-to-player *active-world* row column] 10)
      (let ((direction [direction-to-player *active-world* row column])
	    (world *active-world*))
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
		       [>>move self direction])))))))
    
(define-method loadout rook ()
  [make-inventory self]
  [make-equipment self]
  (let ((probe (clone =shock-probe=)))
    [equip self [add-item self probe]]))

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

;;; The exploding mine

(defcell mine 
  (name :initform "Proximity mine")
  (categories :initform '(:item :target :actor))
  (tile :initform "mine"))

(defvar *mine-warning-sensitivity* 4)
(defvar *mine-explosion-sensitivity* 3)

(define-method run mine ()
  (let ((distance [distance-to-player *active-world* <row> <column>]))
    (if (< distance *mine-warning-sensitivity*)
	(progn
	  (when (string= <tile> "mine")
	    [>>say :narrator "You see a mine nearby!"])
	  (setf <tile> "mine-warn")
	  (when (< distance *mine-explosion-sensitivity*)
	    (when (< (random 8) 1)
	      [explode self])))
	(setf <tile> "mine"))))

(define-method explode mine ()
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

(define-method step mine (stepper)
  (when [is-player stepper]	      
    [explode self]))

(define-method damage mine (damage-points)
  (declare (ignore damage-points))
  [explode self])

;;; Some destructible blocks

(defcell tech-box
  (tile :initform "tech-box")
  (name :initform "Storage crate")
  (categories :initform '(:obstacle :opaque :pushable :destructible :target))
  (hit-points :initform (make-stat :base 10 :min 0)))

(defcell tech-box-debris
  (tile :initform "tech-box-debris")
  (name :initform "Crate debris"))

(define-method die tech-box ()
  [>>drop self (clone =tech-box-debris=)]
  (when (<= (random 16) 2)
    [>>drop self 
	    (if (= 0 (random 100))
		(random-powerup)
		(case (random 3)
		  (0 =oxygen-tank=)
		  (1 =energy=)
		  (2 =med-hypo=)))])
  [parent>>die self])

;;; One of the dead crew is holding the Ankh.

(defcell ankh
  (tile :initform "ankh")
  (name :initform "Ankh key")
  (categories :initform '(:item)))

(defparameter *ankh-generated-p* nil)

(define-method step ankh (stepper)
  (when [is-player stepper]
    [>>take stepper :direction :here :category :item]))

;;; Dead crewmember with random oxygen or possibly health.

(defcell crew-member 
  (tile :initform "crew")
  (categories :initform '(:item :target))
  (has-ankh :initform nil))

(define-method step crew-member (stepper)
  (when [is-player stepper]
    [>>say :narrator "You search the dead crewmember's body."]
    [expend-default-action-points stepper]
    (let ((oxygen (+ 10 (random 60))))
      [>>say :narrator "You recover ~D units of oxygen from the crewmember's tank."
		     oxygen]
      [>>stat-effect stepper :oxygen oxygen])
    (when (> 3 (random 10))
      [>>say :narrator "You find a medical hypo, and recover 30 hit points."]
      [>>stat-effect stepper :hit-points 30])
    (when (> 3 (random 10))
      (let ((energy (+ 10 (random 50))))
	[>>say :narrator "You recover ~D units of energy from the crewmember's battery pack." 
	       energy]
	[>>stat-effect stepper :energy energy]))
    (when <has-ankh>
      (let ((ankh (clone =ankh=)))
	[>>say :narrator "You found the Ankh!"]
	(if (numberp [add-item stepper ankh])
	    [>>say :narrator "You took the ankh."]
	    (progn 
	      [>>drop self ankh]
	      [>>say :narrator 
		     "You drop the ankh on the floor, because there is no room to hold it."]))))
    [>>die self]))
    
(define-method damage crew-member (points)
  (declare (ignore points))
  [>>say :narrator "The crewmember's body was destroyed!"]
  (when <has-ankh>
    [>>say "Something is left behind in the ashes."]
    [>>drop self (clone =ankh=)])
  [>>die self])

(define-method loadout crew-member ()
  (when (not *ankh-generated-p*)
    (setf <has-ankh> t
	  *ankh-generated-p* t)))

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

(define-prototype muon-trail (:parent rlx:=cell=)
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

(define-prototype muon-particle (:parent rlx:=cell=)
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
	  [queue>>damage target 7]
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

(define-prototype muon-pistol (:parent rlx:=cell=)
  (name :initform "Muon energy pistol")
  (tile :initform "gun")
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:left-hand))
  (weight :initform 7000)
  (accuracy :initform (make-stat :base 90))
  (attack-power :initform (make-stat :base 12))
  (attack-cost :initform (make-stat :base 5))
  (energy-cost :initform (make-stat :base 10)))

(define-method fire muon-pistol (direction)
  (if [expend-energy <equipper> 10]
      (let ((muon (clone =muon-particle=)))
	[>>drop <equipper> muon]
	[>>impel muon direction])
      [>>say :narrator "Not enough energy to fire!"]))

(define-method step muon-pistol (stepper)
  (when [is-player stepper]
    [>>take stepper :direction :here :category :item]))

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
  (hit-damage :initform (make-stat :base 14))
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
  (hit-points :initform (make-stat :base 40 :min 0))
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
  (when (= 0 (random 15))
    [drop self (random-powerup)])
  [parent>>die self])
	       

;;; The evil boss stations must be destroyed.

(defcell station-arm-horz 
  (tile :initform "station-arm-horz")
  (categories :initform '(:obstacle :opaque :destructible :target))
  (hit-points :initform (make-stat :base 10 :min 0)))

(defcell station-arm-vert 
  (tile :initform "station-arm-vert")
  (categories :initform '(:obstacle :opaque :destructible :target))
  (hit-points :initform (make-stat :base 10 :min 0)))

(defcell station-base 
  (tile :initform "station-base")
  (categories :initform '(:obstacle :actor :equipper :opaque))
  (speed :initform (make-stat :base 6))
  (hit-points :initform (make-stat :base 40 :min 0))
  (equipment-slots :initform '(:robotic-arm))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (attacking-with :initform :robotic-arm)
  (energy :initform (make-stat :base 800 :min 0 :max 1000))
  (firing-with :initform :robotic-arm)
  (strength :initform (make-stat :base 13))
  (dexterity :initform (make-stat :base 9)))

(defvar *station-base-count* 0)

(define-method loadout station-base ()
  (let ((cannon (clone =lepton-cannon=)))
    [equip self [add-item self cannon]])
  (incf *station-base-count*))

(define-method initialize station-base ()
  [make-inventory self]
  [make-equipment self])

(define-method run station-base ()
  (clon:with-field-values (row column) self
    (let ((world *active-world*))
      (if (< [distance-to-player world row column] 10)
	  (let ((player-dir [direction-to-player world row column]))
	    [expend-default-action-points self]
	    [queue>>fire self player-dir])))))

(define-method die station-base ()
  (decf *station-base-count*)
  (when (= 0 *station-base-count*)
    [>>narrateln :narrator "YOU WIN!"])
  [parent>>die self])

(defun paint-station-piece (world row column maxsize)
  (when (not [obstacle-at-p world row column])
    (labels ((drop-horz (r c)
	       [drop-cell world (clone =station-arm-horz=) r c])
	     (drop-vert (r c)
	       [drop-cell world (clone =station-arm-vert=) r c])
	     (drop-base (r c)
	       [drop-cell world (clone =station-base=) r c :loadout t]))
      (trace-row #'drop-horz row column (max 0 (- column (random maxsize))))
      (trace-row #'drop-horz row column (+ column (random maxsize)))
      (trace-column #'drop-vert column row (max 0 (- row (random maxsize))))
      (trace-column #'drop-vert column row (+ row (random maxsize)))
      (drop-base row column))))

;;; The sinister robot factory is defined here. 

(define-prototype factory-world (:parent rlx:=world=)
  (width :initform 48)
  (height :initform 300)
  (ambient-light :initform :total)
  (pallet-size :initform 10))

(define-method generate factory-world (&optional parameters)
  (declare (ignore parameters))
  (setf *ankh-generated-p* nil)
  (clon:with-field-values (height width pallet-size) self
    ;; create airless corridor space
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =corridor=) i j]))
    ;; create walls
    (labels ((drop-wall (x y)
	       (prog1 nil
		 [drop-cell self (clone =wall=) y x]))
	     (drop-box (x y)
	       (prog1 nil 
		 [drop-cell self (clone =tech-box=) y x :no-collisions t])))
      ;; create border around world
      (trace-rectangle #'drop-wall
		       0 0 width height)
      ;; drop wall blocks ("pallets")
      (let ((imax (1+ (truncate (/ width pallet-size))))
	    (jmax (1+ (truncate (/ height pallet-size)))))
	(dotimes (i imax)
	  (dotimes (j jmax)
	    ;; don't wall in the player
	    (when (not (= 0 i j))
	      (trace-rectangle #'drop-wall
			       (+ (random 3)
				  (* pallet-size i))
			       (+ (random 4) 
				  (* pallet-size j))
			       (random pallet-size)
			       (random pallet-size)
			       :fill)))))
      ;; drop groups of boxes
      (dotimes (i 34)
	(trace-rectangle #'drop-box (random height) (random width)
		       (+ 16 (random 14)) (+ 4 (random 3)) :fill)))
    ;; drop enemies
    (dotimes (i 40)
      (let ((row (random height))
	    (column (random width)))
	[drop-cell self (clone =berserker=) row column :loadout t :no-collisions t]))
    (dotimes (i 30) 
      [drop-cell self (clone =biclops=) (+ 60 (random (- height 80)))
		 (random width) :loadout t :no-collisions t])
    (dotimes (i 30)
      [drop-cell self (clone =scanner=) (+ 100 (random (- height 100)))
		 (random width) :loadout t :no-collisions t])
    (dotimes (i 20)
      [drop-cell self (clone =rook=) (+ 150 (random (- height 150))) (random width) :loadout t :no-collisions t])
    ;; drop dead crewmembers to ransack
    (dotimes (i 60) 
      [drop-cell self (clone =crew-member=) (random height) (random width) :loadout t :no-collisions t])
    ;; drop other stuff
    (dotimes (n 35)
      [drop-cell self (clone =med-hypo=) (random height) (random height) :no-collisions t])
    (dotimes (i 25)
      [drop-cell self (clone =oxygen-tank=) (random height) (random width) :no-collisions t])
    ;; 
    (setf *station-base-count* 0)
    (loop do (paint-station-piece self (+ 150 (random 140)) (random width) 10)
       while (< *station-base-count* 5))

    (dotimes (i 20)
      [drop-cell self (clone =energy=) (random height) (random width) :no-collisions t])
    (dotimes (i 4)
      [drop-cell self (clone =med-pack=) (random height) (random width) :no-collisions t])
    (dotimes (i 4)
      [drop-cell self (clone =level-up=) (random height) (random width) :no-collisions t])
    (dotimes (i 4)
      [drop-cell self (clone =defense-up=) (random height) (random width) :no-collisions t])
    (dotimes (i 4)
      [drop-cell self (clone =speed-up=) (random height) (random width) :no-collisions t])
    (dotimes (i 5)
      [drop-cell self (clone =muon-pistol=) (random height) (random width) :no-collisions t])
    (dotimes (i 5)
      [drop-cell self (clone =ion-shield=) (random height) (random width) :no-collisions t])
;;    [drop-cell self (clone =rusty-wrench=) 3 2 :no-collisions nil]
    (dotimes (i 20) 
      [drop-cell self (clone =mine=) (random height) (random width) :no-collisions t])))

;;; Controlling the game.

(define-prototype invader-prompt (:parent rlx:=prompt=))

(defparameter *qwerty-keybindings*
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
    ("0" nil "equip 0 .")
    ("1" nil "equip 1 .")
    ("0" (:control) "drop-item 0 .")
    ("1" (:control) "drop-item 1 .")
    ("2" nil "activate-equipment :belt .")
    ("Q" (:control) "quit .")))

(defparameter *alternate-qwerty-keybindings*
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
    ("0" nil "equip 0 .")
    ("1" nil "equip 1 .")
    ("0" (:control) "drop-item 0 .")
    ("1" (:control) "drop-item 1 .")
    ("2" nil "activate-equipment :belt .")
    ("Q" (:control) "quit .")))

;; g c r
;;  \|/
;; h-.-n
;;  /|\ 
;; m w v

(defparameter *dvorak-keybindings*
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
    ("0" nil "equip 0 .")
    ("1" nil "equip 1 .")
    ("0" (:control) "drop-item 0 .")
    ("1" (:control) "drop-item 1 .")
    ("2" nil "activate-equipment :belt .")
    ("Q" (:control) "quit .")))

(define-method install-keybindings invader-prompt ()
  (let ((keys (ecase rlx:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:alternate-qwerty *alternate-qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k))))

;;; A character status widget.

(define-prototype status (:parent rlx:=formatter=)
  (character :documentation "The character cell."))

(define-method set-character status (character)
  (setf <character> character))

(define-method print-stat status (stat-name &key warn-below)
  (let* ((stat (field-value stat-name <character>))
	 (value [stat-value <character> stat-name]))
    (destructuring-bind (&key min max base delta) stat
      (let ((color (if (and (numberp warn-below)
			    (< value warn-below))
		       ".red"
		       ".gray20")))
	[print self (symbol-name stat-name)
	       :foreground ".white"]
	[print self " "]
	[print self (format nil "~S" value) 
	       :foreground ".yellow"
	       :background color]
	[print self " "]))))

(define-method print-equipment-slot status (slot-name)
  [print self (symbol-name slot-name)]
  [print self ": "]
  (let* ((item [equipment-slot <character> slot-name]))
    (if item
	(clon:with-field-values (name tile) item
	  [print self nil :image tile]
	  [print self " "]
	  [print self name]
	  [print self "  "])
	[print self "EMPTY  "])))

(define-method print-inventory-slot status (slot-number)
  [print self (format nil "[~D]: " slot-number)]
  (let ((item [item-at <character> slot-number]))
    (if item
	(clon:with-field-values (name tile) item
				[print self nil :image tile]
				[print self " "]
				[print self (get-some-object-name item)]
				[print self "  "])
	[print self "EMPTY  "])))

(define-method update status ()
  [delete-all-lines self]
  (let ((char <character>))
    [print self "  Statistics:  "]
    [print-stat self :hit-points :warn-below 40]
    [print self " "]
    [print-stat self :oxygen :warn-below 40]
    [print self " "]
    [print-stat self :energy :warn-below 50]
    [print self " "]
    [print-stat self :strength :warn-below 10]
    [print self " "]
    [print-stat self :defense :warn-below 10]
    [print self " "]
    [print-stat self :speed :warn-below 2]
    [println self " "]
    [print self "  Equipment:  "]
    [print-equipment-slot self :right-hand]
    [print-equipment-slot self :left-hand]
    [print-equipment-slot self :belt]
    [newline self]
    [print self "  Inventory:  "]
    [print-inventory-slot self 0]
    [print-inventory-slot self 1]
    [newline self]))

;;; Splash screen for titles.

(defvar *play-widgets*)

(define-prototype splash (:parent =widget=))

(define-method render splash ()
  (rlx:draw-resource-image "invader-splash" 0 0 
			   :destination <image>))

(define-method dismiss splash ()
  (set-music-volume 255)	       
  (play-music "xiomacs2" :loop t)
  (apply #'rlx:install-widgets *play-widgets*))

(define-prototype splash-prompt (:parent =prompt=)
  (default-keybindings :initform '(("SPACE" nil "dismiss ."))))

;;; Main program.

(defun invader ()
  (setf clon:*send-parent-depth* 2)
  (rlx:set-screen-height 600)
  (rlx:set-screen-width 800)
  (rlx:disable-timer)
  (let* ((prompt (clone =invader-prompt=))
	 (world (clone =factory-world=))
	 (player (clone =player=))
	 (viewport (clone =viewport=))
	 (narrator (clone =narrator=))
	 (status (clone =status=))
	 (splash (clone =splash=))
	 (splash-prompt (clone =splash-prompt=)))
    (setf *active-world* world)
    ;;
    [resize splash :height 300 :width 420]
    [move splash :x 200 :y 100]
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
    [generate world]
    [set-player world player]
    [drop-cell world player 1 1 :loadout t]
    ;;
    [resize status :height 60 :width 800]
    [move status :x 5 :y 0]
    [set-character status player]
    ;;
    [set-world viewport world]
    [resize viewport :height 460 :width 800]
    [move viewport :x 0 :y 60]
    [set-origin viewport :x 0 :y 0 :height 27 :width 50]
    [adjust viewport]
    ;;
    [resize narrator :height 80 :width 800]
    [move narrator :x 0 :y 520]
    [set-narrator world narrator]
    [set-verbosity narrator 0]
    ;;
    [start world]
    ;;
    (play-music "theme" :loop t)
    (setf *play-widgets* (list prompt status viewport narrator))
    (install-widgets splash-prompt splash)))

(invader)

;;; invader.lisp ends here

