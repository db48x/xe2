;;; invader.lisp --- you against the robots

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
;;   - You can't win; it just gets harder until you die, and your score is tallied. 

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
  (tile :initform "oxygen-tank"))

(define-method step oxygen-tank (stepper)
  (when [is-player stepper]
    [>>stat-effect stepper :oxygen 40]
    [>>die self]))

;;; There are also energy tanks for replenishing ammo.

(define-prototype energy (:parent rlx:=cell=)
  (tile :initform "energy")
  (name :initform "Energy Tank"))

(define-method step energy (stepper)
  (when [is-player stepper]
    (when (has-field :energy stepper)
      [>>stat-effect stepper :energy 100]
      [>>die self])))

;;; The player is depicted as a red diamond.

(defcell player 
  (tile :initform "player")
  (name :initform "Player")
  (categories :initform '(:actor :player :obstacle :target :container))
  ;; action points and movement
  (speed :initform (make-stat :base 7 :min 1 :max 20))
  (movement-cost :initform (make-stat :base 7))
  ;; vital stats
  (hit-points :initform (make-stat :base 100 :min 0 :max 100)) 
  (dexterity :initform (make-stat :base 11 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (strength :initform (make-stat :base 16 :min 0 :max 30))
  ;; inventory-related data
  (max-items :initform (make-stat :base 1))
  ;; equipment-related slots
  (attacking-with :initform :right-hand)
  (firing-with :initform :left-hand)
  ;; other stats
  (energy :initform (make-stat :base 800 :min 0 :max 1000))
  (oxygen :initform (make-stat :base 100 :min 0 :max 100))
  ;; default is do not generate step events; this turns it on
  (stepping :initform t))

(define-method loadout player ()
  [make-inventory self]
  [make-equipment self])

;;; When you run out of oxygen, you die. 

(define-method run player ()
  (when (<= [stat-value self :oxygen] 0)
    [die self]))

;;; When you fight a monster close-up, you use more oxygen.

(define-method attack player (target)
  [>>stat-effect self :oxygen -3]
  [parent>>attack self target])

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
    [parent>>die self]
    [>>narrateln :narrator "You die."]
    [set-player *active-world* skull]))

;;; The medical healing hypo restores hit points.

(defcell med-hypo 
  (categories :initform '(:item))
  (tile :initform "med-hypo")
  (name :initform "Medical Hypo"))

(define-method step med-hypo (stepper)
  (when [is-player stepper]
    [>>stat-effect stepper :hit-points 30]
    [>>die self]))

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
    [>>take stepper :direction :here :category :item]
    [>>equip stepper 0]))

;;; An explosion

(define-prototype explosion (:parent rlx:=cell=)
  (name :initform "Explosion")
  (categories :initform '(:actor))
  (tile :initform "explosion")
  (speed :initform (make-stat :base 10))
  (damage-per-turn :initform 7)
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
  (hit-points :initform (make-stat :base 12 :min 0 :max 10)))

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
  (strength :initform (make-stat :base 28 :min 0 :max 30))
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
  (hit-points :initform (make-stat :base 11 :min 0 :max 10))
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
    (if (> 5 (random 10))
	[drop self (clone =energy=)]
	[drop self (clone =med-hypo=)]))
  (if (> 5 (random 10))
      [drop self (clone =explosion=)])
  [parent>>die self])
 
;;; Glittering flash gives clues on locations of explosions/damage

(define-prototype flash (:parent rlx:=cell=)
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

;;; The exploding mine

(define-prototype mine (:parent rlx:=cell=)
  (name :initform "Contact mine")
  (categories :initform '(:item :target))
  (tile :initform "mine"))

(define-method run mine ()
  nil)

(define-method explode mine ()
  (dolist (dir (list :here :north :south :east :west))
    (multiple-value-bind (r c)
      (step-in-direction <row> <column> dir)
      (when [in-bounds-p *active-world* r c]
  	[drop-cell *active-world* (clone =explosion=) r c :no-collisions nil])))
  [die self])

(define-method step mine (stepper)
  (declare (ignore stepper))
  [explode self])

(define-method damage mine (damage-points)
  (declare (ignore damage-points))
  [explode self])

;;; Some destructible blocks

(defcell tech-box
  (tile :initform "tech-box")
  (categories :initform '(:obstacle :opaque :pushable :destructible))
  (hit-points :initform (make-stat :base 10 :min 0)))

(defcell tech-box-debris
  (tile :initform "tech-box-debris"))

(define-method die tech-box ()
  [>>drop self (clone =tech-box-debris=)]
  (when (<= (random 10) 2)
    [>>drop self 
	    (case (random 3)
	      (0 =oxygen-tank=)
	      (1 =energy=)
	      (2 =med-hypo=))])
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
  (clock :initform 5))

(define-method find-target muon-particle ()
  (let ((target [category-in-direction-p *active-world* 
					 <row> <column> <direction>
					 '(:obstacle :target)]))
    (if target
	(progn
	  [queue>>expend-default-action-points self]
	  [queue>>drop self (clone =flash=)]
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

(define-prototype muon-pistol (:parent rlx:=cell=)
  (name :initform "Muon energy pistol")
  (tile :initform "gun")
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:left-hand))
  (weight :initform 7000)
  (accuracy :initform (make-stat :base 90))
  (attack-power :initform (make-stat :base 18))
  (attack-cost :initform (make-stat :base 4))
  (energy-cost :initform (make-stat :base 10)))

(define-method fire muon-pistol (direction)
  (if [expend-energy <equipper> 10]
      (let ((muon (clone =muon-particle=)))
	[queue>>drop <equipper> muon]
	[queue>>impel muon direction])
      (message "Not enough energy to fire.")))

(define-method step muon-pistol (stepper)
  (when [is-player stepper]
    [>>take stepper :direction :here :category :item]
    [>>equip stepper 0]))

;;; The sinister robot factory is defined here. 

(define-prototype factory-world (:parent rlx:=world=)
  (width :initform 120)
  (height :initform 120)
  (pallet-size :initform 12))

(define-method generate factory-world (&optional parameters)
  (declare (ignore parameters))
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
		 [drop-cell self (clone =tech-box=) y x])))
      ;; create border around world
      (trace-rectangle #'drop-wall
		       0 0 height width)
      ;; drop pallets
      (let ((imax (1- (truncate (/ width pallet-size))))
	    (jmax (1- (truncate (/ height pallet-size)))))
	(dotimes (i imax)
	  (dotimes (j jmax)
	    (when (not (= 0 i j))
	      (trace-rectangle #'drop-wall
			       (+ (random 3)
				  (* pallet-size i))
			       (+ (random 4) 
				  (* pallet-size j))
			       (random pallet-size)
			       (random pallet-size)
			       :fill)))))
      ;; drop columns
      (dotimes (i 13)
	(trace-octagon #'drop-box (random height) (random width)
		       (+ 3 (random 8)))))
    ;; drop enemies
    (dotimes (i 15)
      (let ((row (random 50))
	    (column (random 50)))
	[drop-cell self (clone =berserker=) row column :loadout t :no-collisions t]))
    (dotimes (i 7) 
      [drop-cell self (clone =biclops=) (random height) (random width) :loadout t :no-collisions t])
    ;; drop other stuff
    (dotimes (n 20)
      [drop-cell self (clone =med-hypo=) (random height) (random height) :no-collisions t])
    (dotimes (i 45)
      [drop-cell self (clone =oxygen-tank=) (random height) (random width) :no-collisions t])
    (dotimes (i 7)
      [drop-cell self (clone =energy=) (random height) (random width) :no-collisions t])
    (dotimes (i 6)
      [drop-cell self (clone =rusty-wrench=) (random height) (random width) :no-collisions nil])
    (dotimes (i 10)
      [drop-cell self (clone =muon-pistol=) (random height) (random width) :no-collisions nil])
    [drop-cell self (clone =rusty-wrench=) (random 10) (random 10) :no-collisions t]
    (dotimes (i 50) 
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
    ("Y" (:control) "fire :northwest .")
    ("K" (:control) "fire :north .")
    ("U" (:control) "fire :northeast .")
    ("H" (:control) "fire :west .")
    ("L" (:control) "fire :east .")
    ("B" (:control) "fire :southwest .")
    ("J" (:control) "fire :south .")
    ("N" (:control) "fire :southeast .")
    ;;
    ("T" nil "take .")
    ("E" nil "equip 0 .")
    ("1" nil "activate-equipment :belt .")))

;; f t g
;;  \|/
;; d-.-n
;;  /|\ 
;; x h b

(defparameter *dvorak-keybindings*
  '(("F" nil "move :northwest .")
    ("T" nil "move :north .")
    ("G" nil "move :northeast .")
    ("D" nil "move :west .")
    ("N" nil "move :east .")
    ("X" nil "move :southwest .")
    ("H" nil "move :south .")
    ("B" nil "move :southeast .")
    ;;
    ("F" (:alt) "attack :northwest .")
    ("T" (:alt) "attack :north .")
    ("G" (:alt) "attack :northeast .")
    ("D" (:alt) "attack :west .")
    ("N" (:alt) "attack :east .")
    ("X" (:alt) "attack :southwest .")
    ("H" (:alt) "attack :south .")
    ("B" (:alt) "attack :southeast .")
    ;;
    ("F" (:control) "fire :northwest .")
    ("T" (:control) "fire :north .")
    ("G" (:control) "fire :northeast .")
    ("D" (:control) "fire :west .")
    ("N" (:control) "fire :east .")
    ("X" (:control) "fire :southwest .")
    ("H" (:control) "fire :south .")
    ("B" (:control) "fire :southeast .")
    ;;
    ("O" nil "take .") ;; obtain
    ("E" nil "equip 0 .")
    ("1" nil "activate-equipment :belt .")))

(define-method install-keybindings invader-prompt ()
  (let ((keys (ecase rlx:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k))))

;;; A character status widget.

(define-prototype status (:parent rlx:=formatter=)
  (character :documentation "The character cell."))

(define-method set-character status (character)
  (setf <character> character))

(define-method update status ()
  [delete-all-lines self]
  (let ((char <character>))
    [print self (field-value :name char)]
    [print self (format nil "  HP: ~S" [stat-value char :hit-points])]
    [print self (format nil "  OX: ~S" [stat-value char :oxygen])]
    [println self (format nil "  EN: ~S" [stat-value char :energy])]))

;;; Main program.

(defun invader ()
  (setf rlx:*screen-height* 600)
  (setf rlx:*screen-width* 800)
  (let* ((prompt (clone =invader-prompt=))
	 (world (clone =factory-world=))
	 (player (clone =player=))
	 (viewport (clone =viewport=))
	 (narrator (clone =narrator=))
	 (status (clone =status=)))
    (setf *active-world* world)
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
    [resize status :height 20 :width 800]
    [move status :x 5 :y 0]
    [set-character status player]
    ;;
    [set-world viewport world]
    [resize viewport :height 480 :width 800]
    [move viewport :x 0 :y 20]
    [set-origin viewport :x 0 :y 0 :height 27 :width 50]
    [adjust viewport]
    ;;
    [resize narrator :height 100 :width 800]
    [move narrator :x 0 :y 500]
    [set-narrator world narrator]
    [set-verbosity narrator 1]
    ;;
    [start world]
    ;;
    (install-widgets prompt status viewport narrator)))

(invader)


;;; invader.lisp ends here
