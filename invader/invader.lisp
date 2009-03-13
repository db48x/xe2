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
  (tile :initform "wall")
  (categories :initform '(:obstacle)))

(defcell corridor
  (tile :initform "corridor"))

;;; Moving in a corridor uses up oxygen.

(define-method step corridor (stepper)
  [>>stat-effect stepper :oxygen -1])

;;; You can refill your oxygen stores with these tanks.

(defcell oxygen-tank
  (tile :initform "oxygen-tank"))

(define-method step oxygen-tank (stepper)
  [>>stat-effect stepper :oxygen 200]
  [>>die self])

;;; There are also energy tanks.

(define-prototype energy (:parent rlx:=cell=)
  (tile :initform "energy")
  (name :initform "Energy Tank"))

(define-method step energy (stepper)
  (when (has-field :energy stepper)
    [queue>>stat-effect stepper :energy 100]
    [queue>>die self]))

;;; The player is depicted as a red diamond.

(defcell player 
  (tile :initform "player")
  (categories :initform '(:actor :player :obstacle :target :container))
  ;; action points and movement
  (speed :initform (make-stat :base 10 :min 1 :max 20))
  (movement-cost :initform (make-stat :base 7))
  ;; vital stats
  (hit-points :initform (make-stat :base 50 :min 0 :max 100)) 
 ;; inventory-related data
  (max-items :initform (make-stat :base 1))
  ;; equipment-related slots
  (attacking-with :initform :right-hand)
  (firing-with :initform :left-hand)
  ;; other stats
  (energy :initform (make-stat :base 800 :min 0 :max 1000))
  (oxygen :initform (make-stat :base 1000 :min 0 :max 1200))
  ;; default is do not generate step events; this turns it on
  (stepping :initform t))

;;; Medical healing hypo

(defcell med-hypo 
  (categories :initform '(:item))
  (tile :initform "med-hypo"))

(define-method step med-hypo (stepper)
  [queue>>stat-effect stepper :hit-points 12]
  [queue>>die self])

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

;;; The Berserker is a relatively simple AI enemy.

;; Run in a straight line until hitting an obstacle.
;; Then choose a random direction and try again.
;; If the player gets close, try and attack him.

(defcell berserker 
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
  (when (> 6 (random 10))
    [drop self (clone =energy=)])
  [parent>>die self])

(define-method loadout berserker ()
  (let ((probe (clone =shock-probe=)))
    [equip self [add-item self probe]]))

;;; The sinister robot factory is defined here. 

(define-prototype factory-world (:parent rlx:=world=)
  (width :initform 60)
  (height :initform 60)
  (pallet-size :initform 9))

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
		 [drop-cell self (clone =wall=) y x 
			    :loadout nil :no-collisions nil])))
      ;; create border around world
      (trace-rectangle #'drop-wall
		       0 0 height width)
      ;; drop pallets
      (let ((imax (1- (truncate (/ width pallet-size))))
	    (jmax (1- (truncate (/ height pallet-size)))))
	(dotimes (i imax)
	  (dotimes (j jmax)
	    (trace-rectangle #'drop-wall
			     (+ (random 3)
				(* pallet-size i))
			     (+ (random 4) 
				(* pallet-size j))
			     (random pallet-size)
			     (random pallet-size)
			     :fill)))))
    ;; drop enemies
    (dotimes (i 10)
      (let ((row (random 50))
	    (column (random 50)))
	(when (not [obstacle-at-p self row column])
	  [drop-cell self (clone =berserker=) row column :loadout t :no-collisions t])))
    ;; drop other stuff
    (dotimes (n 4)
      [drop-cell self (clone =med-hypo=) (random height) (random height)])
    (dotimes (i 12)
      [drop-cell self (clone =oxygen-tank=) (random height) (random width)])))

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

;;; Main program.

(defun invader ()
  (setf rlx:*screen-height* 600)
  (setf rlx:*screen-width* 800)
  (let* ((prompt (clone =invader-prompt=))
	 (world (clone =factory-world=))
	 (player (clone =player=))
	 (viewport (clone =viewport=))
	 (narrator (clone =narrator=)))
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
    [drop-cell world player 1 1]
    ;;
    [set-world viewport world]
    [resize viewport :height 500 :width 800]
    [move viewport :x 0 :y 0]
    [set-origin viewport :x 0 :y 0 :height 30 :width 50]
    [adjust viewport]
    ;;
    [resize narrator :height 100 :width 800]
    [move narrator :x 0 :y 500]
    [set-narrator world narrator]
    ;;
    [start world]
    ;;
    (install-widgets (list prompt viewport narrator))))

(invader)


;;; invader.lisp ends here
