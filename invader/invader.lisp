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

;;; Commentary:

;; Invader is a mini roguelike intended as an example game module for
;; RLX, which will (hopefully) also be fun to play. 

;; Basic features:

;;   - Oldschool Atari 5200-style graphics.
;;   - Infiltrate an airless enemy installation and destroy all the robots.
;;   - Level gen params: color scheme, size, complexity, enemy/object density...
;;   - Melee combat with wrench.
;;   - Ranged combat with energy-using particle gun.
;;   - Some enemies have particle shields and require melee hits to kill.
;;   - Rooks require ranged hits, their armor is too strong for wrenches.
;;   - Oxygen is constantly depleting, refills are required.
;;   - Minimal inventory management, one slot, all pickups are "activate-on-step"
;;   - You can't win; it just gets harder until you die, and your score is tallied. 

;;; Code:

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
  (tile :initform "black"))

;;; Moving in a corridor uses up oxygen.

(define-method step corridor (stepper)
  [>>stat-effect stepper :oxygen -1])

;;; You can refill your oxygen stores with these tanks.

(defcell oxygen-tank
  (tile :initform "oxygen-tank"))

(define-method step oxygen-tank (stepper)
  [>>stat-effect stepper :oxygen 200]
  [>>die self])

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

;;; The sinister robot factory is defined here. 

(define-prototype factory-world (:parent rlx:=world=)
  (width :initform 60)
  (height :initform 60)
  (pallet-size :initform 12))

(define-method generate factory-world (&optional parameters)
  (clon:with-field-values (height width pallet-size) self
    ;; create airless corridor space
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =corridor=) i j]))
    ;; create walls
    (let ((imax (1- (truncate (/ width pallet-size))))
	  (jmax (1- (truncate (/ height pallet-size)))))
      (dotimes (i imax)
	(dotimes (j jmax)
	  (labels ((drop-wall (x y)
		     [drop-cell self (clone =wall=) y x]))
	    (when (not (= 0 i j))
	      (trace-rectangle #'drop-wall
			       0 0 height width)
	      (trace-rectangle #'drop-wall
			       (+ (random 3)
				  (* pallet-size i))
			       (+ (random 4) 
				  (* pallet-size j))
			       (random pallet-size)
			       (random pallet-size)
			       :fill))))))))

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

;;; Main program

(defun invader ()
  (setf rlx:*screen-height* 600)
  (setf rlx:*screen-width* 800)
  (let* ((prompt (clone =invader-prompt=))
	 (world (clone =factory-world=))
	 (player (clone =player=))
	 (viewport (clone =viewport=)))
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
    [resize viewport :height 600 :width 800]
    [move viewport :x 0 :y 0]
    [set-origin viewport :x 0 :y 0 :height 20 :width 25]
    [adjust viewport]
    ;;
    [start world]
    ;;
    (install-widgets (list prompt viewport))))

(invader)

    
    




;;; invader.lisp ends here
