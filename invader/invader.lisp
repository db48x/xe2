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

;;; Code:

(defpackage :invader
  (:documentation "A sci-fi roguelike for Common Lisp.")
  (:use :rlx :common-lisp)
  (:export invader))

(in-package :invader)

;; Every space is either a wall or a corridor. 

(defcell wall
  (tile :initform "wall")
  (categories :initform '(:obstacle)))

(defcell corridor
  (tile :initform "black"))

;; Moving in a corridor uses up oxygen.

(define-method step corridor (stepper)
  (when (has-field :oxygen stepper)
    [>>stat-effect stepper :oxygen -1]))

;; You can refill your oxygen stores with these tanks.

(defcell oxygen-tank
  (tile :initform "oxygen-tank"))

(define-method step oxygen-tank (stepper)
  (when (has-field :oxygen stepper)
    [>>stat-effect stepper :oxygen 200]
    [>>die self]))

;; The player is depicted as a red diamond.

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


;;; invader.lisp ends here
