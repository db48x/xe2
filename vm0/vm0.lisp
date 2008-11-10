;;; vm0.lisp --- void mission zero: a common lisp roguelike

;; Copyright (C) 2006, 2007, 2008  David O'Toole

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

;;; Code:

(eval-when (:execute :load-toplevel :compile-toplevel) 
  (require :rlx))

(defpackage :vm0
  (:documentation "A sci-fi roguelike for RLX.")
  (:use :rlx :common-lisp)
  (:export vm0))

(in-package :vm0)

(define-prototype terrain (:parent rlx:=cell=)
  (tile :initform "mars-terrain-flat2"))

(define-prototype terrain2 (:parent rlx:=cell=)
  (tile :initform "mars-terrain-icy"))

(define-prototype gray-brick (:parent rlx:=cell=)
  (categories :initform '(:obstacle :opaque))
  (tile :initform "gray-brick"))

;;; the shock probe

(define-prototype shock-probe (:parent rlx:=cell=)
  (name :initform "Shock probe")
  (categories :initform '(:item :weapon :equipment :builtin))
  (tile :initform "shock-probe")
  (attack-power :initform (make-stat :base 5))
  (attack-cost :initform (make-stat :base 5))
  (accuracy :initform (make-stat :base 80))
  (weight :initform 3000)
  (equip-for :initform '(:robotic-arm)))

;;; the purple-perceptor

;; move in a straight line until hitting an obstacle.
;; then choose a random direction and try again

(define-prototype purple-perceptor (:parent rlx:=cell=)
  (categories :initform '(:actor :obstacle :opaque))
  (equipment-slots :initform '(:robotic-arm))
  (speed :initform (make-stat :base 7 :min 7))
  (movement-cost :initform (make-stat :base 3))
  (tile :initform "purple-perceptor")
  (direction :initform (rlx:random-direction))
  (hit-points :initform (make-stat :base 10 :min 0 :max 10)))

(define-method run purple-perceptor ()
  (if [obstacle-in-direction-p *active-world* <row> <column> <direction>]
      (setf <direction> (rlx:random-direction))
      [queue>>move self <direction>]))

;;; the mysterious crystal
  
(define-prototype crystal (:parent rlx:=cell=)
  (name :initform "Turquoise crystal")
  (categories :initform '(:item))
  (tile :initform "turquoise-crystal"))

;;; the red-perceptor

;; seek player

(define-prototype red-perceptor (:parent rlx:=cell=)
  (strength :initform (make-stat :base 16 :min 0 :max 30))
  (dexterity :initform (make-stat :base 11 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (categories :initform '(:actor :obstacle :opaque :equipper))
  (equipment-slots :initform '(:robotic-arm))
  (max-items :initform (make-stat :base 3))
  (speed :initform (make-stat :base 5))
  (movement-cost :initform (make-stat :base 5))
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (hit-points :initform (make-stat :base 7 :min 0 :max 10))
  (tile :initform "red-perceptor"))

(define-method initialize red-perceptor ()
  [make-inventory self]
  [make-equipment self])

(define-method loadout red-perceptor ()
  (let ((probe (clone =shock-probe=)))
    [equip self [add-item self probe]]))

(define-method run red-perceptor ()
  (let ((direction [direction-to-player *active-world* <row> <column>]))
    (if [adjacent-to-player *active-world* <row> <column>]
	(progn
	  [queue>>attack self direction])
	(progn 
	  (when [obstacle-in-direction-p *active-world* <row> <column> direction]
	    (setf direction (random-direction)))
	  [queue>>move self direction]))))

;;; the rusty wrench

(define-prototype rusty-wrench (:parent rlx:=cell=)
  (name :initform "Rusty wrench")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "rusty-wrench")
  (attack-power :initform (make-stat :base 4)) ;; points of raw damage
  (attack-cost :initform (make-stat :base 5))
  (accuracy :initform (make-stat :base 60)) ;; percent
  (weight :initform 10000) ;; grams
  (equip-for :initform '(:left-hand :right-hand)))

;;; the player

(define-prototype player (:parent rlx:=cell=)
  (tile :initform "player")
  (category :initform '(:actor :container :player :obstacle))
  (actions :initform '(:move :take :equip :cursor-next :cursor-previous :drop :dequip :attack))
  (speed :initform (make-stat :base 10 :min 0 :max 21))
  (strength :initform (make-stat :base 16 :min 0 :max 30))
  (attacking-with :initform :right-hand)
  (max-weight :initform (make-stat :base 25))
  (max-items :initform (make-stat :base 5))
  (movement-cost :initform (make-stat :base 7))
  (equipment :initform nil)
  (dexterity :initform (make-stat :base 11 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (hit-points :initform (make-stat :base 25 :min 0 :max 100))
  (energy :initform (make-stat :base 1200 :min 0 :max 1800))
  (oxygen :initform (make-stat :base 1000 :min 0 :max 1200)))

(define-method initialize player ()
  [make-inventory self]
  [make-equipment self])

;;; the game-specific controls

(define-prototype vm0-prompt (:parent rlx:=prompt=))

(defparameter *vm0-keybindings*
  '(("KP7" nil "move :northwest .")
    ("KP8" nil "move :north .")
    ("KP9" nil "move :northeast .")
    ("KP4" nil "move :west .")
    ("KP6" nil "move :east .")
    ("KP1" nil "move :southwest .")
    ("KP2" nil "move :south .")
    ("KP3" nil "move :southeast .")
    
    ("KP7" (:alt) "attack :northwest .")
    ("KP8" (:alt) "attack :north .")
    ("KP9" (:alt) "attack :northeast .")
    ("KP4" (:alt) "attack :west .")
    ("KP6" (:alt) "attack :east .")
    ("KP1" (:alt) "attack :southwest .")
    ("KP2" (:alt) "attack :south .")
    ("KP3" (:alt) "attack :southeast .")

    ("T" nil "take .")
    ("E" nil "equip 0 .")))
    
  
(define-method install-keybindings vm0-prompt ()
  (dolist (k *vm0-keybindings*)
    (apply #'bind-key-to-prompt-insertion self k)))

;;; a character status widget

(define-prototype status (:parent rlx:=formatter=)
  (character :documentation "The character cell."))

(define-method set-character status (character)
  (setf <character> character))

(define-method update status ()
  [delete-all-lines self]
  (let ((char <character>))
    [println self (field-value :name char)]
    [println self (format nil "AP: ~S" (field-value :action-points char))]
    [println self (format nil "HP: ~S" [stat-value char :hit-points])]
    [println self (format nil "OX: ~S" [stat-value char :oxygen])]
    [println self (format nil "EN: ~S" [stat-value char :energy])]))

;;; our world

(define-prototype mars-world (:parent rlx:=world=)
  (ambient-light :initform :total))

(define-method generate mars-world ()
  ;; create world
  (dotimes (i 100)
    (dotimes (j 100)
      [drop-cell self (clone (if (> (random 100) 10)
				  =terrain= =terrain2=))
		 i j 
		 :loadout]))
  (dotimes (n 100)
    [drop-cell self (clone =gray-brick=) (random 100) (random 100) :loadout])
  ;; (dotimes (i 65)
  ;;   [drop-cell self (clone =purple-perceptor=) (random 100) (random 100) :loadout])
  (dotimes (i 12)
    [drop-cell self (clone =red-perceptor=) (random 100) (random 100) :loadout])
  (dotimes (i 12)
    [drop-cell self (clone =purple-perceptor=) (random 100) (random 100) :loadout])
  (dotimes (i 12)
    [drop-cell self (clone =crystal=) (random 100) (random 100) :loadout])
  (dotimes (i 8)
    [drop-cell self (clone =shock-probe=) (random 100) (random 100) :loadout])
  (dotimes (i 12)
    [drop-cell self (clone =rusty-wrench=) (random 100) (random 100) :loadout])
  (dotimes (i 4)
    (labels ((drop-brick (x y)
	       [drop-cell self (clone =gray-brick=) y x])
	     (drop-crystal (x y)
	       [drop-cell self (clone =crystal=) y x]))
      (trace-octagon #'drop-brick 
		     (+ 20 (random 60))
		     (+ 20 (random 60))
		     (+ 3 (random 5)))))
  ;; add player 
  [drop-cell self <player> 10 10])

;;; putting it all together

(defun vm0 ()
  (setf rlx:*screen-height* 600)
  (setf rlx:*screen-width* 800)
  (let* ((prompt (clone rlx:=prompt=))
	 (player-prompt (clone =vm0-prompt=))
	 (world (clone =mars-world= :height 100 :width 100))
	 (player (clone =player=))
	 (status (clone =status=))
	 (narrator (clone rlx:=narrator=))
	 (browser (clone rlx:=browser=))
	 (viewport nil))
    (setf *active-world* world)
    ;; status
    [resize status :height 70 :width 220]
    [move status :x 405 :y 0]
    [set-character status player]
    ;; browser
    [resize browser :height 500 :width 220]
    [move browser :x 555 :y 60]
    [set-collection browser (field-value :inventory player)]
    ;; system prompt
    [install-keybindings prompt]
    [resize prompt :height 30 :width 400]
    [move prompt :x 0 :y 570]
    [set-mode prompt :forward]
    [set-receiver prompt world]
    ;; vm0 command prompt
    [install-keybindings player-prompt]
    [resize player-prompt :height 30 :width 400]
    [move player-prompt :x 0 :y 0]
    [set-receiver player-prompt world]
    [hide player-prompt]
    ;; narration window
    [resize narrator :height 240 :width 500]
    [move narrator :x 0 :y 330]
    [set-narrator world narrator]

    ;; tie it together
    [set-player world player]
    [set-browser world browser]
    [start world]
    (setf viewport (clone =viewport=))
    [set-world viewport world]
    [resize viewport :height 320 :width 400]
    [move viewport :x 0 :y 0]
    [set-origin viewport :x 0 :y 0 :height 20 :width 25]

    (install-widgets (list prompt browser player-prompt viewport status narrator))
    ))
    
(vm0)

;;; vm0.lisp ends here
