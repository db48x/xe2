;; blast.lisp --- a micro shmup in common lisp

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

;; Blast Tactics is a micro shoot-em-up in Common Lisp. 

;; You can shoot asteroids or destroy them with your trail.  Powerups
;; extend the trail, enabling higher scores!

;;; Packaging

(defpackage :blast
  (:documentation "Blast Tactics: A sci-fi roguelike for Common Lisp.")
  (:use :rlx :common-lisp)
  (:export blast))

(in-package :blast)

;;; Empty space.

(defcell space 
  (tile :initform "space"))

;;; An explosion.

(define-prototype explosion (:parent rlx:=cell=)
  (name :initform "Explosion")
  (categories :initform '(:actor))
  (tile :initform "explosion")
  (speed :initform (make-stat :base 10))
  (damage-per-turn :initform 10)
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
  (when (zerop <clock>)
    [die self]))

(define-method step trail (stepper)
  [drop self (clone =explosion=)]	       
  [damage stepper 5])

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

;;; Your ship.

(defcell ship 
  (tile :initform "player-ship-north")
  (name :initform "Olvac 2")
  (speed :initform (make-stat :base 10))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hit-points :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (trail-length :initform 8)
  (stepping :initform t)
  (lives :initform 3)
  (score :initform 0)
  (categories :initform '(:actor :player :obstacle :target :container :light-source))
  (equipment-slots :initform '(:gun :trail))
  (boost-clock :initform 0))

(define-method quit ship ()
  (rlx:quit :shutdown))

(define-method wait ship ()
  [stat-effect self :oxygen -1]
  [expend-action-points self <action-points>])

(define-method move ship (direction)
  [drop self (clone =trail= 
		    :direction direction 
		    :clock <trail-length>)]
  [parent>>move self direction])

(define-method loadout ship ()
  [make-inventory self]
  [make-equipment self])

(define-method run ship ()
 nil)

(define-method die ship ()
  (let ((skull (clone =skull=)))
    [drop-cell *active-world* skull <row> <column> :loadout t :no-collisions nil]
    (setf <action-points> 0)
    [add-category self :dead]
    [>>delete-from-world self]
    [>>narrateln :narrator "You die."]
    [set-player *active-world* skull]))

;;; An asteroid.

(defcell asteroid
  (categories :initform '(:actor))
  (hit-points :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform :north)
  (stepping :initform t))

(define-method initialize asteroid (&key speed direction color)
  (setf <speed> (make-stat :base speed))
  (setf <direction> direction)
  (setf <tile>
	(ecase color
	  (:red "asteroid-red")
	  (:blue "asteroid-blue")
	  (:brown "asteroid-brown"))))

(define-method run asteroid ()
  (if (<= [stat-value self :hit-points] 0)
      [die self]
      [move self <direction>]))

(define-method step asteroid (stepper)
  [damage stepper 5]
  (setf <direction> (rlx:random-direction)))

(define-method move asteroid (direction)
  (when [obstacle-in-direction-p *active-world* <row> <column> direction]
    (setf <direction> (rlx:random-direction)))
  [parent>>move self direction])

;;; The endless void.

(define-prototype void-world (:parent rlx:=world=)
  (width :initform 200)
  (height :initform 200)
  (asteroid-count :initform 100)
  (ambient-light :initform :total))

(define-method generate void-world (&optional parameters)
  (declare (ignore parameters))
  (clon:with-field-values (height width) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =space=) i j]))
    (dotimes (i <asteroid-count>)
      [drop-cell self (clone =asteroid= 
			     :speed (+ 3 (random 8))
			     :direction (rlx:random-direction)
			     :color (nth (random 3)
					 '(:red :blue :brown)))
		 (random height) (random width)])))

;;; Controlling the game.

(define-prototype blast-prompt (:parent rlx:=prompt=))

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

(define-method install-keybindings blast-prompt ()
  (let ((keys (ecase rlx:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:alternate-qwerty *alternate-qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k)))
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *active-world* :timer])])

;;; Main program. 

(defun blast ()
  (setf clon:*send-parent-depth* 2)
  (rlx:set-screen-height 600)
  (rlx:set-screen-width 800)
  (rlx:set-frame-rate 30)
  (rlx:set-timer-interval 15)
  (rlx:enable-timer)
  (rlx:enable-held-keys 0 15)
  (let* ((prompt (clone =blast-prompt=))
	 (world (clone =void-world=))
	 (narrator (clone =narrator=))
	 (player (clone =ship=))
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
    [drop-cell world player 25 25 :loadout t]
    ;;
    [resize narrator :height 80 :width 800]
    [move narrator :x 0 :y 520]
    [set-narrator world narrator]
    [set-verbosity narrator 0]
   ;;
    (setf (clon:field-value :tile-size viewport) 10)
    [set-world viewport world]
    [resize viewport :height 460 :width 800]
    [move viewport :x 0 :y 60]
    [set-origin viewport :x 0 :y 0 :height 46 :width 80]
    [adjust viewport]
    ;;
    [start world]
    ;;
	 ;;    (play-music "void" :loop t)
    
    (install-widgets  prompt viewport)))

(blast)
