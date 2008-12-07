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

;;; tech-stone

(define-prototype tech-stone (:parent rlx:=cell=)
  (tile :initform "tech-stone"))

(define-prototype tech-stone-moss (:parent rlx:=cell=)
  (tile :initform "tech-stone-moss"))

(define-prototype tech-brick-green (:parent rlx:=cell=)
  (tile :initform "green-tech-brick")
  (categories :initform '(:obstacle :opaque :pushable :destructible))
  (hit-points :initform (make-stat :base 10 :min 0)))

(define-prototype tech-brick-green-debris (:parent rlx:=cell=)
  (tile :initform "green-tech-brick-debris"))

(define-method die tech-brick-green ()
  [queue>>drop-cell *active-world* (clone =tech-brick-green-debris=) <row> <column>]
  [parent>>die self])
  
(define-prototype tech-brick-yellow (:parent rlx:=cell=)
  (tile :initform "yellow-tech-brick")
  (categories :initform '(:obstacle :opaque)))

(define-prototype rook (:parent rlx:=cell=)
  (equipment-slots :initform '(:robotic-arm :shoulder-mount))
  (speed :initform (make-stat :base 12))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 4))
  (tile :initform "rook")
  (target :initform nil)
  (hit-points :initform (make-stat :base 10 :min 0 :max 10)))

;;; the gun 

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
  (speed :initform (make-stat :base 10))
  (default-cost :initform (make-stat :base 5))
  (tile :initform "muon")
  (direction :initform :here)
  (clock :initform 5))

(define-method run muon-particle ()
  (setf <tile> (getf *muon-tiles* <direction>))
  (let ((target [category-in-direction-p *active-world* 
					 <row> <column> <direction>
					 '(:obstacle :actor)]))
    (if target
	(progn
	  [queue>>damage target 5]
	  [queue>>die self])
	(progn 
	  [queue>>move self <direction>]
	  [queue>>drop self (clone =muon-trail= <direction>)]))
    (decf <clock>)
    [expend-default-action-points self]
    (when (zerop <clock>)
      [queue>>die self])))

(define-method impel muon-particle (direction)
  (assert (member direction *compass-directions*))
  (setf <direction> direction)
  ;; don't hit the player
  [move self direction])

(define-prototype muon-pistol (:parent rlx:=cell=)
  (name :initform "Xiong Les Fleurs Muon(TM) energy pistol")
  (tile :initform "muon-pistol")
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:left-hand))
  (weight :initform 7000)
  (accuracy :initform (make-stat :base 72))
  (attack-power :initform (make-stat :base 6))
  (attack-cost :initform (make-stat :base 4)))

(define-method fire muon-pistol (direction)
  (let ((muon (clone =muon-particle=)))
    [queue>>drop :player muon]
    [queue>>impel muon direction]))

;; TODO rook cannon

;;; the shock probe

(define-prototype shock-probe (:parent rlx:=cell=)
  (name :initform "Shock probe")
  (categories :initform '(:item :weapon :equipment :builtin))
  (tile :initform "shock-probe")
  (attack-power :initform (make-stat :base 4))
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
  (stepping :initform t)
  (direction :initform (rlx:random-direction))
  (hit-points :initform (make-stat :base 10 :min 0 :max 10)))

(define-method run purple-perceptor ()
  (if [obstacle-in-direction-p *active-world* <row> <column> <direction>]
      (setf <direction> (rlx:random-direction))
      [queue>>move self <direction>]))

;;; electron

(define-prototype electron (:parent rlx:=cell=)
  (tile :initform "electron")
  (categories :initform '(:item)))

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
  (stepping :initform t)
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

;;; the explosion

(define-prototype explosion (:parent rlx:=cell=)
  (name :initform "Explosion")
  (categories :initform '(:actor))
  (tile :initform "explosion")
  (speed :initform (make-stat :base 10))
  (damage-per-turn :initform 7)
  (clock :initform 2))

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
		    [queue>>damage (aref cells x) <damage-per-turn>]
		    (decf x)))))))

;;; the mine

(define-prototype mine (:parent rlx:=cell=)
  (name :initform "Vanguara XR-1 Contact mine")
  (categories :initform '(:item))
  (tile :initform "mine"))

(define-method explode mine ()
  (dolist (dir (list :here :north :south :east :west))
    (multiple-value-bind (r c)
      (step-in-direction <row> <column> dir)
      (when [in-bounds-p *active-world* r c]
  	[drop-cell *active-world* (clone =explosion=) r c])))
  [die self])

(define-method step mine (stepper)
  (declare (ignore stepper))
  [explode self])

(define-method damage mine (damage-points)
  (declare (ignore damage-points))
  [explode self])

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
  (firing-with :initform :left-hand)
  (max-weight :initform (make-stat :base 25))
  (max-items :initform (make-stat :base 5))
  (movement-cost :initform (make-stat :base 7))
  (equipment :initform nil)
  (dexterity :initform (make-stat :base 11 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (hit-points :initform (make-stat :base 50 :min 0 :max 100))
  (energy :initform (make-stat :base 1200 :min 0 :max 1800))
  (oxygen :initform (make-stat :base 1000 :min 0 :max 1200))
  (stepping :initform t))
	  
(define-method initialize player ()
  [make-inventory self]
  [make-equipment self])

(define-method fire player (direction)
  (let ((weapon [equipment-slot self <firing-with>]))
    (when weapon
      [expend-action-points self [stat-value weapon :attack-cost]]
      [fire weapon direction])))

;;; the game-specific controls

(define-prototype vm0-prompt (:parent rlx:=prompt=))

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
    ("E" nil "equip 0 .")))

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
    ("E" nil "equip 0 .")))

(define-method install-keybindings vm0-prompt ()
  (let ((keys (ecase rlx:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k))))

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
  (dotimes (i 65)
    [drop-cell self (clone =purple-perceptor=) (random 100) (random 100) :loadout])
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

(define-prototype tech-world (:parent rlx:=world=)
  (ambient-light :initform :total))

(define-method generate tech-world ()
  ;; create world
  (dotimes (i 49)
    (dotimes (j 49)
      [drop-cell self (clone (if (> (random 50) 10)
				  =tech-stone= =tech-stone-moss=))
		 i j 
		 :loadout]))
  (dotimes (n 10)
    [drop-cell self (clone =tech-brick-yellow=) (random 50) (random 50) :loadout])
  (dotimes (n 10)
    [drop-cell self (clone =tech-brick-green=) (random 50) (random 50) :loadout])
  (dotimes (i 20)
    [drop-cell self (clone =purple-perceptor=) (random 50) (random 50) :loadout])
  (dotimes (i 12)
    [drop-cell self (clone =red-perceptor=) (random 50) (random 50) :loadout])
  (dotimes (i 14)
    [drop-cell self (clone =mine=) (1+ (random 44)) (1+ (random 44))  :loadout])
  (dotimes (i 12)
    [drop-cell self (clone =electron=) (random 50) (random 50) :loadout])
  (dotimes (i 8)
    [drop-cell self (clone =shock-probe=) (random 50) (random 50) :loadout]) 
  (dotimes (i 12)
    [drop-cell self (clone =rusty-wrench=) (random 50) (random 50) :loadout])
  (dotimes (i 12)
    [drop-cell self (clone =muon-pistol=) (random 50) (random 50)])
 (dotimes (i 22)
    [drop-cell self (clone =rook=) (random 50) (random 50) :loadout])

  (dotimes (i 10)
    (labels ((drop-brick (x y)
	       [drop-cell self (clone =tech-brick-yellow=) y x])
	     (drop-crystal (x y)
	       [drop-cell self (clone =tech-brick-green=) y x]))
      (trace-octagon #'drop-brick 
		     (+ 10 (random 30))
		     (+ 10 (random 30))
		     (+ 4 (random 4)))
      (trace-octagon #'drop-crystal 
		     (+ 10 (random 30))
		     (+ 10 (random 30))
		     (+ 2 (random 3)))))

		  
    
  ;; add player 

  [drop-cell self <player> 5 5])

;;; putting it all together

(defun vm0 ()
  (setf rlx:*screen-height* 600)
  (setf rlx:*screen-width* 800)
  (let* ((prompt (clone rlx:=prompt=))
	 (player-prompt (clone =vm0-prompt=))
	 (world (clone =tech-world= :height 50 :width 50))
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

    ;; foo

    (install-widgets (list prompt browser player-prompt viewport status narrator))
    ))
    
(vm0)

;;; vm0.lisp ends here
