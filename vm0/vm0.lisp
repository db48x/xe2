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

(define-prototype tech-box (:parent rlx:=cell=)
  (tile :initform "tech-box")
n  (categories :initform '(:obstacle :opaque :pushable :destructible))
  (hit-points :initform (make-stat :base 10 :min 0)))

(define-prototype tech-box-debris (:parent rlx:=cell=)
  (tile :initform "tech-box-debris"))

(define-method die tech-box ()
  [queue>>drop-cell *active-world* (clone =tech-box-debris=) <row> <column>]
  [parent>>die self])
  
(define-prototype tech-brick-yellow (:parent rlx:=cell=)
  (tile :initform "yellow-tech-brick")
  (categories :initform '(:obstacle :opaque)))

;;; The deadly Rook bot

(define-prototype rook (:parent rlx:=cell=)
;;  (categories :initform '(:actor :target :obstacle :opaque :enemy))
  (equipment-slots :initform '(:robotic-arm :shoulder-mount))
  (speed :initform (make-stat :base 12))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 4))
  (tile :initform "rook")
  (target :initform nil)
  (hit-points :initform (make-stat :base 10 :min 0 :max 10)))

;;; Nondescript tech wall

(define-prototype tech-wall (:parent rlx:=cell=)
  (tile :initform "gold-tech-wall")
  (categories :initform '(:opaque :obstacle)))

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
  (name :initform "Xiong Les Fleurs Muon(TM) energy pistol")
  (tile :initform "muon-pistol")
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:left-hand))
  (weight :initform 7000)
  (accuracy :initform (make-stat :base 72))

  (attack-power :initform (make-stat :base 6))
  (attack-cost :initform (make-stat :base 4))
  (energy-cost :initform (make-stat :base 10)))

(define-method fire muon-pistol (direction)
  (if [expend-energy <equipper> 10]
      (let ((muon (clone =muon-particle=)))
	[queue>>drop <equipper> muon]
	[queue>>impel muon direction])
      (message "Not enough energy to fire.")))

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
  (categories :initform '(:actor))
  (speed :initform (make-stat :base 20))
  (default-cost :initform (make-stat :base 2))
  (movement-cost :initform (make-stat :base 2))
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
	  [queue>>damage target 5]
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
  (attack-power :initform (make-stat :base 8))
  (attack-cost :initform (make-stat :base 10))
  (energy-cost :initform (make-stat :base 20)))

(define-method fire lepton-cannon (direction)
  (if [expend-energy <equipper> 10]
      (let ((lepton (clone =lepton-particle=)))
	[queue>>drop <equipper> lepton]
	[queue>>impel lepton direction])
      (message "Not enough energy to fire.")))

;;; the med hypo

(define-prototype med-hypo (:parent rlx:=cell=)
  (categories :initform '(:item))
  (tile :initform "med-hypo"))

(define-method step med-hypo (stepper)
  (when (has-field :hit-points stepper)
    [queue>>stat-effect stepper :hit-points 12]
    [queue>>die self]))

;;; Melee shock probe for the Perceptors

(define-prototype shock-probe (:parent rlx:=cell=)
  (name :initform "Shock probe")
  (categories :initform '(:item :weapon :equipment :builtin))
  (tile :initform "shock-probe")
  (attack-power :initform (make-stat :base 10))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (weight :initform 3000)
  (equip-for :initform '(:robotic-arm)))

;;; The Purple Perceptor

;; Move in a straight line until hitting an obstacle.
;; Then choose a random direction and try again

(define-prototype purple-perceptor (:parent rlx:=cell=)
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
  (equipment-slots :initform '(:robotic-arm))
  (speed :initform (make-stat :base 7 :min 7))
  (max-items :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 3))
  (tile :initform "purple-perceptor")
  (stepping :initform t)
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (direction :initform (rlx:random-direction))
  (strength :initform (make-stat :base 12 :min 0 :max 30))
  (dexterity :initform (make-stat :base 9 :min 0 :max 30))
  (intelligence :initform (make-stat :base 11 :min 0 :max 30))
  (hit-points :initform (make-stat :base 12 :min 0 :max 10)))

(define-method initialize purple-perceptor ()
  [make-inventory self]
  [make-equipment self])

(define-method run purple-perceptor ()
  (clon:with-field-values (row column) self
    (let ((world *active-world*))
      (if (< [distance-to-player world row column] 5)
	  (let ((player-dir [direction-to-player world row column]))
	    (if [adjacent-to-player world row column]
		[queue>>attack self player-dir]
		[queue>>move self player-dir]))
	  (progn (when [obstacle-in-direction-p world row column <direction>]
		   (setf <direction> (rlx:random-direction)))
		 [queue>>move self <direction>])))))

(define-method die purple-perceptor ()
  (when (> 6 (random 10))
    [drop self (clone =energy=)])
  [parent>>die self])

(define-method loadout purple-perceptor ()
  (let ((probe (clone =shock-probe=)))
    [equip self [add-item self probe]]))

;;; The ion shield

(define-prototype ion-shield-wall (:parent rlx:=cell=)
  (tile :initform "ion-shield-wall")
  (categories :initform '(:obstacle :actor :target))
  (hit-points :initform (make-stat :base 7 :min 0))
  (clock :initform (+ 4 (random 1))))

(define-method die ion-shield-wall ()
  [queue>>drop-cell *active-world* (clone =flash=) <row> <column>]
  [parent>>die self])

(define-method run ion-shield-wall ()
  (when (zerop <clock>)
    [die self])
  (decf <clock>))

(define-prototype ion-shield (:parent rlx:=cell=)
  (categories :initform '(:item :equipment))
  (name :initform "Xiong Les Fleur Ion (TM) shield belt")
  (tile :initform "ion-shield")
  (equip-for :initform '(:belt))
  (size :initform 5))

(define-method activate ion-shield ()
  (let* ((world *active-world*)
	 (row [player-row world])
	 (column [player-column world])
	 (size <size>))
    (when [expend-energy [get-player world] 200]
      (labels ((drop-ion (r c)
		 [drop-cell world (clone =ion-shield-wall=) r c]))
	(trace-rectangle #'drop-ion 
			 (- row (truncate (/ size 2)))
			 (- column (truncate (/ size 2)))
			 size size)))))

;;; Electron

(define-prototype electron (:parent rlx:=cell=)
  (tile :initform "electron")
  (categories :initform '(:item)))

;;; The mysterious crystal

(define-prototype crystal (:parent rlx:=cell=)
  (name :initform "Turquoise crystal")
  (categories :initform '(:item))
  (tile :initform "turquoise-crystal"))

;;; The Red Perceptor

;; Seek player, attack any obstacles in the way.

(define-prototype red-perceptor (:parent rlx:=cell=)
  (strength :initform (make-stat :base 16 :min 0 :max 30))
  (dexterity :initform (make-stat :base 11 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
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
  (clon:with-field-values (row column) self
    (let* ((world *active-world*)
	   (direction [direction-to-player *active-world* row column]))
      (if [adjacent-to-player world row column]
	  [queue>>attack self direction]
	  (if [obstacle-in-direction-p world row column direction]
	      (let ((target [target-in-direction-p world row column direction]))
		(if (and target (not [in-category target :enemy]))
		    [queue>>attack self direction]
		    (progn (setf <direction> (random-direction))
			   [queue>>move self direction])))
	      (progn (when (< 7 (random 10))
		       (setf <direction> (random-direction)))
		     [queue>>move self direction]))))))

(define-method die red-perceptor ()
  (when (> 4 (random 10))
    (if (> 5 (random 10))
	[drop self (clone =energy=)]
	[drop self (clone =med-hypo=)]))
  [parent>>die self])

;;; An explosion

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
    (0 [queue>>die self]))
  (decf <clock>))

;;; The exploding mine

(define-prototype mine (:parent rlx:=cell=)
  (name :initform "Vanguara XR-1 Contact mine")
  (categories :initform '(:item :target))
  (tile :initform "mine"))

(define-method run mine ()
  nil)

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

;;; The rusty wrench

(define-prototype rusty-wrench (:parent rlx:=cell=)
  (name :initform "Rusty wrench")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "rusty-wrench")
  (attack-power :initform (make-stat :base 4)) ;; points of raw damage
  (attack-cost :initform (make-stat :base 5))
  (accuracy :initform (make-stat :base 60)) ;; percent
  (weight :initform 10000) ;; grams
  (equip-for :initform '(:left-hand :right-hand)))

;;; The energy tank

(define-prototype energy (:parent rlx:=cell=)
  (tile :initform "energy")
  (name :initform "Energy Tank"))

(define-method step energy (stepper)
  (when (has-field :energy stepper)
    [queue>>stat-effect stepper :energy 100]
    [queue>>die self]))

;;; The oxygen tank

(define-prototype oxygen-tank (:parent rlx:=cell=)
  (tile :initform "oxygen-tank")
  (name :initform "Oxygen Tank"))

(define-method step oxygen-tank (stepper)
  (when (has-field :oxygen stepper)
    [queue>>stat-effect stepper :oxygen 200]
    [queue>>die self]))

;;; The player and his remains

(define-prototype skull (:parent rlx:=cell=)
  (tile :initform "skull"))

(define-method forward skull (&rest args)
  (declare (ignore args))
  [queue>>narrateln :narrator "You are dead. You can't do anything!"])
  
(define-method move skull (&rest args)
  (declare (ignore args))
  [queue>>narrateln :narrator "You are dead. You can't do anything!"])

(define-prototype player (:parent rlx:=cell=)
  (tile :initform "player")
  (categories :initform '(:actor :target :container :player :obstacle))
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
  (energy :initform (make-stat :base 800 :min 0 :max 1000))
  (oxygen :initform (make-stat :base 1000 :min 0 :max 1200))
  (stepping :initform t)
  (menu :initform '(("equip 0 ." :name "Equip 0" :key "0" :description "Equip the item in inventory slot 0.")
		    ("equip 1 ."  :name "Equip 1" :key "1" :description "Equip the item in inventory slot 1.")
		    ("activate-equipment :belt ." :name "Activate Belt" :key "0" :description "Activate belt.")
		    ("take ." :name "Take item" :key "T" :description "Take item from ground.")
		    ("move " :name "Move..." :description "Move in a chosen direction." :sub-menu *choose-direction-menu*))))
	  
(define-method initialize player ()
  [make-inventory self]
  [make-equipment self])

(define-method loadout player ()
  (let ((gun (clone =muon-pistol=))
	(belt (clone =ion-shield=)))
    [equip self [add-item self gun]]
    [equip self [add-item self belt]]))
    
(define-method die player ()
  (let ((skull (clone =skull=)))
    [drop-cell *active-world* skull <row> <column>]
    [queue>>narrateln :narrator "You die."]
    [set-player *active-world* skull]
    [parent>>die self]))
  
(define-method activate-equipment player (slot)
  [activate [equipment-slot self slot]])

;;; The storage container you break into

(define-prototype storage-world (:parent rlx:=world=)
  (ambient-light :initform :total)
  (width :initform 60)
  (height :initform 60)
  (pallet-size :initform 9))

(define-method generate storage-world (&optional parameters)
  (declare (ignore parameters))
  (clon:with-field-values (height width pallet-size) self
    ;; create world
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone (if (> (random 50) 10)
				   =tech-stone= =tech-stone-moss=))
		   i j 
		   :loadout]))
    ;; (dotimes (n 10)
    ;;   [drop-cell self (clone =tech-brick-yellow=) (random height) (random height) :loadout])
    (dotimes (n 2)
      [drop-cell self (clone =med-hypo=) (random height) (random height)])
    (dotimes (i 12)
      [drop-cell self (clone =red-perceptor=) (random height) (random width) :loadout])
    (dotimes (i 20)
      [drop-cell self (clone =purple-perceptor=) (random height) (random width) :loadout])
    (dotimes (i 14)
      [drop-cell self (clone =mine=) (random 50) (random 50)  :loadout])
    ;; (dotimes (i 12)
    ;;   [drop-cell self (clone =energy=) (random height) (random height) :loadout])
    (dotimes (i 8)
      [drop-cell self (clone =ion-shield=) (random height) (random width) :loadout]) 
    (dotimes (i 12)
      [drop-cell self (clone =rusty-wrench=) (random height) (random width) :loadout])
    (dotimes (i 12)
      [drop-cell self (clone =muon-pistol=) (random height) (random width)])
    ;; (dotimes (i 22)
    ;;    [drop-cell self (clone =rook=) (random height) (random height) :loadout])
    (let ((imax (1- (truncate (/ width pallet-size))))
	  (jmax (1- (truncate (/ height pallet-size)))))
      (dotimes (i imax)
	(dotimes (j jmax)
	  (labels ((drop-brick (x y)
		     [drop-cell self (clone =tech-brick-yellow=) y x])
		   (drop-box (x y)
		     [drop-cell self (clone =tech-box=) y x])
		   (drop-wall (x y)
		     [drop-cell self (clone =tech-wall=) y x]))
	    (when (not (= 0 i j))
	      (trace-rectangle #'drop-wall
			       0 0 height width)
	      (trace-rectangle #'drop-box
			       (+ (random 3)
				  (* pallet-size i))
			       (+ (random 4) 
				  (* pallet-size j))
			       (random pallet-size)
			       (random pallet-size)
			       :fill))))))))

;;; The exterior of the space station

(define-prototype void (:parent rlx:=cell=)
  (categories :initform '(:obstacle))
  (tile :initform "void"))

(define-prototype space (:parent rlx:=cell=)
  (tile :initform "starfield"))

(define-method step space (stepper)
  (when (has-field :oxygen stepper)
    [queue>>stat-effect stepper :oxygen -1]))

(define-prototype star (:parent rlx:=cell=)
  (tile :initform "star"))

(define-prototype small-star (:parent rlx:=cell=)
  (tile :initform "small-star"))

(define-prototype station-arm-horz (:parent rlx:=cell=)
  (tile :initform "station-arm-horz")
  (categories :initform '(:obstacle :opaque :destructible))
  (hit-points :initform (make-stat :base 10 :min 0)))

(define-prototype station-arm-vert (:parent rlx:=cell=)
  (tile :initform "station-arm-vert")
  (categories :initform '(:obstacle :opaque :destructible))
  (hit-points :initform (make-stat :base 10 :min 0)))

(define-prototype station-base (:parent rlx:=cell=)
  (tile :initform "station-base")
  (categories :initform '(:obstacle :actor :equipper :opaque))
  (speed :initform (make-stat :base 10))
  (hit-points :initform (make-stat :base 40 :min 0))
  (equipment-slots :initform '(:robotic-arm))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (attacking-with :initform :robotic-arm)
  (energy :initform (make-stat :base 800 :min 0 :max 1000))
  (firing-with :initform :robotic-arm)
  (strength :initform (make-stat :base 13))
  (dexterity :initform (make-stat :base 9)))

(define-method loadout station-base ()
  (let ((cannon (clone =lepton-cannon=)))
    [equip self [add-item self cannon]]))

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

(define-prototype station-world (:parent rlx:=world=)
  (ambient-light :initform :total)
  (width :initform 22)
  (height :initform 130))

(define-method paint-station-piece station-world (row column maxsize)
  (labels ((drop-horz (r c)
	     [drop-cell self (clone =station-arm-horz=) r c])
	   (drop-vert (r c)
	     [drop-cell self (clone =station-arm-vert=) r c])
	   (drop-base (r c)
	     [drop-cell self (clone =station-base=) r c :loadout]))
    (trace-row #'drop-horz row column (max 0 (- column (random maxsize))))
    (trace-row #'drop-horz row column (+ column (random maxsize)))
    (trace-column #'drop-vert column row (max 0 (- row (random maxsize))))
    (trace-column #'drop-vert column row (+ row (random maxsize)))
    (drop-base row column)))

(define-method generate station-world (&optional parameters)
  (declare (ignore parameters))
  (clon:with-field-values (height width) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =space=) i j]))
    (dotimes (i 400)
      [drop-cell self (clone =small-star=) (random height) (random width)])
    (dotimes (i 140)
      [drop-cell self (clone =star=) (random height) (random width)])
    (trace-rectangle #'(lambda (r c)
			 [drop-cell self (clone =void=) r c])
		     0 0 height width)
    ;; paint station pieces
    (dotimes (i 25) 
      [paint-station-piece self (random height) (random width) (+ 3 (random 5))])
    ;;
    ;; paint other stuff
    (dotimes (n 4)
      [drop-cell self (clone =med-hypo=) (random height) (random height)])
    (dotimes (i 18)
      [drop-cell self (clone =red-perceptor=) (random height) (random width) :loadout])
    (dotimes (i 20)
      [drop-cell self (clone =purple-perceptor=) (random height) (random width) :loadout])
    (dotimes (i 100)
      [drop-cell self (clone =mine=) (random height) (random width) :loadout])
    (dotimes (i 12)
      [drop-cell self (clone =oxygen-tank=) (random height) (random width) :loadout])
    ;;
    ;; place portals TODO and player??
    ))

;;; The game-specific controls

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

(define-method install-keybindings vm0-prompt ()
  (let ((keys (ecase rlx:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k))))

;;; A character status widget

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

;;; putting it all together

(defun vm0 ()
  (setf rlx:*screen-height* 600)
  (setf rlx:*screen-width* 800)
  (let* ((prompt (clone rlx:=prompt=))
	 (player-prompt (clone =vm0-prompt=))
	 (world (clone =station-world=))
	 (player (clone =player=))
	 (status (clone =status=))
	 (narrator (clone rlx:=narrator=))
	 (browser (clone rlx:=browser=))
	 (menu (clone rlx:=browser=))
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
    ;; MENU
    [resize menu :height 500 :width 220]
    [move menu :x 100 :y 60]
    [set-collection-from-menu-spec menu (field-value :menu player)]
    [set-prompt menu player-prompt]
    ;; system prompt
    [install-keybindings prompt]
    [resize prompt :height 30 :width 400]
    [move prompt :x 0 :y 570]
    [set-mode prompt :forward]
    [set-receiver prompt world]
    ;; vm0 player command prompt
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
    [create-default-grid world]
    [generate world]
    [set-player world player]
    [set-browser world browser]
    [start world]
    (setf viewport (clone =viewport=))
    [set-world viewport world]
    [resize viewport :height 320 :width 400]
    [move viewport :x 0 :y 0]
    [drop-cell world player 125 10 :loadout]
    [set-origin viewport :x 0 :y 110 :height 20 :width 25]
    [adjust viewport]
    ;; foo

    (install-widgets (list narrator prompt browser player-prompt viewport status))
    ))
    
(vm0)

;;; vm0.lisp ends here
