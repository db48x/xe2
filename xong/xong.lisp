;;; xong.lisp --- hockey paintball snake pong

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


;;; Packaging

(defpackage :xong
  (:documentation "XONG is a variation on Pong.")
  (:use :rlx :common-lisp)
  (:export xong))

(in-package :xong)

;;; Counting enemies

(defvar *enemies* 0)

;;; Colors

(defparameter *colors* '(:purple :red :blue :orange :green :yellow :white))

;;; Gates 

(defparameter *gate-timeout* 30)

(defcell gate
  (categories :initform '(:actor :obstacle :gate))
  (speed :initform (make-stat :base 10))
  (tile :initform "gate-closed")
  (clock :initform 0)
  (description :initform "Opens for a brief time when hit with the puck."))

(define-method open gate ()
  [delete-category self :obstacle]
  (setf <tile> "gate-open")
  (setf <clock> *gate-timeout*))

(define-method close gate ()
  [add-category self :obstacle]
  [play-sample self "gate-closing-sound"]
  (setf <tile> "gate-closed"))

(define-method run gate ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (zerop <clock>)
    [close self]))

;;; The player's tail

(defcell tail 
  (categories :initform '(:actor))
  (clock :initform 4))
  
(define-method initialize tail (&key direction clock)
  (setf <clock> clock)
  (setf <tile> (ecase direction
		 (:north "tail-north")
		 (:south "tail-south")
		 (:east "tail-east")
		 (:west "tail-west"))))

(define-method run tail ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (< <clock> 0) (setf <clock> 0))
  (when (zerop <clock>)
    [die self]))

;; (define-method step tail (stepper)
;;   (when [in-category stepper :puck]

;;; A tail extender powerup

(defcell extender 
  (tile :initform "plus")
  (description :initform 
"This powerup extends your trail."))	      

(define-method step extender (stepper)
  (when [in-category stepper :tailed]
    [play-sample self "worp"]
    [stat-effect stepper :tail-length 7]
    [stat-effect stepper :score 2000]
    [die self]))

;;; Chevrons change the direction of the puck

(defcell chevron
  (tile :initform "chevron-east")
  (categories :initform '(:chevron))
  (description :initform 
"Chevrons change the direction of the puck and certain enemies."))

(defvar *chevron-tiles* '(:north "chevron-north"
			  :south "chevron-south"
			  :east "chevron-east"
			  :west "chevron-west"))

(define-method orient chevron (direction)
  (assert (member direction '(:north :south :east :west)))
  (setf <tile> (getf *chevron-tiles* direction))
  (setf <direction> direction))

(define-method step chevron (stepper)
  (when [in-category stepper :puck]
    [play-sample self "chevron"]
    [kick stepper <direction>]))

;;; Diamond pickup replenishes chevrons

(defcell diamond 
  (tile :initform "chevron-pickup")
  (description :initform "Adds five chevrons to your inventory."))

(define-method step diamond (stepper)
  (when [in-category stepper :pointer]
    [stat-effect stepper :chevrons 5]
    [play-sample self "worp"]
    [die self]))

;;; Tracers lay down deadly red wires

(defcell wire 
  (categories :initform '(:actor :damaging))
  (stepping :initform t)
  (speed :initform (make-stat :base 1))
  (clock :initform 20)
  (description :initform "Deadly wires are an instant kill for player and puck."))
  
(define-method initialize wire (&key direction clock)
  (setf <clock> clock)
  (setf <tile> (ecase direction
		 (:north "wire-north")
		 (:south "wire-south")
		 (:east "wire-east")
		 (:west "wire-west"))))

(define-method run wire ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (< <clock> 0) (setf <clock> 0))
  (when (zerop <clock>)
    [die self]))

(define-method step wire (stepper)
  (if [is-player stepper]
      [damage stepper 1]
      (when [in-category stepper :puck]
	(unless [in-category stepper :enemy]
	  [die stepper]))))

(defvar *tracer-tiles* '(:north "tracer-north"
			  :south "tracer-south"
			  :east "tracer-east"
			  :west "tracer-west"))

(defcell tracer 
  (tile :initform "tracer-north")
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper :puck :tailed :tracer))
  (dead :initform nil)
  (speed :initform (make-stat :base 7 :min 5))
  (max-items :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 1))
  (stepping :initform t)
  (tail-length :initform (make-stat :base 20))
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (direction :initform (car (one-of '(:north :south :east :west))))
  (strength :initform (make-stat :base 4 :min 0 :max 30))
  (dexterity :initform (make-stat :base 5 :min 0 :max 30))
  (intelligence :initform (make-stat :base 11 :min 0 :max 30))
  (hit-points :initform (make-stat :base 10 :min 0 :max 10))
  (description :initform 
"The Tracer drags a live wire behind it. Don't touch! 
Use chevrons to direct tracers into Black Holes."))

(define-method update-tile tracer ()
  (setf <tile> (getf *tracer-tiles* <direction>)))

(define-method kick tracer (direction)
  (setf <direction> direction))
    
(define-method run tracer ()
  (clon:with-field-values (row column) self
    (let ((world *active-world*))
      (when [obstacle-in-direction-p world row column <direction>]
	(setf <direction> (car (one-of '(:north :south :east :west)))))
      [expend-action-points self 25]
      [move self <direction>])))

(define-method drop-wire tracer ()
  [drop-cell *active-world* (clone =wire= :direction <direction> :clock 5)
	     <row> <column>])

(define-method move tracer (direction)
  [drop-wire self]
  [update-tile self]
  [parent>>move self direction])

(define-method loadout tracer ()
  (incf *enemies*))

(define-method cancel tracer ()
  (decf *enemies*))

(define-method die tracer ()
  (unless <dead>
    (setf <dead> t)
    (decf *enemies*)
    [delete-from-world self]))

;;; The deadly security monitor has a deadly ring field attack

(defcell monitor
  (tile :initform "monitor")
  (name :initform "Monitor")
  (categories :initform '(:obstacle :actor :equipper :opaque 
			  :exclusive :enemy :target :puck :monitor))
  (direction :initform nil)
  (speed :initform (make-stat :base 2))
  (movement-cost :initform (make-stat :base 6))
  (hit-points :initform (make-stat :base 20 :min 0))
  (equipment-slots :initform '(:robotic-arm))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (dead :initform nil)
  (attacking-with :initform :robotic-arm)
  (energy :initform (make-stat :base 800 :min 0 :max 1000))
  (firing-with :initform :robotic-arm)
  (strength :initform (make-stat :base 24))
  (dexterity :initform (make-stat :base 12))
  (description :initform 
"These security drones scan areas methodically until an intruder is
detected; if you get close enough, an alarm sounds, and an electric
shock field is projected in pulses. If you become trapped, try
squeezing by in between pulses!"))
    
(define-method choose-new-direction monitor ()
  [expend-action-points self 2]
  (setf <direction>
	(if (= 0 (random 20))
	    ;; occasionally choose a random dir
	    (nth (random 3)
		 '(:north :south :east :west))
	    ;; otherwise turn left
	    (getf '(:north :west :west :south :south :east :east :north)
		  (or <direction> :north)))))
  
(define-method loadout monitor ()
  (incf *enemies*)
  [choose-new-direction self])
  
(define-method cancel monitor ()
  (decf *enemies*))

;; (define-method initialize monitor ()
;;   [make-inventory self]
;;   [make-equipment self])

(define-method kick monitor (direction)
  (setf <direction> direction))

(define-method alarm monitor ()
  [play-sample self "activate"]
  [expend-action-points self 10]
  (labels ((do-circle (image)
	     (multiple-value-bind (x y) 
		 [screen-coordinates self]
	       (draw-circle x y 40 :destination image)
	       (draw-circle x y 35 :destination image))))
    [>>add-overlay :viewport #'do-circle])
  (when (< [distance-to-player self] 3.5)
    [damage [get-player *active-world*] 1]))

(define-method run monitor ()
  (clon:with-field-values (row column) self
    (let ((world *active-world*))
      (if (< [distance-to-player world row column] 6)
	  (let ((player-dir [direction-to-player world row column]))
	    [alarm self]
	    [move self player-dir]
	    [expend-action-points self 10])
	  (multiple-value-bind (r c)
	      (step-in-direction <row> <column> <direction>)
	    (when [obstacle-at-p world r c]
	      [choose-new-direction self])
	    [move self <direction>])))))
  
(define-method die monitor ()
  (unless <dead>
    (setf <dead> t)
    (decf *enemies*)
    [play-sample self "death-alien"]
    [delete-from-world self]))

;;; Replacement puck

(defcell puckup 
  (tile :initform "puckup")
  (name :initform "Replacement puck")
  (description :initform "A new puck, in case you lose the one you have."))

(define-method step puckup (stepper)
  (when [is-player stepper]
    (let ((puck (clone =puck=)))
      [drop self puck]
      [grab stepper puck]
      [die self])))

;;; Black hole eats anything in category :puck (and the player)

(defcell hole 
  (tile :initform "hole")
  (categories :initform '(:exclusive :hole))
  (name :initform "Black hole")
  (description :initform 
"These holes eat the puck and enemies. The object of the game is to
defeat enemies by guiding them into the black holes."))

(define-method step hole (stepper)
  [play-sample self "hole-suck"]
  (if [in-category stepper :puck]
      [die stepper]
      (when [is-player stepper]
	[damage stepper 1])))

;;; Bricks

(defvar *lock-tiles* '(:purple "brick-purple"
			:black "brick-black"
			:red "brick-red"
			:blue "brick-blue"
			:orange "brick-orange"
			:green "brick-green"
			:white "brick-white"
			:yellow "brick-yellow"))

(defcell lock 
  (tile :initform "lock-white")
  (color :initform :white)
  (categories :initform '(:obstacle :exclusive :paintable))
  (description :initform "These open permanently when hit with the same color paint."))

(define-method set-color lock (c)
  (setf <color> c)
  (let ((res (getf *lock-tiles* c)))
    (assert (stringp res))
    (setf <tile> res)))

(define-method paint lock (color)
  (if (eq <color> color)
      (progn [play-sample self "lock-opening-sound"]
	     [die self])
      [play-sample self "error"]))

;; (define-method spawn lock ()
;;   (let ((dir (car (one-of '(:north :south :east :west))))
;; 	(lock (clone =lock=)))
;;     (multiple-value-bind (r c)
;; 	(step-in-direction <row> <column> dir)
;;       [drop-cell *active-world* lock r c :probe t :exclusive t])))

;;; Door to next level

(define-prototype door (:parent rlx:=gateway=)
  (tile :initform "door")
  (name :initform "Level exit")
  (description :initform "Door to the next level of Xong.")
  (categories :initform '(:gateway :actor))
  (address :initform nil))
  
(define-method level door (lev)
  (setf <address> (generate-level-address lev)))

(define-method step door (stepper)
  (when [is-player stepper]
    (if (zerop *enemies*)
	[activate self]
	[play-sample self "error"])))

(define-method run door ()
  (when (zerop *enemies*)
    (setf <tile> "door-open")))
	
;;; Breakable paint walls re-color the ball

(defvar *wall-tiles* '(:purple "wall-purple"
			:black "wall-black"
			:red "wall-red"
			:blue "wall-blue"
			:orange "wall-orange"
			:green "wall-green"
			:white "wall-white"
			:yellow "wall-yellow"))

(defcell wall 
  (name :initform "Paint block")
  (tile :initform "wall-purple")
  (description :initform
"These blocks of paint can be broken using the puck to 
reach new areas and items. The puck also picks up the color.")
  (categories :initform '(:exclusive :obstacle :wall))
  (color :initform :purple))

(define-method paint wall (c)
  (setf <color> c)
  (let ((res (getf *wall-tiles* c)))
    (assert (stringp res))
    (setf <tile> res)))

;; (define-method step wall (puck)
;;   (when [in-category puck :puck]
;;     [paint puck <color>]
;;     [bounce puck]
;;     [die self]))
  
;;; Our hero, the player

(defvar *player-tiles* '(:purple "player-purple"
			:black "player-black"
			:red "player-red"
			:blue "player-blue"
			:orange "player-orange"
			:green "player-green"
			:white "player-white"
			:yellow "player-yellow"
			 :other "player-other"))

(defcell player 
  (tile :initform "player")
  (name :initform "Player")
  (score :initform (make-stat :base 0))
  (last-direction :initform :north)
  (dead :initform nil)
  (puck :initform nil)
  (chevrons :initform (make-stat :base 0 :min 0 :max 10))
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (strength :initform (make-stat :base 13))
  (tail-length :initform (make-stat :base 20 :min 0))
  (dexterity :initform (make-stat :base 13))
  (defense :initform (make-stat :base 15))
  (equipment-slots :initform '(:left-hand :right-hand))
  (hearing-range :initform 1000)
  (hit-points :initform (make-stat :base 1 :min 0 :max 30))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (stepping :initform t)
  (attacking-with :initform :right-hand)
  (light-radius :initform 3)
  (categories :initform '(:actor :tailed :player :target 
			  :container :light-source :pointer))
  (description :initform "This is you! Move with the arrow keys or numeric keypad."))

(define-method run player ()
  (unless <dead>
    (setf <tile> (if (null <puck>)
		     "player-empty"
		     (getf *player-tiles* (if (has-field :color <puck>)
					      (field-value :color <puck>)
					      :other))))
    [step-on-current-square self]))

(define-method quit player ()
  (rlx:quit :shutdown))

(define-method step player (stepper)
  (when [in-category stepper :item]
    [grab self stepper]))

(define-method drop-tail player ()
  [drop self (clone =tail= 
		    :direction <last-direction> 
		    :clock [stat-value self :tail-length])])

(define-method restart player ()
  (let ((player (clone =player=)))
    [destroy *active-universe*]
    [set-player *active-universe* player]
    [set-character *status* player]
    [play *active-universe*
	  :address '(=xong= :level 1)]
    [loadout player]
    [play-sample self "go"]))

(define-method drop-chevron player (direction)
  (unless <dead>
    (if (or (zerop [stat-value self :chevrons])
	    [category-at-p *active-world* <row> <column> :chevron])
	[play-sample self "error"]
	(let ((chevron (clone =chevron=)))
	  [drop self chevron]
	  [play-sample self "powerup"]
	  [stat-effect self :chevrons -1]
	  [orient chevron direction]))))

(define-method move player (direction)
  (unless <dead>
    (setf <last-direction> direction)
    [drop-tail self]
    [parent>>move self direction]))
  
(define-method loadout player ()
  (setf <puck> (clone =puck=)))

(define-method throw player (direction)
  (assert (member direction '(:north :south :east :west)))
  (unless <dead>
    (clon:with-fields (puck) self
      (when puck
	[drop-cell *active-world* puck <row> <column> :no-stepping t]
	[kick puck direction]
	(setf puck nil)
	[play-sample self "serve"]))))

(define-method grab player (puck)
  (assert [in-category puck :puck])
  (when (null <puck>)
    (progn (setf <puck> puck)
	   [delete-from-world puck]
	   [play-sample self "grab"])))

(define-method die player ()
  (unless <dead>
    (setf <tile> "skull")
    [play-sample self "death"]
    [say self "You died. Press ESCAPE to try again."]
    (setf <dead> t)))

;;; Controlling the game

(define-prototype xong-prompt (:parent rlx:=prompt=))

(defparameter *numpad-keybindings* 
  '(("KP8" nil "move :north .")
    ("KP4" nil "move :west .")
    ("KP6" nil "move :east .")
    ("KP2" nil "move :south .")
    ;;
    ("KP8" (:control) "throw :north .")
    ("KP4" (:control) "throw :west .")
    ("KP6" (:control) "throw :east .")
    ("KP2" (:control) "throw :south .")
    ;;
    ("KP8" (:alt) "drop-chevron :north .")
    ("KP4" (:alt) "drop-chevron :west .")
    ("KP6" (:alt) "drop-chevron :east .")
    ("KP2" (:alt) "drop-chevron :south .")
    ;; arrows
    ("UP" nil "move :north .")
    ("LEFT" nil "move :west .")
    ("RIGHT" nil "move :east .")
    ("DOWN" nil "move :south .")
    ;;
    ("UP" (:control) "throw :north .")
    ("LEFT" (:control) "throw :west .")
    ("RIGHT" (:control) "throw :east .")
    ("DOWN" (:control) "throw :south .")
    ;;
    ("UP" (:alt) "drop-chevron :north .")
    ("LEFT" (:alt) "drop-chevron :west .")
    ("RIGHT" (:alt) "drop-chevron :east .")
    ("DOWN" (:alt) "drop-chevron :south .")))


(defparameter *qwerty-keybindings*
  (append *numpad-keybindings*
	  '(("K" nil "move :north .")
	    ("H" nil "move :west .")
	    ("L" nil "move :east .")
	    ("J" nil "move :south .")
	    ;;
	    ("K" (:control) "throw :north .")
	    ("H" (:control) "throw :west .")
	    ("L" (:control) "throw :east .")
	    ("J" (:control) "throw :south .")
	    ;;
	    ("K" (:alt) "drop-chevron :north .")
	    ("H" (:alt) "drop-chevron :west .")
	    ("L" (:alt) "drop-chevron :east .")
	    ("J" (:alt) "drop-chevron :south .")
	    ;;
	    ("ESCAPE" nil "restart .")
	    ("Q" (:control) "quit ."))))
  
(define-method install-keybindings xong-prompt ()
  (dolist (k *qwerty-keybindings*)
      (apply #'bind-key-to-prompt-insertion self k))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *active-world* :timer])])

;;; The floor

(defcell floor
  (tile :initform "floor")
  (color :initform ".black"))

;;; The puck

(defvar *puck-tiles* '(:purple "puck-purple"
			:black "puck-black"
			:red "puck-red"
			:blue "puck-blue"
			:orange "puck-orange"
			:green "puck-green"
			:white "puck-white"
			:yellow "puck-yellow"))

(defcell puck
  (tile :initform "puck")
  (description :initform "A frictionless paint-absorbent hockey puck.")
  (categories :initform '(:puck :obstacle :target :actor :paintable :item))
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform :here)
  (stepping :initform t)
  (color :initform :white))

(define-method kick puck (direction)
  (setf <direction> direction))

(define-method bounce puck ()
  (setf <direction> (opposite-direction <direction>))
  [play-sample self "bounce"])
  ;; ;; check player collision; this happens when shooting an adjacent wall
  ;; (when [category-at-p *active-world* <row> <column> :player]
  ;;   [grab [get-player *active-world*] self]))

(define-method paint puck (color)
  (setf <color> color)
  (setf <tile> (getf *puck-tiles* color)))

(define-method move puck (direction)
  (multiple-value-bind (r c) 
      (step-in-direction <row> <column> direction)
    (let ((obstacle [obstacle-at-p *active-world* r c]))
      (when obstacle
	[bounce self]
	(when (clon:object-p obstacle)
	  (if [is-player obstacle]
	      [grab obstacle self]
	      ;; it's not the player. see if we can color, or get paint
	      (progn 
		(when [in-category obstacle :paintable]
		  [paint obstacle <color>])
		(when [in-category obstacle :wall]
		  [paint self (field-value :color obstacle)]
		  [die obstacle]
		  [parent>>move self direction])
		(when [in-category obstacle :gate]
		  [open obstacle]
		  [parent>>move self direction]))))))
    (when [is-located self]
      [parent>>move self <direction>])))

(define-method run puck ()
  ;; pucks don't stop moving.
  (if (eq :here <direction>)
      [die self]
      [move self <direction>]))

(define-method die puck ()
  [play-sample self "buzz"]
  [parent>>die self])

;;; Bulkheads are indestructible walls

(defcell bulkhead
  (name :initform "Bulkhead")
  (tile :initform "bulkhead")
  (categories :initform '(:obstacle))
  (description :initform "It's an indestructible wall."))

;;; Xong game board

(defun generate-level-address (n)
  (assert (and (integerp n) (plusp n)))
  (list '=xong= 
	:level n
	:extenders (truncate (/ (* 3 (1- n)) 2))
	:tracers (+ 4 (* (1- n) 3))
	:monitors (+ 2 (* (1- n) 2))
	:rooms 1
	:puzzle-length (+ 4 (truncate (/ n 3)))
	:puckups (+ 4 (truncate (* (1- n) 2.5)))
	:diamonds (+ 6 (* (1- n) 2))
	:swatches (+ 10 (truncate (* 1.6 n)))))

(define-prototype xong (:parent rlx:=world=)
  (name :initform "Xong board")
  (description :initform "Welcome to Xong. Press F1 for general help, or click any object.")
  (edge-condition :initform :block)
  (level :initform 1)
  (width :initform 50)
  (height :initform 29)
  (scale :initform '(1 nm))
  (ambient-light :initform :total))

(define-method drop-room xong (row column height width next-level puzzle-length &optional (material =bulkhead=))
  (let (rectangle openings)
    (labels ((collect-point (&rest args)
	       (prog1 nil (push args rectangle)))
	     (drop-wall (r c)
		 [replace-cells-at self r c (clone material)]))
      (trace-rectangle #'collect-point row column height width)
      ;; make sure there are openings
      (dotimes (i 6)
	(let ((n (random (length rectangle))))
	  (push (nth n rectangle) openings)
	  (setf rectangle (delete (nth n rectangle) rectangle))))
      ;; draw walls
      (dolist (point rectangle)
	(destructuring-bind (r c) point
	  (drop-wall r c)))
      ;; draw gates
      (dolist (point openings)
	(destructuring-bind (r c) point
	  [drop-cell self (clone =gate=) r c]))
      ;; drop floor, obliterating what's below
      (labels ((drop-floor (r c)
		 (prog1 nil
		   [replace-cells-at self r c (clone =floor=)]))
	       (drop-lock (r c)
		 (prog1 nil 
		   (let ((lock (clone =lock=)))
		     [set-color lock (car (one-of *colors*))]
		     [drop-cell self lock r c]))))
	(trace-rectangle #'drop-floor (1+ row) (1+ column) (- height 2) (- width 2) :fill)
	;; drop lock puzzle
	(let ((col (+ column (truncate (/ width 2)))))
	  (trace-column #'drop-wall (- col 1) row (+ 1 row puzzle-length))
	  (trace-column #'drop-lock col (+ row 2) (+ 1 row puzzle-length))
	  (trace-column #'drop-wall (+ col 1) row (+ 1 row puzzle-length))
	  ;; drop door
	  (let ((door (clone =door=)))
	    [level door next-level]
	    [drop-cell self (clone material) (+ 1 row) col]
	    [drop-cell self door (+ 2 row) col]))))))

(define-method generate xong (&key (level 1)
				   (extenders 0)
				   (tracers 4)
				   (rooms 1)
				   (puzzle-length 4)
				   (puckups 4)
				   (monitors 3)
				   (diamonds 6)
				   (swatches 10))
  [create-default-grid self]
  (setf <level> level)
  (setf *enemies* 0)
  (clon:with-fields (height width grid) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))
    (dotimes (n swatches)
      ;; ensure all colors are present,
      ;; after that make it random
      (let ((color (if (< n (length *colors*))
		       (nth n *colors*)
		       (car (one-of *colors*)))))
	(labels ((drop-wall (r c)
		   (prog1 nil
		     (let ((wall (clone =wall=)))
		       [drop-cell self wall r c :exclusive t]
		       [paint wall color]))))
	  (multiple-value-bind (r c) [random-place self]
	    [drop-cell self (clone =hole=) 
		       (+ r 2 (random 3))
		       (+ c 2 (random 3))
		       :exclusive t]
	    (trace-rectangle #'drop-wall r c
			     (+ 4 (random 8)) (+ 4 (random 8)) :fill)))))
    (dotimes (n rooms)
      [drop-room self 
		 (+ 5 (random (- height 15)))
		 (+ 5 (random (- width 15)))
		 (+ 9 (random 6)) (+ 10 (random 4)) (+ level 1) puzzle-length])
    (dotimes (n monitors)
      [drop-cell self (clone =monitor=) (+ 10 (random height)) (+ 10 (random width))
		 :loadout t :exclusive t])
    (dotimes (n tracers)
      [drop-cell self (clone =tracer=) (random height) (random width) :loadout t])
    (dotimes (n extenders)
      [drop-cell self (clone =extender=) (random height) (random width)])
    (dotimes (n diamonds)
      [drop-cell self (clone =diamond=) (random height) (random width)])
    (dotimes (n puckups)
      (multiple-value-bind (r c) [random-place self]
	[drop-cell self (clone =puckup=) r c]))))

(define-method begin-ambient-loop xong ()  
  (play-music (car (one-of '("flyby" "pensive" "toybox"))) :loop t))
      
;;; Splash screen
  
(defvar *pager* nil)

(define-prototype splash (:parent =widget=))

(define-method render splash ()
  (rlx:draw-resource-image "splash" 0 0 
			   :destination <image>))

(defvar *space-bar-function*)

(define-method dismiss splash ()
  [select *pager* :play]
  (when (functionp *space-bar-function*)
    (funcall *space-bar-function*))
  ;; TODO ugh this is a hack!
  (rlx:show-widgets))

(define-prototype splash-prompt (:parent =prompt=)
  (default-keybindings :initform '(("SPACE" nil "dismiss ."))))

;;; Player status

(defvar *status* nil)

(define-prototype status (:parent rlx:=formatter=)
  (character :documentation "The character cell."))

(define-method set-character status (character)
  (setf <character> character))

(define-method print-stat status (stat-name &key warn-below show-max)
  (let* ((stat (field-value stat-name <character>))
	 (value [stat-value <character> stat-name]))
    (destructuring-bind (&key min max base delta unit) stat
      (let ((color (if (and (numberp warn-below)
			    (< value warn-below))
		       ".red"
		       ".gray40")))
	[print self (symbol-name stat-name)
	       :foreground ".white"]
	[print self ":["]
	[print self (format nil "~S" value) 
	       :foreground ".yellow"
	       :background color]
	(when show-max
	  [print self (format nil "/~S" max)
		 :foreground ".yellow"
		 :background color])
	(when unit 
	  [print self " "]
	  [print self (symbol-name unit)])
	[print self "]"]
	))))

(defparameter *status-bar-character* " ")

(define-method print-stat-bar status (stat &key 
					   (color ".yellow")
					   (background-color ".gray40"))
  (let ((value (truncate [stat-value <character> stat]))
	(max (truncate [stat-value <character> stat :max])))
    (dotimes (i max)
      [print self *status-bar-character*
	     :foreground ".yellow"
	     :background (if (< i value)
			     color
			   background-color)])))

(define-method update status ()
  [delete-all-lines self]
  (let* ((char <character>))
    (when char
	[print-stat self :chevrons :warn-below 1 :show-max t]
	[print-stat-bar self :chevrons :color ".yellow"]
	[space self]
	[print self (format nil "   LEVEL:~S" (field-value :level *active-world*))]
	[print self (format nil "   ENEMIES REMAINING:~S" *enemies*)]
	[print self "     PAINT COLOR:"]
	(when (field-value :puck char)
	  [print self nil :image (field-value :tile
					      (field-value :puck char))])
	[newline self])))

;;; Main program. 

(defparameter *xong-window-width* 800)
(defparameter *xong-window-height* 600)

(defvar *viewport*)

(defun xong ()
  (rlx:message "Initializing Xong...")
  (setf clon:*send-parent-depth* 2) 
  (rlx:set-screen-height *xong-window-height*)
  (rlx:set-screen-width *xong-window-width*)
  ;; go!
  (let* ((prompt (clone =xong-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (player (clone =player=))
	 (splash (clone =splash=))
	 (textbox (clone =textbox=))
	 (viewport (clone =viewport=))
	 (status (clone =status=))
	 (splash-prompt (clone =splash-prompt=))
	 (terminal (clone =narrator=))
	 (stack (clone =stack=)))
    ;;
    (setf *viewport* viewport)
    (setf *status* status)
    ;;
    [resize splash :height (- *xong-window-height* 20) :width *xong-window-width*]
    [move splash :x 0 :y 0]
    [resize splash-prompt :width 10 :height 10]
    [move splash-prompt :x 0 :y 0]
    [hide splash-prompt]
    [set-receiver splash-prompt splash]
    ;;
    [resize *status* :height 20 :width 500]
    [move *status* :x 0 :y 0]
    ;;
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    ;;
    (labels ((spacebar ()
	       ;;
	       ;; enable pseudo timing
	       (rlx:enable-timer)
	       (rlx:set-frame-rate 30)
	       (rlx:set-timer-interval 1)
	       (rlx:enable-held-keys 1 3)
	       ;;
	       [set-player universe player]
	       [play universe
	       	     :address (generate-level-address 1)
	       	     :prompt prompt
	       	     :narrator terminal
	       	     :viewport viewport]
	       [loadout player]
	       ;;
	       [set-character *status* player]
	       ;;
	       [set-tile-size viewport 16]
	       [resize viewport :height 470 :width *xong-window-width*]
	       [move viewport :x 0 :y 0]
	       [set-origin viewport :x 0 :y 0 
			   :height (truncate (/ (- *xong-window-height* 130) 16))
			   :width (truncate (/ *xong-window-width* 16))]
	       [adjust viewport]))
      (setf *space-bar-function* #'spacebar))
    ;;
    [set-buffer textbox
    		(find-resource-object "help-message")]
    [resize-to-fit textbox] 
    [move textbox :x 0 :y 0]
    
    (play-music "techworld" :loop t)
    (set-music-volume 255)	       
    ;;
    [resize stack :width *xong-window-width* :height (- *xong-window-height* 20)]
    [move stack :x 0 :y 0]
    [set-children stack (list viewport terminal status)]
    ;;
    [resize terminal :height 80 :width *xong-window-width*]
    [move terminal :x 0 :y (- *xong-window-height* 80)]
    [set-verbosity terminal 0]
    ;;
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    (rlx:install-widgets splash-prompt splash)
    [add-page *pager* :play prompt stack viewport terminal *status*]
    [add-page *pager* :help textbox]))

(xong)
