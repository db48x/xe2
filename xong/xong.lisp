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
  (:documentation "XONG is a colorful puzzle game in Common Lisp.")
  (:use :xe2 :common-lisp)
  (:export xong))

(in-package :xong)

;;; Counting enemies

(defvar *enemies* 0)

;;; Scoring points

(defun score (points)
  [score-points [get-player *world*] points])

;;; Colors

(defparameter *colors* '(:purple :red :blue :orange :green :yellow :white))

;;; Gates 

(defparameter *gate-timeout* 70)

(defcell gate
  (categories :initform '(:actor :obstacle :gate :exclusive))
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

(define-method is-open gate ()
  (let ((retval (null [in-category self :obstacle])))
    (prog1 retval (message "IS-OPEN: ~S" retval))))

(define-method run gate ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (zerop <clock>)
    [close self]))

;;; Yasichi

(defparameter *yasichi-bounce-time* 8)

(defsprite yasichi
  (image :initform "yasichi2")
  (bounce-clock :initform 0)
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform (random-direction))
  (categories :initform '(:actor :obstacle)))

(define-method loadout yasichi ()
  [update-position self (+ 200 (random 100)) (+ 200 (random 100))])

(define-method run yasichi ()
  (unless (zerop <bounce-clock>)
    (decf <bounce-clock>))
  [expend-action-points self 10]
  (multiple-value-bind (y x) (xe2:step-in-direction <y> <x> <direction>)
    [update-position self x y]))

(define-method do-collision yasichi (object)
  (labels ((do-box (image)
	     (prog1 t
	       (when (clon:object-p object)
		 (multiple-value-bind (x y) 
		     [viewport-coordinates self]
		   (multiple-value-bind (x0 y0)
		       [viewport-coordinates object]
		     (draw-box x0 y0 16 16 :color ".cyan" :destination image)))))))
    [>>add-overlay :viewport #'do-box])
  (if (zerop <bounce-clock>)
      (progn 
	(setf <direction> (opposite-direction <direction>))
	(setf <bounce-clock> *yasichi-bounce-time*))
      ;; another collision too soon
      (setf <direction> (random-direction))))

(define-method light-square yasichi (r c)
  (labels ((do-square (image)
	     (prog1 t
	       (multiple-value-bind (x y) 
		   [get-viewport-coordinates (field-value :viewport *world*)
					      r
					      c]
		 (draw-rectangle x y 16 16 :destination image :color ".magenta")))))
    [>>add-overlay :viewport #'do-square]))

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
  (name :initform "Chevron pack")
  (categories :initform '(:exclusive))
  (description :initform "Adds five chevrons to your inventory."))

(define-method step diamond (stepper)
  (when [in-category stepper :pointer]
    [stat-effect stepper :chevrons 5]
    [play-sample self "worp"]
    (score 1000)
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
	(unless (or [in-category stepper :enemy]
		    [in-category stepper :snowflake])
	  [die stepper]))))

(defvar *tracer-tiles* '(:north "tracer-north"
			  :south "tracer-south"
			  :east "tracer-east"
			  :west "tracer-west"))

(defcell tracer 
  (tile :initform "tracer-north")
  (categories :initform '(:actor :target :obstacle 
			  :opaque :exclusive :enemy :equipper :puck :tailed :tracer))
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
    (let ((world *world*))
      (when [obstacle-in-direction-p world row column <direction>]
	(setf <direction> (car (one-of '(:north :south :east :west)))))
      [expend-action-points self 25]
      [move self <direction>])))

(define-method drop-wire tracer ()
  [drop-cell *world* (clone =wire= :direction <direction> :clock 5)
	     <row> <column>])

(define-method move tracer (direction)
  [drop-wire self]
  [update-tile self]
  [parent>>move self direction])

(define-method loadout tracer ()
  (incf *enemies*))

*(define-method cancel tracer ()
  (decf *enemies*))

(define-method die tracer ()
  (unless <dead>
    (setf <dead> t)
    (decf *enemies*)
    (score 2000)
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
	     (prog1 t
	       (multiple-value-bind (x y) 
		   [viewport-coordinates self]
		 (let ((x0 (+ x 8))
		       (y0 (+ y 8)))
		   (draw-circle x0 y0 40 :destination image)
		   (draw-circle x0 y0 35 :destination image))))))
    [>>add-overlay :viewport #'do-circle])
  (when (< [distance-to-player self] 3.5)
    [damage [get-player *world*] 1]))

(define-method run monitor ()
  (clon:with-field-values (row column) self
    (let ((world *world*))
      (if (and (< [distance-to-player world row column] 7)
	       [line-of-sight world row column 
			      [player-row world]
			      [player-column world]])
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
    (score 5000)
    [delete-from-world self]))

;;; Replacement puck

(defcell puckup 
  (tile :initform "puckup")
  (name :initform "Replacement puck")
  (categories :initform '(:exclusive))
  (description :initform "A new puck, in case you lose the one you have."))

(define-method step puckup (stepper)
  (when [is-player stepper]
    (when (field-value :puck stepper)
      [>>narrateln :narrator "You wasted your puck."]
      [play-sample self "buzz"])
    (let ((puck (clone =puck=)))
      [drop self puck]
      (score 1000)
      [grab stepper puck]
      [die self])))

;;; Black hole eats anything in category :puck (and the player)

(defcell hole 
  (tile :initform "hole")
  (nospew :initform nil)
  (open :initform t)
  (categories :initform '(:exclusive :hole))
  (name :initform "Black hole")
  (description :initform 
"These holes eat the puck and enemies. The object of the game is to
defeat enemies by guiding them into the black holes. Be careful; black
holes can only eat one object before closing. Not only that, they
explode with deadly plasma radiation!"))

(define-method spew-plasma hole ()
  (clon:with-field-values (row column) self
    (let ((color (car (one-of *colors*))))
      (assert (and row column))
      (dotimes (n (+ 9 (random 10)))
	(let ((plasma (clone =plasma=)))
	  [set-color plasma color]
	  [set-clock plasma (+ 10 (random 10))]
	  (let ((limit 10))
	    (block placing
	      (loop do (let ((r (+ row (- (random 3) (random 5))))
			     (c (+ column (- (random 3) (random 5)))))
			 (if [line-of-sight *world* row column r c]
			     (progn 
			       [drop-cell *world* plasma r c]
			       (return-from placing))
			     ;; try again
			     (decf limit)))
		    while (plusp limit)))))))))

(define-method step hole (stepper)
  (when <open>
    (assert (and <row> <column>))
    (unless <nospew>
      [spew-plasma self])
    (progn [play-sample self "hole-suck"]
	   (if [in-category stepper :puck]
	       [die stepper]
	       (when [is-player stepper]
		 [damage stepper 1]))
	   (setf <open> nil)
	   (setf <tile> "hole-closed"))))

(define-method initialize hole (&key nospew)
  (setf <nospew> nospew))

;;; Snakes are the body components of a snake

(defvar *snake* nil)

(defun snake-living-p ()
  (labels ((smashed (c)
	     (field-value :smashed c)))
    (notevery #'smashed *snake*)))

(defvar *snake-tiles* '(:purple "brick-purple"
			:black "brick-black"
			:red "brick-red"
			:blue "brick-blue"
			:orange "brick-orange"
			:green "brick-green"
			:white "brick-white"
			:yellow "brick-yellow"))

(defparameter *snake-escape-time* 100)

(defcell snake 
  (tile :initform "snake-white")
  (smashed :initform nil)
  (speed :initform (make-stat :base 20))
  (movement-cost :initform (make-stat :base 60))
  (escape-clock :initform 0)
  (ahead :initform nil)
  (behind :initform nil)
  (color :initform :white)
  (direction :initform :south)
  (categories :initform '(:obstacle :exclusive :paintable :actor :snake))
  (description :initform "The deadly Snake's body segments must be painted to defeat it."))

(define-method set-color snake (c)
  (setf <color> c)
  (let ((res (getf *snake-tiles* c)))
    (assert (stringp res))
    (setf <tile> res)))

(define-method paint snake (color)
  (if (and (eq <color> color) (not <smashed>))
      (progn [play-sample self "lock-opening-sound"]
	     (score 1000)
	     (setf <smashed> t)
	     (setf <tile> "brick-smashed"))
      [play-sample self "error"]))

(define-method attach snake (piece)
  (setf <behind> piece)
  (setf (field-value :ahead piece) self))

(define-method adjacent-gate snake ()
  (clon:with-field-values (row column) self
    (block searching
      (dolist (dir '(:north :south :east :west))
	(multiple-value-bind (r c) (step-in-direction row column dir)
	  (let ((gate [category-at-p *world* r c :gate]))
	    (when (and (clon:object-p gate)
		       [is-open gate]
		       (zerop <escape-clock>))
	      (setf <escape-clock> *snake-escape-time*)
	      (return dir))))))))	

(define-method probe snake (direction)
  (let ((retval (clon:with-field-values (row column) self
		  (multiple-value-bind (r c) (step-in-direction row column direction)
		    (if (and [in-bounds-p *world* r c]
			     (not [category-at-p *world* r c :obstacle]))
			;; all clear
			direction
			;; allow overlapping self
			(when (or [category-at-p *world* r c :snake]
				  ;; try to escape the room
				  (and (let ((gate [category-at-p *world* r c :gate]))
					 (when (clon:object-p gate)
					   [is-open gate]))))
			  direction))))))
    (when retval 
      (prog1 retval [expend-action-points self 20]))))
	      
(define-method run snake ()
  (setf <escape-clock> (max 0 (1- <escape-clock>)))
  (clon:with-field-values (row column) self
    (when (null <ahead>)
      ;; kill player if adjacent
      (when [adjacent-to-player self]
	[damage [get-player *world*] 1])
      ;; we are the head of the snake
      (let ((dir (or [adjacent-gate self] <direction>)))
	(if [probe self dir]
	    (progn [move self dir :ignoring-obstacles]
		   (let ((piece <behind>)
			 (r row)
			 (c column)
			 next-r next-c)
		     (loop while piece
			   do [>>move piece (direction-to (setf next-r (field-value :row piece))
							  (setf next-c (field-value :column piece))
						      r c) :ignoring-obstacles]
			      (setf r next-r
				    c next-c
				    piece (field-value :behind piece)))))
	    (let ((dir (car (one-of '(:north :south :east :west)))))
	      (setf <direction> (or [probe self dir] <direction>))))))))

;; (define-method spawn snake ()
;;   (let ((dir (car (one-of '(:north :south :east :west))))
;; 	(snake (clone =snake=)))
;;     (multiple-value-bind (r c)
;; 	(step-in-direction <row> <column> dir)
;;       [drop-cell *world* snake r c :probe t :exclusive t])))

;;; Door to next level

(define-prototype door (:parent xe2:=gateway=)
  (tile :initform "door")
  (name :initform "Level exit")
  (description :initform "Door to the next level of Xong.")
  (categories :initform '(:gateway :actor :exclusive :obstacle))
  (address :initform nil))

(define-method level door (lev)
  (setf <address> (generate-level-address lev)))

(define-method step door (stepper)
  (when [is-player stepper]
    (if (and (zerop *enemies*) (not (snake-living-p)))
	(progn 
	  (score 20000)
	  [play-sample self "go"]
	  [say self "You made it to the next level!"]
	  [activate self])
	[play-sample self "error"])))

(define-method run door ()
  (when (and (zerop *enemies*) (not (snake-living-p)))
    [delete-category self :obstacle]
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

(define-method die wall ()
  (score 100)
  [parent>>die self])

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

(defparameter *shield-time* 70)

(defcell player 
  (tile :initform "player")
  (name :initform "Player")
  (score :initform (make-stat :base 0 :min 0))
  (shield-clock :initform 0)
  (last-direction :initform :north)
  (dead :initform nil)
  (puck :initform nil)
  (chevrons :initform (make-stat :base 5 :min 0 :max 10))
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (strength :initform (make-stat :base 13))
  (tail-length :initform (make-stat :base 20 :min 0))
  (dexterity :initform (make-stat :base 13))
  (defense :initform (make-stat :base 15))
  (equipment-slots :initform '(:left-hand :right-hand))
  (hearing-range :initform 1000)
  (hit-points :initform (make-stat :base 1 :min 0 :max 2))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (stepping :initform t)
  (attacking-with :initform :right-hand)
  (light-radius :initform 3)
  (categories :initform '(:actor :tailed :player :target :above
			  :container :light-source :pointer))
  (description :initform "This is you! Move with the arrow keys or numeric keypad."))

(define-method run player ()
  (unless <dead>
    (setf <tile> (if (null <puck>)
		     "player-empty"
		     (getf *player-tiles* (if (has-field :color <puck>)
					      (field-value :color <puck>)
					      :other))))
    [run-shield self]
    [step-on-current-square self]))

(define-method run-shield player (&optional clock)
  (clon:with-fields (shield-clock row column) self
    (when clock (setf shield-clock clock))
    (decf shield-clock)
    ;; warning when about to expire
    (when (= 10 shield-clock)
      [play-sample self "shield-warning"])
    (if (plusp shield-clock)
	(labels ((draw-shield (image)
		   (prog1 t (multiple-value-bind (x y)
				[viewport-coordinates self]
			      (let ((circles (1+ (truncate (/ shield-clock 5))))
				    (radius 16))
				(dotimes (n circles)
				  (draw-circle (+ x 8) (+ y 8) radius :color ".cyan" :destination image)
				  (incf radius 2)))))))
	  [play-sample self "shield-sound"]
	  [>>add-overlay :viewport #'draw-shield]))))
	
(define-method damage player (points)
  (when (not (plusp <shield-clock>))
    [parent>>damage self points]))
   
(define-method score-points player (points)
  [stat-effect self :score points]
  [>>narrateln :narrator (format nil "Scored ~S points." points)])

(define-method quit player ()
  (xe2:quit :shutdown))

(define-method step player (stepper)
  (when [in-category stepper :item]
    [grab self stepper])
  (when [in-category stepper :snake]
    [damage self 1]))

(define-method drop-tail player ()
  [drop self (clone =tail= 
		    :direction <last-direction> 
		    :clock [stat-value self :tail-length])])

(define-method restart player ()
  (let ((player (clone =player=)))
    [destroy *universe*]
    [set-player *universe* player]
    [set-character *status* player]
    [play *universe*
	  :address '(=menu-world=)]
    [loadout player]
    [play-sample self "go"]))

(define-method drop-chevron player (direction)
  (unless <dead>
    (if (zerop [stat-value self :chevrons])
	(progn [play-sample self "error"]
	       [say self "You don't have any chevrons to drop."])
	(if [category-at-p *world* <row> <column> :chevron]
	    (progn [play-sample self "error"]
		   [say self "You can't drop a chevron on top of another chevron."])
	    (let ((chevron (clone =chevron=)))
	      [drop self chevron]
	      [play-sample self "powerup"]
	      [stat-effect self :chevrons -1]
	      [orient chevron direction])))))

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
	[drop-cell *world* puck <row> <column> :no-stepping t]
	[kick puck direction]
	(setf puck nil)
	[play-sample self "serve"]))))

(define-method grab player (puck)
  (assert [in-category puck :puck])
  (setf <puck> puck)
  [delete-from-world puck]
  [play-sample self "grab"])

(define-method die player ()
  (unless <dead>
    (setf <tile> "skull")
    [play-sample self "death"]
    [say self "You died. Press ESCAPE to try again."]
    (setf <dead> t)))

;;; Text overlay balloons

(defcell balloon 
  (categories :initform '(:drawn :actor))
  text stroke-color background-color timeout)

(define-method initialize balloon (&key text (stroke-color ".white") (background-color ".blue")
					(style :balloon) (timeout nil))
  (setf <text> text)
  (setf <stroke-color> stroke-color)
  (setf <background-color> background-color)
  (setf <style> style)
  (setf <timeout> (if (floatp timeout)
		      ;; specify in (roughly) seconds if floating
		      (truncate (* 15 timeout))
		      ;; leave as frames if integer
		      timeout)))
  
(define-method draw balloon (x y image)
  (clon:with-field-values (text style) self
    (let* ((offset (ecase style
		     (:balloon 16)
		     (:flat 0)))
	   (x0 (+ x offset))
	   (y0 (+ y offset))
	   (x1 (+ x0 offset))
	   (y1 (+ y0 offset))
	   (margin 4)
	   (height (+ (* 2 margin) (apply #'+ (mapcar #'formatted-line-height text))))
	   (width (+ (* 2 margin) (apply #'max (mapcar #'formatted-line-width text)))))
      (draw-box x1 y1 width height 
		:stroke-color <stroke-color>
		:color <background-color>
		:destination image)
      (when (eq style :balloon)
	(draw-line x0 y0 x1 y1 :destination image))
      (let ((x2 (+ margin x1))
	    (y2 (+ margin y1)))
	(dolist (line text)
	  (render-formatted-line line x2 y2 :destination image)
	  (incf y2 (formatted-line-height line)))))))

(define-method run balloon ()
  [expend-default-action-points self]
  (when (integerp <timeout>)
    (when (minusp (decf <timeout>))
      [die self])))
		  
;;; Tutorial NPC: Beckoner

(defparameter *greeting-text* '((("Hello there!"))))

(defparameter *beckon-text* '((("Use the arrow keys")) (("to approach me."))))

(defparameter *success-text* '((("Good job. You've mastered the use")) (("of the arrow keys."))))

(defparameter *bye-text* '((("You can exit through one")) (("of the portals."))
			   (("Portals look like this: ") (nil :image "portal"))))

(defcell beckoner 
  (tile :initform "npc")
  (state :initform 0)
  (categories :initform '(:obstacle :actor :npc))
  (timeout :initform 40))
      
(define-method emote beckoner (text &optional (timeout 3.0))
  (let ((balloon (clone =balloon= :text text :timeout timeout)))
    [play-sample self "talk"]
    [drop self balloon]))

(define-method run beckoner ()
  (clon:with-fields (state timeout) self
    [expend-default-action-points self]
    (labels ((act ()
	       (ecase state 
		 (0 [emote self *greeting-text* 1.0]
		    (incf state)
		    (setf timeout 20))
		 (1 [emote self *beckon-text*]
		    (setf timeout 100))
		 (2 [emote self *success-text*]
		    (incf state)
		    (setf timeout 50))
		 (3 [emote self *bye-text*]
		    (setf timeout 200)))))
      (if (and (not (= 3 state))
	       (< [distance-to-player self] 5))
	  (progn (setf state 2) (act))
	  (if (null timeout)
	      (act)
	      (when (minusp (decf timeout))
		(setf timeout nil)
		(act)))))))

;;; Controlling the game

(define-prototype xong-prompt (:parent xe2:=prompt=))

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
    ;;
    ("KP8" (:meta) "drop-chevron :north .")
    ("KP4" (:meta) "drop-chevron :west .")
    ("KP6" (:meta) "drop-chevron :east .")
    ("KP2" (:meta) "drop-chevron :south .")
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
    ("DOWN" (:alt) "drop-chevron :south .")
    ;;
    ("UP" (:meta) "drop-chevron :north .")
    ("LEFT" (:meta) "drop-chevron :west .")
    ("RIGHT" (:meta) "drop-chevron :east .")
    ("DOWN" (:meta) "drop-chevron :south .")))


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
	    ("K" (:meta) "drop-chevron :north .")
	    ("H" (:meta) "drop-chevron :west .")
	    ("L" (:meta) "drop-chevron :east .")
	    ("J" (:meta) "drop-chevron :south .")
	    ;;
	    ("P" (:control) "pause .")
	    ("PAUSE" nil "pause .")
	    ("ESCAPE" nil "restart .")
	    ("Q" (:control) "quit ."))))
  
(define-method install-keybindings xong-prompt ()
  (dolist (k *qwerty-keybindings*)
      (apply #'bind-key-to-prompt-insertion self k))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *world* :timer])])

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
  ;; (when [category-at-p *world* <row> <column> :player]
  ;;   [grab [get-player *world*] self]))

(define-method paint puck (color)
  (setf <color> color)
  (setf <tile> (getf *puck-tiles* color)))

(define-method move puck (direction)
  (multiple-value-bind (r c) 
      (step-in-direction <row> <column> direction)
    (let ((obstacle [obstacle-at-p *world* r c]))
      (when obstacle
	[bounce self]
	(when (clon:object-p obstacle)
	  (if [is-player obstacle]
	      [grab obstacle self]
	      ;; it's not the player. see if we can color, or get paint
	      (progn 
		(when [in-category obstacle :paintable]
		  [paint obstacle <color>])
		(when [in-category obstacle :breakable]
		  [die obstacle])
		(when [in-category obstacle :wall]
		  [paint self (field-value :color obstacle)]
		  [die obstacle]
		  [parent>>move self direction])
		(when [in-category obstacle :gate]
		  [open obstacle]
		  (when [category-at-p *world* <row> <column> :player]
		    [parent>>move self direction]))
		(when [in-category obstacle :bulkhead]
		  [parent>>move self direction :ignore-obstacles]))))))
    (when [is-located self]
      [parent>>move self <direction>])))

(define-method run puck ()
  ;; pucks don't stop moving.
  (if (eq :here <direction>)
      [die self]
      [move self <direction>]))

(define-method die puck ()
  [say self "You lost your puck!"]
  [play-sample self "buzz"]
  [parent>>die self])

;;; Powerup mystery box

(defcell mystery-box
  (name :initform "Mystery box")
  (tile :initform "mystery-box")
  (categories :initform '(:target :obstacle :breakable :exclusive))
  (description :initform  "Break it open to find a surprise inside!"))

(define-method die mystery-box ()
  (let ((item (clone (car (one-of (list =snowflake= =shield=))))))
    [drop self item]
    [parent>>die self]))

;;; Special puck: snowflake

(defcell snowflake
  (tile :initform "snowflake")
  ;; not paintable
  (categories :initform '(:puck :target :actor :item :snowflake))
  (description :initform 
"A puck that freezes enemies for a brief time.
You must drop any other puck in order to pick this up.")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform :here)
  (stepping :initform t))

(define-method bounce snowflake ()
  (setf <direction> (opposite-direction <direction>))
  [play-sample self "bounce"])

(define-method move snowflake (direction)
  (multiple-value-bind (r c) 
      (step-in-direction <row> <column> direction)
    (let ((obstacle [obstacle-at-p *world* r c]))
      (when obstacle
	[bounce self]
	(when (clon:object-p obstacle)
	  (if [is-player obstacle]
	      [grab obstacle self]
	      ;; if it's an enemy or puck, freeze it!
	      (progn 
		(when (or [in-category obstacle :puck]
			  [in-category obstacle :enemy])
		  [freeze self obstacle])
		(when [in-category obstacle :gate]
		  [open obstacle])))))
      (when [is-located self]
	[parent>>move self <direction>]))))

(define-method kick snowflake (direction)
  (setf <direction> direction)
  (when [category-at-p *world* <row> <column> :player]
    [move self direction]))

(define-method freeze snowflake (enemy)
  [play-sample self "freeze"]
  [expend-action-points enemy 100 -100])

(define-method paint snowflake (color)
  nil)

(define-method step snowflake (stepper)
  (if (and [is-player stepper]
	   (null (field-value :puck stepper)))
      (progn (score 1000)
	     [grab stepper self])
      (when [in-category stepper :enemy]
	[bounce self]
	[freeze self stepper])))

(define-method run snowflake ()
  (unless (eq :here <direction>)
    [move self <direction>]))

(define-method die snowflake ()
  [say self "The snowflake was destroyed."]
  [play-sample self "buzz"]
  [parent>>die self])

;;; Special puck: shield

(defcell shield
  (tile :initform "shield")
  ;; not paintable
  (categories :initform '(:puck :target :item :shield))
  (player :initform nil)
  (description :initform 
"A puck that creates a shield around you when fired.
You must drop any other puck in order to pick this up.")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform :here)
  (stepping :initform t))

(define-method kick shield (direction)
  (when <player>
    [run-shield <player> *shield-time*]
    [die self]))

(define-method step shield (stepper)
  (if (and [is-player stepper]
	   (null (field-value :puck stepper)))
      (progn (score 1000)
	     [grab stepper self]
	     (setf <player> stepper)
	     [say self "You picked up the shield puck. Fire it to become invulnerable!"])))

;;; Radioactive gas

(defvar *plasma-tiles* '(:purple "plasma-purple"
			:black "plasma-black"
			:red "plasma-red"
			:blue "plasma-blue"
			:orange "plasma-orange"
			:green "plasma-green"
			:white "plasma-white"
			:yellow "plasma-yellow"))

(defcell plasma
  (tile :initform "plasma-white")
  (color :initform :white)
  (name :initform "Toxic paint plasma")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (clock :initform 100)
  (categories :initform '(:actor :paint-source :plasma))
  (description :initform "Spreading toxic paint gas. Avoid at all costs!"))

(define-method step plasma (stepper)
  (when [is-player stepper]
    [damage stepper 1]))

(define-method set-color plasma (color)
  (setf <color> color)
  (setf <tile> (getf *plasma-tiles* color)))

(define-method set-clock plasma (clock)
  (setf <clock> clock))

(define-method run plasma ()
  [play-sample self "plasma"]
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      (progn 
	(do-cells (cell [cells-at *world* <row> <column>])
	  (when (has-field :hit-points cell)
	    [damage cell 1]))
	(let ((dir (random-direction)))
	  (multiple-value-bind (r c) (step-in-direction <row> <column> dir)
	    (let ((brick [category-at-p *world* r c :wall]))
	      (if brick
		  (progn 
		    [paint brick <color>]
		    [die self])
		  [move self dir])))))))

;;; Muon bullets

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

(defcell muon-trail
  (categories :initform '(:actor))
  (clock :initform 2)
  (speed :initform (make-stat :base 10))
  (default-cost :initform (make-stat :base 10))
  (tile :initform ".gear")
  (direction :initform :north))

(define-method orient muon-trail (direction)
  (setf <direction> direction)
  (setf <tile> (getf *trail-middle-tiles* direction)))

(define-method run muon-trail ()
  (setf <tile> (getf (nth <clock> *trail-tile-map*)
		     <direction>))
  [expend-default-action-points self]
  (decf <clock>)
  (when (minusp <clock>)
    [die self]))

;;; Basic muon particle

(defcell muon-particle 
  (categories :initform '(:actor :muon :target))
  (speed :initform (make-stat :base 20))
  (default-cost :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 20))
  (attack-power :initform 5)
  (tile :initform "muon")
  (name :initform "Muon particle")
  (firing-sound :initform "muon-fire")
  (direction :initform :here)
  (clock :initform 12)
  (description :initform
"This high-energy particle will kill you instantly."))

(define-method initialize muon-particle (&key attack-power)
  (when attack-power
    (setf <attack-power> attack-power)))

(define-method drop-trail muon-particle (direction)
  (let ((trail (clone =muon-trail=)))
    [orient trail direction]
    [drop self trail]))

(define-method find-target muon-particle ()
  (let ((target [category-in-direction-p *world* 
					 <row> <column> <direction>
					 '(:obstacle :target)]))
    (if target
	(progn
	  [>>move self <direction>]
	  [>>expend-default-action-points self]
	  [>>push target <direction>]
	  [>>damage target <attack-power>]
	  [>>die self])
	(multiple-value-bind (r c) 
	    (step-in-direction <row> <column> <direction>)
	  (if (not (array-in-bounds-p (field-value :grid *world*) r c))
	      [die self]
	      (progn [drop-trail self <direction>]
		     [>>move self <direction>]))))))

(define-method step muon-particle (stepper)
  [damage stepper <attack-power>]
  [die self])
  
(define-method update-tile muon-particle ()
  (setf <tile> (getf *muon-tiles* <direction>)))

(define-method run muon-particle ()
  [update-tile self]
  [find-target self]
  (decf <clock>)
  (when (zerop <clock>)
    [>>die self]))

(define-method impel muon-particle (direction)
  (assert (member direction *compass-directions*))
  (setf <direction> direction)
  ;; don't hit the player
  ;;  [move self direction]
  [play-sample self <firing-sound>]
  [find-target self])

;;; The Oscillator

(defcell oscillator 
  (tile :initform "oscillator")
  (categories :initform '(:actor :obstacle :target :enemy :opaque :oscillator :puck))
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 20))
  (default-cost :initform (make-stat :base 20))
  (direction :initform (car (one-of '(:south :west))))
  (stepping :initform t)
  (dead :initform nil)
  (name :initform "Oscillator")
  (description :initform 
"These bounce back and forth very quickly, firing muon particles if
the player gets too close."))

(define-method get-nasty oscillator ()
  [damage [get-player *world*] 1])

(define-method loadout oscillator ()
  (incf *enemies*))
  
(define-method cancel oscillator ()
  (decf *enemies*))

(define-method run oscillator ()
  (if [obstacle-in-direction-p *world* <row> <column> <direction>]
      (setf <direction> (opposite-direction <direction>))
      (progn [move self <direction>]
	     (if (and (> 8 [distance-to-player self])
		      [line-of-sight *world* <row> <column> 
				     [player-row *world*]
				     [player-column *world*]])
		 [fire self [direction-to-player self]]))))

(define-method fire oscillator (direction)
  (let ((muon (clone =muon-particle=)))
    [drop self muon]
    [expend-action-points self 100]
    [impel muon direction] ))

(define-method damage oscillator (points)
  [get-nasty self])

(define-method die oscillator ()
  (unless <dead>
    (decf *enemies*)
    (score 5000)
    [play-sample self "death-alien"]
    [parent>>die self]))
    
(define-method kick oscillator (direction)
  (setf <direction> direction)
  [move self direction])

;;; Bulkheads are indestructible walls

(defcell bulkhead
  (name :initform "Bulkhead")
  (tile :initform "bulkhead")
  (categories :initform '(:obstacle :bulkhead :exclusive))
  (description :initform "It's an indestructible wall."))

;;; Xong game board

(defun generate-level-address (n)
  (assert (and (integerp n) (plusp n)))
  (list '=xong= 
	:level n
	:extenders (truncate (/ (* 3 (1- n)) 2))
	:tracers (+ 4 (truncate (/ (* (1- n) 2) 3)))
	:monitors (if (= n 1)
		      0
		      (* 2 (truncate (/ n 2))))
	:rooms 1
	:mystery-boxes (+ 1 (truncate (/ n 2)))
	:oscillators (* (max 0 (- n 2)) (truncate (/ n 4)))
	:puzzle-length (+ 4 (truncate (/ n 3)))
	:extra-holes (+ 1 (truncate (/ n 3)))
	:puckups (+ 4 (truncate (* (1- n) 2.5)))
	:diamonds (+ 9 (* (1- n) 3))
	:swatches (+ 10 (truncate (* 1.6 n)))))

(defparameter *xong-level-width* 50)
(defparameter *xong-level-height* 29)

(define-prototype xong (:parent xe2:=world=)
  (name :initform "Xong board")
  (description :initform 
	       '((("Welcome to Xong." :foreground ".white" :background ".blue")
		  ("Press F1 for general help" :foreground ".white" :background ".red")
		  (", or click any object." :foreground ".white" :background ".blue"))))
  (edge-condition :initform :block)
  (level :initform 1)
  (width :initform *xong-level-width*)
  (height :initform *xong-level-height*)
  (scale :initform '(1 nm))
  (ambient-light :initform :total))

(define-method drop-snake xong (column row1 row2)
  (setf *snake* nil)
  (let (piece last-piece)
    (labels ((drop-piece (r c)
	       (progn nil
		      (setf piece (clone =snake=))
		      (push piece *snake*)
		      [drop-cell *world* piece r c]
		      [set-color piece (car (one-of *colors*))]
		      (when last-piece
			[attach last-piece piece])
		      (setf last-piece piece))))
      (trace-column #'drop-piece column row1 row2))))

(define-method drop-room xong (row column height width 
				   next-level puzzle-length &optional (material =bulkhead=))
  (let (rectangle openings)
    (labels ((collect-point (&rest args)
	       (prog1 nil (push args rectangle)))
	     (drop-wall (r c)
	       (unless (and (= r 0)
			    (= c 0))
		 (let ((wall (clone =bulkhead=)))
		   [replace-cells-at self r c wall]
		   [set-location wall r c]))))
      (trace-rectangle #'collect-point row column height width)
      ;; make sure there are openings
      (dotimes (i 6)
	(let* ((n (random (length rectangle)))
	       (point (nth n rectangle)))
	  (destructuring-bind (r c) point
	    ;; don't make gate holes on corners or above exit
	    (unless (or (and (= r row) (= c (+ -1 column (truncate (/ width 2)))))
			(and (= r row) (= c (+ column (truncate (/ width 2)))))
			(and (= r row) (= c (+ 1 column (truncate (/ width 2)))))
			(and (= r row) (= c column)) ;; top left 
			(and (= r row) (= c (+ -1 column width))) ;; top right
			(and (= r (+ -1 row height)) (= c column)) ;; bottom left
			(and (= r (+ -1 row height)) (= c (+ -1 column width)))) ;; bottom right
	      (push (nth n rectangle) openings)
	      (setf rectangle (delete (nth n rectangle) rectangle))))))
      ;; draw walls
      (dolist (point rectangle)
	(destructuring-bind (r c) point
	  (drop-wall r c)))
      ;; draw gates
      (dolist (point openings)
	(destructuring-bind (r c) point
	  [replace-cells-at self r c (clone =gate=)]))
      ;; drop floor, obliterating what's below
      (labels ((drop-floor (r c)
		 (prog1 nil
		   [replace-cells-at self r c (clone =floor=)])))
	(trace-rectangle #'drop-floor (1+ row) (1+ column) (- height 2) (- width 2) :fill)
	;; drop lock puzzle
	(let ((col (+ column (truncate (/ width 2)))))
	  (trace-column #'drop-wall (- col 1) row (+ 1 row puzzle-length))
	  [drop-snake self col (+ row 2) (+ 1 row puzzle-length)]
	  (trace-column #'drop-wall (+ col 1) row (+ 1 row puzzle-length))
	  ;; drop door
	  (let ((door (clone =door=)))
	    [level door next-level]
	    [drop-cell self (clone material) (+ 1 row) col]
	    [drop-cell self door (+ 2 row) col])
	  ;; drop a puck or two
	  (dotimes (n (1+ (random 2)))
	    [drop-cell self (clone =puckup=) (+ 2 (random 2) row)
		       (+ 2 (random 2) column)]))))))
  
(define-method generate xong (&key (level 1)
				   (extenders 0)
				   (tracers 4)
				   (rooms 1)
				   (mystery-boxes 2)
				   (oscillators 3)
				   (puzzle-length 4)
				   (puckups 4)
				   (extra-holes 4)
				   (monitors 3)
				   (diamonds 6)
				   (swatches 8))
  [create-default-grid self]
  (setf <level> level)
  (setf *enemies* 0)
  (clon:with-fields (height width grid player) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))
    (dotimes (n (+ swatches 3))
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
	    (unless (= 0 r c)
	      (let ((hr (+ r 2 (random 3)))
		    (hc (+ c 2 (random 3))))
		[replace-cells-at self hr hc
				  (clone =floor=)]
		[drop-cell self (clone =hole=) hr hc]
		(trace-rectangle #'drop-wall r c
				 (+ 4 (random 8)) (+ 4 (random 8)) :fill)))))))
    (dotimes (n extra-holes)
      (multiple-value-bind (r c) [random-place self]
	[drop-cell self (clone =hole=) r c]))
    (dotimes (n rooms)
      [drop-room self 
		 (+ 5 (random (- height 20)))
		 (+ 5 (random (- width 20)))
		 (+ 10 (random 6)) (+ 10 (random 4)) (+ level 1) puzzle-length])
    (dotimes (n monitors)
      (let ((monitor (clone =monitor=)))
	(multiple-value-bind (r c)
	    [random-place self :avoiding player :distance 10]
	  [drop-cell self monitor r c :loadout t])))
    ;; EXPERIMENTAL
    ;; (dotimes (n )
    ;;   (let ((balloon (clone =balloon= :text '((("This is some") (" formatted " :foreground ".red") ("text."))
    ;; 					      (("Here's some more."))) :style :balloon)))
    ;; 	(multiple-value-bind (r c)
    ;; 	    [random-place self :avoiding player :distance 10]
    ;; 	  (setf (field-value :tile balloon) "yasichi")
    ;; 	  [drop-cell self balloon r c :loadout t])))
    ;; ;; EXPERIMENTAL
    ;; (dotimes (n 1)
    ;;   (let ((npc (clone =beckoner=)))
    ;; 	(multiple-value-bind (r c)
    ;; 	    [random-place self :avoiding player :distance 10]
    ;; 	  [drop-cell self npc r c :loadout t])))
    ;;
    ;; EXPERIMENTAL
    ;; (dotimes (n 20)			
    ;;   (let ((p (clone =particle=)))
    ;; 	[loadout p]
    ;; 	[add-sprite self p]))
    ;; (dotimes (n 4)
    ;;   (let ((p (clone =yasichi=)))
    ;; 	[loadout p]
    ;; 	[add-sprite self p]))
    ;;
    ;;
    (dotimes (n tracers)
      (let ((tracer (clone =tracer=)))
	(multiple-value-bind (r c)
	    [random-place self :avoiding player :distance 10]
	  [drop-cell self tracer r c :loadout t])))
    (dotimes (n oscillators)
      (let ((oscillator (clone =oscillator=)))
	(multiple-value-bind (r c)
	    [random-place self :avoiding player :distance 10]
	  [drop-cell self oscillator r c :loadout t])))
    (dotimes (n extenders)
      (multiple-value-bind (r c) [random-place self]
	[drop-cell self (clone =extender=) r c]))
    (dotimes (n diamonds)
      (multiple-value-bind (r c) [random-place self]
	[drop-cell self (clone =diamond=) r c]))
    (dotimes (n puckups)
      (multiple-value-bind (r c) [random-place self]
	[drop-cell self (clone =puckup=) r c]))
    (dotimes (n mystery-boxes)
      (multiple-value-bind (r c) [random-place self]
	[drop-cell self (clone =mystery-box=) r c]))))

(define-method begin-ambient-loop xong ()  
  (play-music (car (one-of '("flyby" "sparqq" "synthy" "neon" "phong" "xong-theme" "pensive" "toybox"))) :loop t))

;;; Other level gates

(defcell portal 
  text
  (tile :initform "portal") 
  (name :initform "Portal")
  (categories :initform '(:above))
  (description :initform "A doorway to another level.")
  (address :initform nil))

(define-method initialize portal (&key address text)
  (when address
    (setf <address> address))
  (when text 
    (setf <text> text)))

(define-method loadout portal ()
  (when <text> [drop self (clone =balloon= :text <text>)]))

(define-method step portal (stepper)
  (when [is-player stepper]
    [play *universe* :address <address> :player [get-player *world*]]))

;;; Dancefloor

(defparameter *light-tiles* '("floor"
			      "rezlight5"
			      "rezlight4"
			      "rezlight3"
			      "rezlight2"
			      "rezlight1"))

(defparameter *light-clock* 12)

(defcell dancefloor 
  (tile :initform nil)
  (repeating :initform nil)
  (clock :initform *light-clock*)
  (categories :initform '(:actor :dancefloor)))

(define-method initialize dancefloor (&key repeating (clock *light-clock*))
  (setf <clock> clock)
  (setf <repeating> repeating))

(define-method update-tile dancefloor ()
  (when [is-located self]
    (setf <tile> 
	  (if [category-at-p *world* <row> <column> '(:obstacle :above)]
	      nil
	      (nth (truncate (/ <clock> 2)) *light-tiles*)))))

(define-method light dancefloor (&optional (time *light-clock*))
  (setf <clock> (max 0 time))
  [update-tile self])

(define-method light-toward dancefloor (direction)
  (multiple-value-bind (r c) (step-in-direction <row> <column> direction)
    (unless [category-at-p *world* r c :dancefloor]
      (let ((dancefloor (clone =dancefloor=)))
	[drop-cell *world* dancefloor r c]
	[light dancefloor (max 0 (- <clock> 1))]))))
      
(define-method light-plus dancefloor ()
  (dolist (dir '(:north :south :east :west))
    [light-toward self dir]))
		
(define-method run dancefloor ()
  (setf <clock> (max 0 (- <clock> 1)))
  (if (plusp <clock>)
      (progn
	(when (< 2 <clock>)
	  [light-plus self])
	[update-tile self])
      [die self]))

;;; Karma

(defparameter *karma-tiles* '("rezblur5"
			       "rezblur4"
			       "rezblur3"
			       "rezblur2"
			       "rezblur1"))

(defparameter *karma-samples* '("zap1" "zap2" "zap3"))

(defparameter *karma-alt-samples* '("zap4" "zap5" "zap6" "zap7"))

(defparameter *karma-sample-schemes* (list *karma-samples* *karma-alt-samples*))

(defcell karma
  (tile :initform "rezblur1")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (clock :initform 20)
  (samples :initform *karma-samples*)
  (categories :initform '(:actor :paint-source :karma)))

(define-method initialize karma (&key clock)
  (setf <clock> clock))

(define-method set-clock karma (clock)
  (setf <clock> clock))

(define-method run karma ()
  (when <clock> (decf <clock>))
  (if (and <clock> (> 0 <clock>))
      [die self]
      (let ((dir (random-direction)))
	(setf <tile> (car (one-of *karma-tiles*)))
	(percent-of-time 1
	  [play-sample self (car (one-of <samples>))]
	  [drop self (clone =dancefloor=)])
	[move self dir])))

;;; Menu level

(define-prototype menu-world (:parent xe2:=world=)
  (height :initform *xong-level-height*)
  (width :initform *xong-level-width*)
  (edge-condition :initform :block)
  (ambient-light :initform :total))

(define-method generate menu-world (&rest args)
  [create-default-grid self]
  (setf <level> 0)
  (clon:with-fields (height width grid player) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))
    (dotimes (i 10)
      [drop-cell self (clone =karma= :clock nil) (random height) (random width)])
    (let ((beckoner (clone =beckoner=))
	  (urhere (clone =balloon= 
			 :style :flat
			 :timeout 3.0
			 :text '((("<----- YOU ARE HERE. Use the arrow keys to move."))))))
      [drop-cell self urhere 0 2 :loadout t]
      [drop-cell self beckoner 8 14]
      [drop-cell self (clone =portal= 
			     :address (generate-level-address 1)
			     :text '((("To Level 1"))))
		 3 12 :loadout t]
      [drop-cell self (clone =portal=
			     :address '(=puckman-world=)
			     :text '((("To the Tutorial"))))
		 10 5 :loadout t])))

(define-method begin-ambient-loop menu-world ()
  (play-music "flyby" :loop t))

;;; Tutorial level 1: puckman

(defvar *bricks* 0)

(defcell brick
  (tile :initform "wall-red")
  (categories :initform '(:obstacle :paint-source :wall :brick))
  (color :initform :red))

(define-method initialize brick ()
  (incf *bricks*))

(define-method die brick ()
  (decf *bricks*)
  [parent>>die self])
    
(defparameter *puck-welcome-text* '((("Hello and welcome to the tutorial!"))))

(defparameter *puck-instruction-text* '((("Use CONTROL with an arrow-key to"))
					(("fire your puck at the bricks."))
					(("See if you can break them all."))))

(defparameter *puck-success-text* '((("Good job. You've mastered the use")) (("of the puck."))))

(defparameter *puck-bye-text* '((("You can exit through the")) (("portal to the southeast."))))

(defcell puckman 
  (tile :initform "npc")
  (state :initform 0)
  (categories :initform '(:obstacle :actor :npc))
  (timeout :initform 10))
      
(define-method emote puckman (text &optional (timeout 3.0))
  (let ((balloon (clone =balloon= :text text :timeout timeout)))
    [play-sample self "talk"]
    [drop self balloon]))

(define-method run puckman ()
  (message "BRICKS ~S" *bricks*)
  (clon:with-fields (state timeout) self
    [expend-default-action-points self]
    (labels ((act ()
	       (ecase state 
		 (0 [emote self *puck-welcome-text* 1.0]
		    (incf state)
		    (setf timeout 20))
		 (1 [emote self *puck-instruction-text*]
		    (setf timeout 200))
		 (2 [emote self *puck-success-text*]
		    (incf state)
		    (setf timeout 60))
		 (3 [emote self *puck-bye-text*]
		    (setf timeout 200)))))
      (if (and (not (= 3 state))
	       (zerop *bricks*))
	  (progn (setf state 2) (act))
	  (if (null timeout)
	      (act)
	      (when (minusp (decf timeout))
		(setf timeout nil)
		(act)))))))

(define-prototype puckman-world (:parent xe2:=world=)
  (height :initform *xong-level-height*)
  (width :initform *xong-level-width*)
  (edge-condition :initform :block)
  (ambient-light :initform :total))

(define-method generate puckman-world (&rest args)
  [create-default-grid self]
  (setf <level> 0 *bricks* 0)
  (clon:with-fields (height width grid player) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))
    (dotimes (i 3)
      (dotimes (j 3)
	[drop-cell self (clone =brick=) (+ 10 i) (+ 10 j)]))
    (let ((puckman (clone =puckman=)))
      [drop-cell self puckman 3 5]
      [drop-cell self (clone =portal=
			     :address '(=chevronman-world=)
			     :text '((("To part 2"))))
		 18 30 :loadout t])))

(define-method begin-ambient-loop puckman-world ()
  (play-music "sparqq" :loop t))

;;; Tutorial level 2: Chevrons

(defvar *chevron-enemies* 0)

(defcell enemy
  (tile :initform "enemy")
  (categories :initform '(:actor :target :obstacle :enemy :puck))
  (direction :initform :east)
  (stepping :initform t))

(define-method kick enemy (direction)
  (setf <direction> direction))

(define-method run enemy ()
  (clon:with-field-values (row column) self
    (when [obstacle-in-direction-p *world* row column <direction>]
      (setf <direction> (random-direction)))
      [move self <direction>])
  [expend-action-points self 25])

(define-method loadout enemy ()
  (incf *chevron-enemies*))

(define-method die enemy ()
  (decf *chevron-enemies*)
  [parent>>die self])

(defparameter *chevron-welcome-text* '((("Now you'll learn about chevrons."))))

(defparameter *chevron-instruction-text* '((("Use ALT (or OPTION on the Mac)"))
					   (("with an arrow-key to drop a chevron."))
					   (("The chevron directs enemies toward the"))
					   (("direction it points in. See if you can"))
					   (("defeat the enemy ") (nil :image "enemy") (" by directing it"))
					   (("into the Black hole, i.e. ") (nil :image "hole") (" ."))))

(defparameter *chevron-success-text* '((("Good job. You've mastered the use")) (("of the chevron."))))

(defparameter *chevron-bye-text* '((("You can exit through the")) (("portal to the northeast."))))

(defcell chevronman 
  (tile :initform "npc")
  (state :initform 0)
  (categories :initform '(:obstacle :actor :npc))
  (timeout :initform 10))
      
(define-method emote chevronman (text &optional (timeout 3.0))
  (let ((balloon (clone =balloon= :text text :timeout timeout)))
    [play-sample self "talk"]
    [drop self balloon]))

(define-method run chevronman ()
  (clon:with-fields (state timeout) self
    [expend-default-action-points self]
    (labels ((act ()
	       (ecase state 
		 (0 [emote self *chevron-welcome-text* 1.0]
		    (incf state)
		    (setf timeout 20))
		 (1 [emote self *chevron-instruction-text* 4.0]
		    (setf timeout 300))
		 (2 [emote self *chevron-success-text*]
		    (incf state)
		    (setf timeout 50))
		 (3 [emote self *chevron-bye-text*]
		    (setf timeout 200)))))
      (if (and (not (= 3 state))
	       (zerop *chevron-enemies*))
	  (progn (setf state 2) (act))
	  (if (null timeout)
	      (act)
	      (when (minusp (decf timeout))
		(setf timeout nil)
		(act)))))))

(define-prototype chevronman-world (:parent xe2:=world=)
  (height :initform *xong-level-height*)
  (width :initform *xong-level-width*)
  (edge-condition :initform :block)
  (ambient-light :initform :total))

(define-method generate chevronman-world (&rest args)
  [create-default-grid self]
  (setf <level> 0 *chevron-enemies* 0)
  (clon:with-fields (height width grid player) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))
    (let ((chevron-1 (clone =chevron=))
	  (chevron-2 (clone =chevron=))
	  (chevronman (clone =chevronman=))
	  (enemy (clone =enemy=)))
      [orient chevron-1 :east]
      [orient chevron-2 :west]
      [drop-cell self chevron-1 10 10]
      [drop-cell self chevron-2 10 20]
      (dotimes (n 9)
	[drop-cell self (clone =hole= :nospew t) 20 (+ n 11)])
      (dotimes (n 9)
	[drop-cell self (clone =hole= :nospew t) 5 (+ n 11)])
      (dotimes (n 5)
	(let ((diamond (clone =diamond=)))
	  [drop-cell self diamond (+ 20 (random 5)) (+ 30 (random 10))]
	  (when (= n 0)
	    [drop diamond (clone =balloon= :text '((("Extra chevrons."))))])))
      [drop-cell self enemy 10 15]
      [loadout enemy]
      [drop-cell self chevronman 20 5]
      [drop-cell self (clone =portal=
			     :address '(=multiman-world=)
			     :text '((("To part 3"))))
		 6 30 :loadout t])))

(define-method begin-ambient-loop chevronman-world ()
  (play-music "phong" :loop t))

;;; MULTIMAN: use multiple skills

(defparameter *multi-welcome-text* '((("Now combine your skills")) (("to defeat multiple enemies."))))

(defparameter *multi-instruction-text* '((("Use multiple chevrons to direct"))
					 (("enemies around the obstacles and"))
					 (("into the black holes. Use the puck"))
					 (("to break any bricks blocking your path."))))

(defparameter *multi-success-text* '((("Great! You've combined"))
				     (("your skills and learned"))
				     (("the basics of XONG."))))

(defparameter *multi-bye-text* '((("Exit through the portal to")) (("the northeast to begin"))
				 (("playing Level 1 of XONG."))))

(defcell multiman 
  (tile :initform "npc")
  (state :initform 0)
  (categories :initform '(:obstacle :actor :npc))
  (timeout :initform 10))
      
(define-method emote multiman (text &optional (timeout 3.0))
  (let ((balloon (clone =balloon= :text text :timeout timeout)))
    [play-sample self "talk"]
    [drop self balloon]))

(define-method run multiman ()
  (clon:with-fields (state timeout) self
    [expend-default-action-points self]
    (labels ((act ()
	       (ecase state 
		 (0 [emote self *multi-welcome-text* 1.0]
		    (incf state)
		    (setf timeout 20))
		 (1 [emote self *multi-instruction-text* 4.0]
		    (setf timeout 300))
		 (2 [emote self *multi-success-text*]
		    (incf state)
		    (setf timeout 35))
		 (3 [emote self *multi-bye-text*]
		    (setf timeout 200)))))
      (if (and (not (= 3 state))
	       (zerop *chevron-enemies*))
	  (progn (setf state 2) (act))
	  (if (null timeout)
	      (act)
	      (when (minusp (decf timeout))
		(setf timeout nil)
		(act)))))))

(define-prototype multiman-world (:parent xe2:=world=)
  (height :initform *xong-level-height*)
  (width :initform *xong-level-width*)
  (edge-condition :initform :block)
  (ambient-light :initform :total))

(define-method generate multiman-world (&rest args)
  [create-default-grid self]
  (setf <level> 0 *chevron-enemies* 0)
  (labels ((drop-bulkhead (r c)
	     (prog1 nil
	       [drop-cell self (clone =bulkhead=) r c]))
	   (drop-brick (r c)
	     (prog1 nil
	       [drop-cell self (clone =brick=) r c])))
    (clon:with-fields (height width grid player) self
      (dotimes (i height)
	(dotimes (j width)
	  [drop-cell self (clone =floor=) i j]))
      (let ((chevron-1 (clone =chevron=))
	    (chevron-2 (clone =chevron=))
	    (chevron-3 (clone =chevron=))
	    (chevron-4 (clone =chevron=))
	    (multiman (clone =multiman=))
	    (enemy-1 (clone =enemy=))
	    (enemy-2 (clone =enemy=)))
	[orient chevron-1 :east]
	[orient chevron-2 :west]
	[drop-cell self chevron-1 15 10]
	[drop-cell self chevron-2 15 20]
	[orient chevron-3 :east]
	[orient chevron-4 :west]
	[drop-cell self chevron-3 8 10]
	[drop-cell self chevron-4 8 20]
	(trace-row #'drop-bulkhead 5 8 22)
	(trace-row #'drop-bulkhead 17 8 22)
	(dotimes (n 8)
	  [drop-cell self (clone =hole= :nospew t) 2 (+ n 11)])
	(dotimes (n 8)
	  [drop-cell self (clone =hole= :nospew t) 20 (+ n 11)])
	(trace-rectangle #'drop-brick 1 10 3 10)
	(trace-rectangle #'drop-brick 19 10 3 10)
	(dotimes (n 5)
	  (let ((diamond (clone =diamond=)))
	    [drop-cell self diamond (+ 20 (random 5)) (+ 30 (random 10))]
	    (when (= n 0)
	      [drop diamond (clone =balloon= :text '((("Extra chevrons."))))])))
	(dotimes (n 2)
	  (let ((puckup (clone =puckup=)))
	    [drop-cell self puckup (+ 20 (random 5)) (+ 3 (random 5))]
	    (when (= n 0)
	      [drop puckup (clone =balloon= :text '((("Extra pucks."))))])))
	[drop-cell self enemy-1 15 18]
	[drop-cell self enemy-2 8 15]
	[loadout enemy-1]
	[loadout enemy-2]
	[drop-cell self multiman 15 25]
	[drop-cell self (clone =portal=
			       :address (generate-level-address 1)
			       :text '((("Exit to Level 1"))))
		   6 30 :loadout t]))))
  
(define-method begin-ambient-loop multiman-world ()
    (play-music "neon" :loop t))

		       
;;; Splash screen
  
(defvar *pager* nil)

(define-prototype splash (:parent =widget=))

(define-method render splash ()
  (xe2:draw-resource-image "splash" 0 0 
			   :destination <image>))

(defvar *space-bar-function*)

(define-method dismiss splash ()
  [select *pager* :game]
  (when (functionp *space-bar-function*)
    (funcall *space-bar-function*))
  ;; TODO ugh this is a hack!
  (xe2:show-widgets))

(define-prototype splash-prompt (:parent =prompt=)
  (default-keybindings :initform '(("SPACE" nil "dismiss ."))))

;;; Player status

(defvar *status* nil)

(define-prototype status (:parent xe2:=formatter=)
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
	[print-stat self :chevrons :warn-below 3 :show-max t]
	[print-stat-bar self :chevrons :color ".yellow"]
	[space self]
	[print self (format nil "   LEVEL:~S" (field-value :level *world*))]
	[print self (format nil "   ENEMIES:~S" *enemies*)]
	[print self "     HOLDING:"]
	(if (field-value :puck char)
	    [print self nil :image (field-value :tile
						(field-value :puck char))]
	    [print self nil :image "hole-closed"])
	[print self (format nil "   SCORE:~S" [stat-value char :score])]
	[newline self])))

;;; Main program. 

(defparameter *xong-window-width* 800)
(defparameter *xong-window-height* 600)

(defvar *viewport*)

(defun xong ()
  (xe2:message "Initializing Xong...")
  (setf xe2:*window-title* "Xong")
  (setf clon:*send-parent-depth* 2) 
  (xe2:set-screen-height *xong-window-height*)
  (xe2:set-screen-width *xong-window-width*)
  ;; go!
  (let* ((prompt (clone =xong-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (player (clone =player=))
	 (splash (clone =splash=))
	 (help (clone =formatter=))
	 (quickhelp (clone =formatter=))
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
    [resize *status* :height 20 :width *xong-window-width*]
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
	       (xe2:enable-timer)
	       (xe2:set-frame-rate 30)
	       (xe2:set-timer-interval 1)
	       (xe2:enable-held-keys 1 3)
	       ;;
	       [set-player universe player]
	       [play universe
	       	     :address '(=menu-world=)
	       	     :prompt prompt
	       	     :narrator terminal
	       	     :viewport viewport]
	       [loadout player]
	       ;;
	       [set-character *status* player]
	       ;;
	       [set-tile-size viewport 16]
	       (setf (field-value :use-overlays viewport) t)
	       [resize viewport :height 470 :width *xong-window-width*]
	       [move viewport :x 0 :y 0]
	       [set-origin viewport :x 0 :y 0 
			   :height (truncate (/ (- *xong-window-height* 130) 16))
			   :width (truncate (/ *xong-window-width* 16))]
	       [adjust viewport]))
      (setf *space-bar-function* #'spacebar))
    ;;
    [resize help :height 540 :width 800] 
    [move help :x 0 :y 0]
    (let ((text	(find-resource-object "help-message")))
      (dolist (line text)
	(dolist (string line)
	  (funcall #'send nil :print-formatted-string help string))
	[newline help]))
    ;;
    [resize quickhelp :height 85 :width 250] 
    [move quickhelp :y (- *xong-window-height* 130) :x (- *xong-window-width* 250)]
    (let ((text	(find-resource-object "quickhelp-message")))
      (dolist (line text)
	(dolist (string line)
	  (funcall #'send nil :print-formatted-string quickhelp string))
	[newline quickhelp]))
    ;;
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
    ;; HACK
    (labels ((light-hack (sr sc r c &optional (color ".white"))
    	       (labels ((hack-overlay (image)
    			  (multiple-value-bind (sx sy)
    			      [get-viewport-coordinates *viewport* sr sc]
    			    (multiple-value-bind (x y)
    				[get-viewport-coordinates *viewport* r c]
    			      (draw-line x y sx sy :destination image
    					 :color color)
			      (draw-circle x y 5 :destination image)))))
    		 [add-overlay *viewport* #'hack-overlay]))))
;;      (setf xe2::*lighting-hack-function* #'light-hack))
    ;; END HACK
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    (xe2:install-widgets splash-prompt splash)
    [add-page *pager* :game prompt stack viewport terminal *status* quickhelp]
    [add-page *pager* :help help]))

(xong)
