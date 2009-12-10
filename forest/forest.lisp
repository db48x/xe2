;;; forest.lisp --- forest exploration stories

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

;; See also http://dto.github.com/notebook/developers-guide.html

;;; Packaging

(defpackage :forest
  (:documentation "Forest xe2 game.")
  (:use :xe2 :common-lisp)
  (:export forest))

(in-package :forest)

(defvar *status* nil)

;;; Turn on timing after SDL init

(add-hook 'xe2:*initialization-hook*
	  #'(lambda ()
	      (xe2:enable-timer)
	      (xe2:set-frame-rate 30)
	      (xe2:set-timer-interval 1)
	      (xe2:enable-held-keys 1 3)))

;;; Forest addresses

(defun generate-forest-address (n)
  (list '=forest= 
	:sequence-number (xe2:genseq) 
	:height *forest-height*
	:width *forest-width*
	:fireflies 100
	:graveyards 8
	:ruins 10
	:tree-grain 0.8
	:tree-density 40
	:water-grain 0.6
	:water-density 90
	:water-cutoff 0.2))

;;; Text overlay balloons

(defcell balloon 
  (categories :initform '(:drawn :actor))
  text timeout following
  (stroke-color :initform ".white")
  (background-color :initform ".gray40"))

(define-method initialize balloon (&key text (stroke-color ".white") (background-color ".blue")
					(style :balloon) (timeout nil) following)
  (setf <text> text)
  (setf <stroke-color> stroke-color)
  (setf <following> following)
  (setf <background-color> background-color)
  (setf <style> style)
  (setf <timeout> (if (floatp timeout)
		      ;; specify in (roughly) seconds if floating
		      (truncate (* 15 timeout))
		      ;; leave as frames if integer
		      timeout)))
  
(define-method follow balloon (cell)
  (setf <following> cell))

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
  (when <following>
    (multiple-value-bind (r c) [grid-coordinates <following>]
      ;; follow emoter
      [move-to self r c]))
  (when (integerp <timeout>)
    (when (minusp (decf <timeout>))
      [die self])))

;;; Water

(defcell foam 
  (tile :initform "foam")
  (clock :initform 4)
  (categories :initform '(:actor :ephemeral)))

(define-method run foam ()
  (let ((dir (car (one-of '(:southwest :south :southeast)))))
    (when [category-in-direction-p *world* <row> <column> dir :water]
      [move self dir])
    (decf <clock>)
    (when (minusp <clock>)
      [die self])))

(defparameter *water-tiles* '("water-1"
			     "water-2"
			     "water-3"))

(defcell water 
  (tile :initform "floor")
  (categories :initform '(:actor :reflective :water :exclusive)))

(define-method run water ()
  (let ((dist [distance-to-player self]))
    (setf <tile> (if (< dist *earth-light-radius*)
		     (prog1 (nth (truncate (/ dist 6)) *water-tiles*)
		       (percent-of-time 1 [drop self (clone =foam=)]))
		     "floor"))))

;;; The storm is an invisible cell that sits in the corner and plays thunder

(defcell storm 
  (tile :initform nil)
  (categories :initform '(:actor))
  (clock :initform 10))

(define-method run storm ()
  [expend-default-action-points self]
  (decf <clock>)
  (if (minusp <clock>)
      (progn 
	(setf <clock> (random (+ 400 (random 200))))
	(message "THUNDER")
	[play-sample self (car (one-of '("thunder-med" "thunder-med" "thunder-big")))]
      (decf <clock>))))
  
;;; The tree

(defcell tree 
  (tile :initform "tree-1")
  (categories :initform '(:obstacle :opaque :nosnow)))

;;; The snow

(defparameter *snow-tiles* '("snow-1"
			     "snow-2"
			     "snow-3"
			     "snow-4"
			     "snow-5"))

(defparameter *snow-dark-tiles* '("snow-dark-1"
				  "snow-dark-2"
				  "snow-dark-3"
				  "snow-dark-4"
				  "snow-dark-5"))

(defcell snow 
  (amount :initform 0)
  (tile :initform "snow-1")
  (categories :initform '(:snow)))

(define-method collect snow (&optional (amount 1) dark)
  (setf <amount> (min (+ amount <amount>) 
		      (length *snow-tiles*)))
    (setf <tile> (nth <amount> (if dark *snow-dark-tiles* *snow-tiles*))))

(define-method update-tile snow (dark)
  (setf <tile> (nth <amount> (if dark *snow-dark-tiles* *snow-tiles*))))


;;; Reflects light 

(defparameter *earth-tiles* '("earth-1" 
			      "earth-2"
			      "earth-3"
			      "earth-4"
			      "earth-5"
			      "earth-6"
			      "floor"))

(defparameter *earth-light-radius* 14)

(defparameter *earth-rain-clock* 10)

(defparameter *snow-clock* 8)

(defcell earth 
  (tile :initform "floor")
  (categories :initform '(:actor :reflective))
  (snow-clock :initform *snow-clock*)
  (clock :initform (random *earth-rain-clock*)))

(define-method snow earth (dark)
  (let ((snow [category-at-p *world* <row> <column> :snow]))
    (when snow
      [update-tile snow dark])
    (if (minusp <snow-clock>)
	(progn (setf <snow-clock> *snow-clock*)
	       (if (null snow)
		   (percent-of-time 3
		     (setf snow (clone =snow=))
		     [drop self snow])
		   (percent-of-time 10 
		     [collect snow 1 dark])))
	(decf <snow-clock>))))
    
(define-method run earth ()
  [expend-action-points self 10]
  (let* ((dist [distance-to-player self])
	 (dark (not (< dist 8))))
    (when [is-snowing *world*]
      [snow self dark])
    (setf <tile> (if (< dist *earth-light-radius*)
		     (prog1 (nth (truncate (/ dist 2)) *earth-tiles*)
		       (if (minusp <clock>)
			   (progn (percent-of-time 5
				    (when [is-snowing *world*]
				      (multiple-value-bind (x y) [viewport-coordinates self]
					[drop-sprite self (clone =snowflake=) x y])))
				  (setf <clock> *earth-rain-clock*))
			   (decf <clock>)))
		     "floor"))))

;;; Icy tundra

(defparameter *tundra-tiles* '("tundra-1" 
			      "tundra-2"
			      "tundra-3"
			      "tundra-4"
			      "tundra-5"
			      "tundra-6"
			      "floor"))

(defparameter *tundra-light-radius* 14)


(defcell tundra 
  (tile :initform "floor")
  (categories :initform '(:actor :reflective)))

(define-method blow tundra (dark)
  (let ((snow [category-at-p *world* <row> <column> :snow]))
    (when snow
      [update-tile snow dark])
    (if (minusp <snow-clock>)
	(progn (setf <snow-clock> *snow-clock*)
	       (if (null snow)
		   (percent-of-time 3
		     (setf snow (clone =snow=))
		     [drop self snow])
		   (percent-of-time 10 
		     [collect snow 1 dark])))
	(decf <snow-clock>))))
    
(define-method run tundra ()
  (let* ((dist [distance-to-player self]))
    (setf <tile> (if (< dist *tundra-light-radius*)
		     (nth (truncate (/ dist 2)) *tundra-tiles*)
		     "floor"))))

(defcell mountain 
  (tile :initform "mountain")
  (categories :initform '(:obstacle :opaque)))
 
    
;;; The stone wall

(defcell wall
  (tile :initform "wall")
  (categories :initform '(:obstacle :opaque)))

(defcell debris
  (tile :initform "debris"))

(defcell ruin-floor
  (tile :initform "ruin-floor"))

;;; The player
	     
(defparameter *arrow-tiles* '(:north "arrow-north"
			      :south "arrow-south"
			      :east "arrow-east"
			      :west "arrow-west"))

(defcell arrow 
  (name :initform "arrow")
  (speed :initform (make-stat :base 25))
  (categories :initform '(:actor))
  (clock :initform 8)
  (direction :initform nil))

(define-method impel arrow (direction)
  (assert (member direction '(:north :south :east :west)))
  (setf <direction> direction))

(define-method run arrow ()
  [expend-default-action-points self]
  (when <direction>
    (setf <tile> (getf *arrow-tiles* <direction>))
    (let ((target [category-in-direction-p *world* <row> <column> <direction> :target]))
      (if target 
	  (progn [damage target 3]
		 [die self])
	  [move self <direction>])))
  (decf <clock>)
  (when (zerop <clock>)
    [die self])
  (when [obstacle-in-direction-p *world* <row> <column> <direction>]
    (setf <clock> 0)))

(defcell wooden-bow 
  (name :initform "Wooden bow")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "wooden-bow")
  (attack-power :initform (make-stat :base 5))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (weight :initform 3000)
  (equip-for :initform '(:left-hand)))

(define-method fire wooden-bow (direction)
  (if (plusp [stat-value <equipper> :arrows])
      (let ((arrow (clone =arrow=)))
	[stat-effect <equipper> :arrows -1]
	[drop <equipper> arrow]
	[impel arrow direction]
	[play-sample <equipper> "bow"])
      [say self "You are out of arrows!" :foreground ".red"]))

(defparameter *hunger-warn* 700)

(defparameter *hunger-warn-2* 850)

(defparameter *hunger-max* 1000)

(defcell player 
  (tile :initform "player")
  (name :initform "Player")
  (dead :initform nil)
  (hit-points :initform (make-stat :base 30 :min 0 :max 30))
  (rations :initform (make-stat :base 5 :min 0 :max 20))
  (hunger :initform (make-stat :base 0 :min 0 :max 1000))
  (hunger-damage-clock :initform 0)
  (hearing-range :initform 1000)
  (firing-with :initform :left-hand)
  (arrows :initform (make-stat :base 40 :min 0 :max 40))
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (strength :initform (make-stat :base 15 :min 0 :max 50))
  (defense :initform (make-stat :base 15 :min 0 :max 50))
  (dexterity :initform (make-stat :base 15 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (equipment-slots :initform '(:right-hand :left-hand))
  (max-items :initform (make-stat :base 20))
  (movement-cost :initform (make-stat :base 10))
  (stepping :initform t)
  (categories :initform '(:actor :player :obstacle :target)))

(define-method enter player ()
  (let ((gateway [category-at-p *world* <row> <column> :gateway]))
    (if (null gateway)
	[>>say :narrator "No gateway to enter."]
	[activate gateway])))

(define-method emote player (text &optional (timeout 20))
  (let ((balloon (clone =balloon= :text text :timeout timeout)))
    [play-sample self "talk"]
    [follow balloon self]
    [drop self balloon]))

(define-method quit player ()
  (xe2:quit :shutdown))

(define-method run player ()
  [stat-effect self :hunger 1]
  (let ((hunger [stat-value self :hunger]))
    (when (= *hunger-warn* hunger)
      [say self "You are getting hungry."])
    (when (= *hunger-warn-2* hunger)
      [say self "You are getting extremely hungry!"])
    (when (= *hunger-max* hunger)
      (if (minusp <hunger-damage-clock>)
	  (progn 
	    [say self "You are starving! You will die if you do not eat soon."]
	    (setf <hunger-damage-clock> 20)
	    [damage self 1])
	  (decf <hunger-damage-clock>))))
  (when (zerop [stat-value self :hit-points])
    [die self])
  (when (and *status* <inventory>) [update *status*]))

(define-method restart player ()
  (let ((player (clone =player=)))
    [destroy *universe*]
    [set-player *universe* player]
    [set-character *status* player]
    [play *universe*
	  :address (generate-forest-address 1)]
    [loadout player]))

(define-method damage player (points)
  [say self "You take ~A hit ~A of damage."
       points (if (= 1 points) "point" "points")]
  (percent-of-time 70 [play-sample self (car (one-of '("unh-1" "unh-2" "unh-3")))])
  [stat-effect self :hit-points (- points)])

(define-method die player ()
  (unless <dead>
    (setf <tile> "skull")
    [play-sample self "death"]
    [say self "You died. Press ESCAPE to try again."]
    (setf <dead> t)))

(define-method loadout player ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =wooden-bow=)]]
  [emote self '((("I'd better get moving.")) (("The monastery is to the south.")))])

;;; Raindrops

(defsprite raindrop 
  (image :initform "raindrop")
  (categories :initform '(:actor))
  (movement-distance :initform 1)
  (clock :initform 4))

(define-method run raindrop ()
  [expend-default-action-points self]
  (clon:with-fields (clock) self
    (if (plusp clock) 
	(progn 
	  (decf clock)
	  [move self :southeast])
	[die self])))

(defsprite snowflake
  (image :initform "snowflake")
  (categories :initform '(:actor))
  (movement-distance :initform 2)
  (clock :initform 8))

(define-method run snowflake ()
  (clon:with-fields (clock) self
    (if (plusp clock) 
	(progn 
	  (decf clock)
	  [move self :southeast])
	[die self])))

;;; Ambient Fireflies

(defsprite firefly 
  (image :initform "firefly-1")
  (categories :initform '(:actor))
  (movement-distance :initform 1)
  (clock :initform 0))

(define-method run firefly ()
  [expend-default-action-points self]
  (clon:with-fields (clock image) self
    (if (plusp clock) 
	(progn 
	  (setf image (car (one-of '("firefly-1" "firefly-2"))))
	  (decf clock))
	(progn
	  (setf image nil)
	  (when (< [distance-to-player self] 10)
	    (percent-of-time 3
	      (setf clock (+ 5 (random 5)))))))
    [move self (random-direction)]))

;;; Skeletons haunt the woods, coming from gravestones

(defcell gravestone 
  (tile :initform "gravestone")
  (contains-body :initform (percent-of-time 25 t))
  (categories :initform '(:obstacle :actor))
  (generated :initform nil))

(define-method run gravestone ()
  (when (and <contains-body>
	     (< [distance-to-player self] 10)
	     [line-of-sight *world* <row> <column> 
			    [player-row *world*]
			    [player-column *world*]])
    (percent-of-time 40
      (when (not <generated>)
	(setf <generated> t)
	(let ((skeleton (clone =skeleton=)))
	  [drop self skeleton]
	  [loadout skeleton])))))
	  
(defcell dagger 
  (name :initform "dagger")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "dagger")
  (attack-power :initform (make-stat :base 15))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (stepping :initform t)
  (weight :initform 3000)
  (equip-for :initform '(:left-hand :right-hand)))

(defcell skeleton 
  (name :initform "Skeleton")
  (strength :initform (make-stat :base 20 :min 0 :max 50))
  (dexterity :initform (make-stat :base 20 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
  (equipment-slots :initform '(:left-hand))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (speed :initform (make-stat :base 1))
  (movement-cost :initform (make-stat :base 5))
  (attacking-with :initform :left-hand)
  (equipment-slots :initform '(:left-hand :right-hand :belt :extension :feet))
  (max-weight :initform (make-stat :base 25))
  (max-items :initform (make-stat :base 20))
  (hit-points :initform (make-stat :base 5 :min 0 :max 5))
  (tile :initform "skeleton"))

(define-method loadout skeleton ()
  [make-inventory self]
  [make-equipment self]
  (let ((dagger (clone =dagger=)))
    [equip self [add-item self dagger]]))

(define-method run skeleton ()
  (clon:with-field-values (row column) self
    (let* ((world *world*)
	   (direction [direction-to-player *world* row column]))
      (when (< [distance-to-player self] 8)
	(percent-of-time 40 [play-sample self "grak"]))
      (if [adjacent-to-player world row column]
	  (progn [say self "The skeleton stabs at you with its dagger."]
		 [play-sample self "groar"]
		 [expend-action-points self 10]
		 (percent-of-time 80 [damage [get-player *world*] 2]))
	  (if [obstacle-in-direction-p world row column direction]
	      (let ((target [target-in-direction-p world row column direction]))
		(if (and target (not [in-category target :enemy]))
		    [move self (random-direction)]
		    (progn (setf <direction> (random-direction))
			   [>>move self direction])))
	      (progn (when (< 7 (random 10))
		       (setf <direction> (random-direction)))
		     [>>move self direction]))))))

(define-method die skeleton ()
  [play-sample self "dead"]
  [parent>>die self])
	   
;;; Bodies of other adventurers

(defcell arrows 
  (tile :initform "arrows")
  (count :initform (+ 2 (random 12))))

(define-method step arrows (stepper)
  (when [is-player stepper]
    [say self "You found ~S arrows." <count>]
    [stat-effect stepper :arrows <count>]
    [die self]))
     
(defcell herb 
  (tile :initform "herb")
  (categories :initform '(:equipment :item))
  (equip-for :initform '(:right-hand :left-hand)))

(define-method step herb (stepper)
  (when [is-player stepper]
    [say self "You found a healing herb."]
    [take stepper :direction :here :category :item]))

(defcell body 
  (tile :initform "body"))

(define-method step body (stepper)
  (when [is-player stepper]
    (percent-of-time 30
      [drop self (clone (car (one-of (list =herb= =arrows=))))])
    [die self]))

;;; The forest

(defcell drop-point 
  (categories :initform '(:player-entry-point))
  (tile :initform nil))

(defparameter *forest-width* 49)
(defparameter *forest-height* 100)

(define-prototype forest (:parent xe2:=world=)
  (height :initform *forest-height*)
  (width :initform *forest-width*)
  (snowing :initform t)
  (ambient-light :initform *earth-light-radius*)
  (description :initform "It is cold and snowing.")
  (edge-condition :initform :block))

(define-method is-snowing forest ()
  <snowing>)

(define-method drop-earth forest ()
  (dotimes (i <height>)
    (dotimes (j <width>)
      [drop-cell self (clone =earth=) i j])))

(define-method drop-trees forest (&optional &key (object =tree=)
					    distance 
					    (row 0) (column 0)
					    (graininess 0.3)
					    (density 100)
					    (cutoff 0))
  (clon:with-field-values (height width) self
    (let* ((h0 (or distance height))
	   (w0 (or distance width))
	   (r0 (- row (truncate (/ h0 2))))
	   (c0 (- column (truncate (/ w0 2))))
	   (plasma (xe2:render-plasma h0 w0 :graininess graininess))
	   (value nil))
      (dotimes (i h0)
	(dotimes (j w0)
	  (setf value (aref plasma i j))
	  (when (< cutoff value)
	    (when (or (null distance)
		      (< (distance (+ j r0) (+ c0 i) row column) distance))
	      (percent-of-time density
		[drop-cell self (clone object) i j :no-collisions t]))))))))

(define-method drop-graves forest (row column height width)
  (setf height (max 5 height))
  (setf width (max 5 width))
  (dotimes (i height)
    (dotimes (j width)
      (percent-of-time 40
	[drop-cell self (clone =gravestone=) 
		   (+ (* 2 i) row) 
		   (+ (* 2 j) column)]))))

(define-prototype passage-gateway (:parent =gateway=)
  (tile :initform "passage-gateway")
  (sequence-number :initform (genseq))
  (address :initform (list '=passage= :sequence-number (genseq))))

(define-method step passage-gateway (stepper)
  [say self "A pass through the mountains. Press RETURN to enter."])

(define-method drop-water forest (&optional &key (object =water=)
					    distance 
					    (row 0) (column 0)
					    (graininess 0.3)
					    (density 100)
					    (cutoff 0))
  (clon:with-field-values (height width) self
    (let* ((h0 (or distance height))
	   (w0 (or distance width))
	   (r0 (- row (truncate (/ h0 2))))
	   (c0 (- column (truncate (/ w0 2))))
	   (plasma (xe2:render-plasma h0 w0 :graininess graininess))
	   (value nil))
      (dotimes (i h0)
	(dotimes (j w0)
	  (setf value (aref plasma i j))
	  (when (< cutoff value)
	    (when (or (null distance)
		      (< (distance (+ j r0) (+ c0 i) row column) distance))
	      (percent-of-time density
		(let ((cell (clone object)))
		  [replace-cells-at self i j cell]
		  [set-location cell i j])))))))))

(define-method drop-ruin forest (row column height width)
  (let (rectangle openings)
    (labels ((collect-point (&rest args)
	       (prog1 nil (push args rectangle)))
	     (drop-wall (r c)
	       (unless (and (= r 0)
			    (= c 0))
		 (let ((wall (clone =wall=)))
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
	  (let ((debris (clone =debris=)))
	    [replace-cells-at self r c debris]
	    [set-location debris r c])))
      ;; drop floor, obliterating what's below
      (labels ((drop-floor (r c)
		 (prog1 nil
		   (percent-of-time 80
		     [replace-cells-at self r c (clone =ruin-floor=)]))))
	(trace-rectangle #'drop-floor (1+ row) (1+ column) (- height 2) (- width 2) :fill))
      (dotimes (n (random 3))
	(percent-of-time 70
	  [drop-cell self (clone =body=) (+ 1 row (random (- height 1))) (+ 1 column (random (- width 1)))])))))

(define-method generate forest (&key (height *forest-height*)
				     (width *forest-width*)
				     sequence-number 
				     (fireflies 100)
				     (graveyards 15)
				     (ruins 15)
				     (tree-grain 0.3)
				     (tree-density 30)
				     (water-grain 0.9)
				     (water-density 90)
				     (water-cutoff 0.2))
  (setf <height> height)
  (setf <width> width)
  (setf <sequence-number> sequence-number)
  [create-default-grid self]
  [drop-earth self]
  [drop-cell self (clone =storm=) 0 0]
  (dotimes (i fireflies)
    (let ((firefly (clone =firefly=)))
      [add-sprite self firefly]
      [update-position firefly 
		       (random (* 16 *forest-width*))
		       (random (* 16 *forest-height*))]))
  [drop-trees self :graininess tree-grain :density tree-density]
  [drop-water self :graininess water-grain :density water-density :cutoff water-cutoff]
  (dotimes (n graveyards)
    [drop-graves self (+ 20 (random (- *forest-height* 20))) (random *forest-width*)
		 (+ 4 (random 3)) (+ 4 (random 2))])
  (dotimes (n ruins)
    [drop-ruin self (random *forest-height*) (random *forest-width*) (+ 9 (random 8)) (+ 4 (random 8))])
  (let ((row (1+ (random 20)) )
	(column (1+ (random 20))))
    [drop-cell self (clone =drop-point=) row column
	       :exclusive t :probe t]
    [drop-cell self (clone =herb=) (+ row (random 20)) (+ column (random 20))])
  (let* ((passage (clone =passage-gateway=))
	 (row (+ (- height 10) (random 10))) ;; 20 FIXME
	 (column (random 10)))
    [replace-cells-at *world* row column passage]
    [set-location passage row column]))
    
(define-method begin-ambient-loop forest ()
  (play-sample "lutey")
  (play-music "nightbird" :loop t))

;;; Mountain passage world

(defparameter *passage-width* 49)
(defparameter *passage-height* 100)

(define-prototype passage (:parent xe2:=world=)
  (height :initform *passage-height*)
  (width :initform *passage-width*)
  (ambient-light :initform *earth-light-radius*)
  (description :initform "The air is oddly still in this pass between the crags.")
  (edge-condition :initform :block))

(define-method drop-tundra passage ()
  (dotimes (i <height>)
    (dotimes (j <width>)
      [drop-cell self (clone =tundra=) i j])))

(define-method drop-mountains passage ()
  (let ((offset 10))
  (dotimes (i <height>)
    (setf offset (max 0 (incf offset (if (= 0 (random 2))
					 1 -1))))
    (labels ((drop-mountain (r c)
	       (prog1 nil
		 [drop-cell *world* (clone =mountain=) r c])))
      (trace-row #'drop-mountain i 0 (+ offset (random 4)))
      (trace-row #'drop-mountain i (+ offset (random 4) 20) <width>)))))

(define-method drop-trees passage (&optional &key (object =tree=)
					    distance 
					    (row 0) (column 0)
					    (graininess 0.3)
					    (density 100)
					    (cutoff 0))
  (clon:with-field-values (height width) self
    (let* ((h0 (or distance height))
	   (w0 (or distance width))
	   (r0 (- row (truncate (/ h0 2))))
	   (c0 (- column (truncate (/ w0 2))))
	   (plasma (xe2:render-plasma h0 w0 :graininess graininess))
	   (value nil))
      (dotimes (i h0)
	(dotimes (j w0)
	  (setf value (aref plasma i j))
	  (when (< cutoff value)
	    (when (or (null distance)
		      (< (distance (+ j r0) (+ c0 i) row column) distance))
	      (percent-of-time density
		[drop-cell self (clone object) i j :no-collisions t]))))))))

(define-method begin-ambient-loop passage ()
  (play-music "passageway" :loop t)
  (play-sample "thunder-big"))

(define-method generate passage (&key (height *forest-height*)
				      (width *forest-width*)
				      sequence-number)
  (setf <height> height)
  (setf <width> width)
  (setf <sequence-number> sequence-number)
  [create-default-grid self]
  [drop-tundra self]
  [drop-mountains self]
    (let ((row (1+ (random 10)) )
	  (column (+ 15 (random 6))))
      [drop-cell self (clone =drop-point=) row column
		 :exclusive t :probe t]))

;;; Controlling the game

(define-prototype room-prompt (:parent xe2:=prompt=))

(defparameter *numpad-keybindings* 
  '(("KP7" nil "move :northwest .")
    ("KP8" nil "move :north .")
    ("KP9" nil "move :northeast .")
    ("KP4" nil "move :west .")
    ("KP6" nil "move :east .")
    ("KP1" nil "move :southwest .")
    ("KP2" nil "move :south .")
    ("KP3" nil "move :southeast .")
    ;;
    ("KP8" (:control) "fire :north .")
    ("KP4" (:control) "fire :west .")
    ("KP6" (:control) "fire :east .")
    ("KP2" (:control) "fire :south .")))

(defparameter *qwerty-keybindings*
  (append *numpad-keybindings*
	  '(("Y" nil "move :northwest .")
	    ("K" nil "move :north .")
	    ("U" nil "move :northeast .")
	    ("H" nil "move :west .")
	    ("L" nil "move :east .")
	    ("B" nil "move :southwest .")
	    ("J" nil "move :south .")
	    ("N" nil "move :southeast .")
	    ;;
	    ("K" (:control) "fire :north .")
	    ("H" (:control) "fire :west .")
	    ("L" (:control) "fire :east .")
	    ("J" (:control) "fire :south .")
	    ;;
	    ("ESCAPE" nil "restart .")
	    ("RETURN" nil "enter .")
	    ;;
	    ("Q" (:control) "quit ."))))

(define-method install-keybindings room-prompt ()
  (dolist (k (append *numpad-keybindings* *qwerty-keybindings*))
    (apply #'bind-key-to-prompt-insertion self k))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *world* :timer])])

;;; A character status widget.

(define-prototype status (:parent xe2:=formatter=)
  (character :documentation "The character cell."))

(define-method set-character status (character)
  (setf <character> character))

(define-method print-stat status (stat-name &key warn-below)
  (let* ((stat (field-value stat-name <character>))
	 (value [stat-value <character> stat-name]))
    (destructuring-bind (&key min max base delta unit) stat
      (let ((color (if (and (numberp warn-below)
			    (< value warn-below))
		       ".red"
		       ".gray20")))
	[print self (symbol-name stat-name)
	       :foreground ".white"]
	[print self " "]
	[print self (format nil "~S" value) 
	       :foreground ".yellow"
	       :background color]
	[print self " "]))))

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

(define-method print-equipment-slot status (slot-name)
  [print self (symbol-name slot-name)]
  [print self ": "]
  (let* ((item [equipment-slot <character> slot-name]))
    (if item
	(clon:with-field-values (name tile) item
	  [print self nil :image tile]
	  [print self " "]
	  [print self name]
	  [print self "  "])
	[print self "EMPTY  "])))

(define-method print-inventory-slot status (slot-number)
  [print self (format nil "[~D]: " slot-number)]
  (let ((item [item-at <character> slot-number]))
    (if item
	(clon:with-field-values (name tile) item
				[print self nil :image tile]
				[print self " "]
				[print self (get-some-object-name item)]
				[print self "  "])
	[print self "EMPTY  "])))

(define-method update status ()
  [delete-all-lines self]
  (let ((char <character>))
    [print self "  Statistics:  "]
    [print-stat self :hit-points :warn-below 12]
    [print-stat-bar self :hit-points :color ".red" :background-color ".gray30"]
    [print self " "]
    [print-stat self :strength :warn-below 10]
    [print self " "]
    [print-stat self :defense :warn-below 10]
    [print self " "]
    [print-stat self :speed :warn-below 2]
    [println self " "]
    [print self "  Equipment:  "]
    [print-equipment-slot self :right-hand]
    [print-equipment-slot self :left-hand]
    [print self (format nil "  ARROWS: ~S " [stat-value char :arrows])]
    [println self nil :image "arrows"]
    
    [newline self]
    [print self "  Inventory:  "]
    [print-inventory-slot self 0]
    [print-inventory-slot self 1]
    [newline self]))

;;; Main program. 

(defparameter *room-window-width* 800)
(defparameter *room-window-height* 600)

(defun init-forest ()
  (xe2:message "Initializing Forest...")
  (clon:initialize)
  (xe2:set-screen-height *room-window-height*)
  (xe2:set-screen-width *room-window-width*)
  (let* ((prompt (clone =room-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (player (clone =player=))
	 (status (clone =status=))
	 (viewport (clone =viewport=)))
    ;;
    (setf *status* status)
    [resize status :height 60 :width 800]
    [move status :x 5 :y 0]
    [set-character status player]
    ;;
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    ;;
    [resize narrator :height 80 :width *room-window-width*]
    [move narrator :x 0 :y (- *room-window-height* 80)]
    [set-verbosity narrator 0]
    ;;
    [play universe
	  :address (generate-forest-address 1)
	  :player player
	  :narrator narrator
	  :prompt prompt
	  :viewport viewport]
    [set-tile-size viewport 16]
    [resize viewport :height 470 :width *room-window-width*]
    [move viewport :x 0 :y 60]
    [set-origin viewport :x 0 :y 0 
		:height (truncate (/ (- *room-window-height* 200) 16))
		:width (truncate (/ *room-window-width* 16))]
    [adjust viewport] 
    [loadout player]
   ;;
    (xe2:install-widgets prompt viewport narrator status)))

(init-forest)
