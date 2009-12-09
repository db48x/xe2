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

;;; Turn on timing after SDL init

(add-hook 'xe2:*initialization-hook*
	  #'(lambda ()
	      (xe2:enable-timer)
	      (xe2:set-frame-rate 30)
	      (xe2:set-timer-interval 1)
	      (xe2:enable-held-keys 1 3)))

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

(defcell earth 
  (tile :initform "floor")
  (categories :initform '(:actor :reflective))
  (clock :initform (random *earth-rain-clock*)))

(define-method run earth ()
  (let ((dist [distance-to-player self]))
    (setf <tile> (if (< dist *earth-light-radius*)
		     (prog1 (nth (truncate (/ dist 2)) *earth-tiles*)
		       (if (minusp <clock>)
			   (progn (percent-of-time 5
				    (multiple-value-bind (x y) [viewport-coordinates self]
				      [drop-sprite self (clone =raindrop=) x y]))
				  (setf <clock> *earth-rain-clock*))
			   (decf <clock>)))
		     "floor"))))
    
;;; The storm 

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
  (categories :initform '(:obstacle :opaque)))

;;; The stone wall

(defcell wall
  (tile :initform "wall")
  (categories :initform '(:obstacle :opaque)))

(defcell debris
  (tile :initfomr "debris"))

(defcell ruin-floor
  (tile :initform "ruin-floor"))

;;; The player

(defcell player 
  (tile :initform "player")
  (name :initform "Player")
  (hearing-range :initform 1000)
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (movement-cost :initform (make-stat :base 10))
  (stepping :initform t)
  (categories :initform '(:actor :player :obstacle)))

(define-method quit player ()
  (xe2:quit :shutdown))

(define-method run player ()
  ;; if you are in category :actor, this is called every turn
  nil)

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
  (categories :initform '(:obstacle :actor))
  (generated :initform nil))

(define-method run gravestone ()
  (when (and (< [distance-to-player self] 10)
	     [line-of-sight *world* <row> <column> 
			    [player-row *world*]
			    [player-column *world*]])
    (percent-of-time 40
      (when (not <generated>)
	(setf <generated> t)
	[drop self (clone =skeleton=) :loadout t]))))

(defcell dagger 
  (name :initform "dagger")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "dagger")
  (attack-power :initform (make-stat :base 5))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (stepping :initform t)
  (weight :initform 3000)
  (equip-for :initform '(:left-hand :right-hand)))

(defcell skeleton 
  (name :initform "Skeleton")
  (strength :initform (make-stat :base 15 :min 0 :max 50))
  (dexterity :initform (make-stat :base 15 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
  (equipment-slots :initform '(:left-hand))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (speed :initform (make-stat :base 5))
  (movement-cost :initform (make-stat :base 5))
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (hit-points :initform (make-stat :base 25 :min 0 :max 10))
  (tile :initform "skeleton"))

(define-method initialize skeleton ()
  [make-inventory self]
  [make-equipment self])

(define-method loadout skeleton ()
  (let ((dagger (clone =dagger=)))
    [equip self [add-item self dagger]]))

(define-method run skeleton ()
  (clon:with-field-values (row column) self
    (let* ((world *world*)
	   (direction [direction-to-player *world* row column]))
      (if [adjacent-to-player world row column]
	  [>>attack self direction]
	  (if [obstacle-in-direction-p world row column direction]
	      (let ((target [target-in-direction-p world row column direction]))
		(if (and target (not [in-category target :enemy]))
		    [>>attack self direction]
		    (progn (setf <direction> (random-direction))
			   [>>move self direction])))
	      (progn (when (< 7 (random 10))
		       (setf <direction> (random-direction)))
		     [>>move self direction]))))))

;; (define-method die skeleton ()
;;   (when (> 8 (random 10))
;;     [drop self (clone (random-stat-powerup))])
;;   [play-sample self "blaagh3"]
;;   [parent>>die self])
	   
;;; The forest

(defcell drop-point 
  (categories :initform '(:player-entry-point))
  (tile :initform nil))

(defparameter *forest-width* 49)
(defparameter *forest-height* 200)

(define-prototype forest (:parent xe2:=world=)
  (height :initform *forest-height*)
  (width :initform *forest-width*)
  (ambient-light :initform *earth-light-radius*)
  (edge-condition :initform :block))

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
		[drop-cell self (clone object) i j :no-collisions t]))))))))

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
	(trace-rectangle #'drop-floor (1+ row) (1+ column) (- height 2) (- width 2) :fill)))))


(define-method generate forest (&key (height *forest-height*)
				     (width *forest-width*))
  (setf <height> height)
  (setf <width> width)
  [create-default-grid self]
  [drop-earth self]
  [drop-cell self (clone =storm=) 0 0]
  (dotimes (i 100)
    (let ((firefly (clone =firefly=)))
      [add-sprite self firefly]
      [update-position firefly 
		       (random (* 16 *forest-width*))
		       (random (* 16 *forest-height*))]))
  [drop-trees self :graininess 0.3 :density 32]
  [drop-water self :graininess 0.2 :density 90 :cutoff 0.9]
  (dotimes (n 10)
    [drop-graves self (random *forest-height*) (random *forest-width*)
		 (+ 4 (random 4)) (+ 4 (random 4))])
  (dotimes (n 15)
    [drop-ruin self (random *forest-height*) (random *forest-width*) (+ 9 (random 8)) (+ 4 (random 8))])
  [drop-cell self (clone =drop-point=) 
	     (1+ (random 20)) 
	     (1+ (random 20))
	     :exclusive t :probe t])

(define-method begin-ambient-loop forest ()
  (play-sample "lutey")
  (play-music "nightbird" :loop t))

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
    ("KP7" (:control) "serve-ball :northwest .")
    ("KP8" (:control) "serve-ball :north .")
    ("KP9" (:control) "serve-ball :northeast .")
    ("KP4" (:control) "serve-ball :west .")
    ("KP6" (:control) "serve-ball :east .")
    ("KP1" (:control) "serve-ball :southwest .")
    ("KP2" (:control) "serve-ball :south .")
    ("KP3" (:control) "serve-ball :southeast .")))

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
	    ("Y" (:control) "serve-ball :northwest .")
	    ("K" (:control) "serve-ball :north .")
	    ("U" (:control) "serve-ball :northeast .")
	    ("H" (:control) "serve-ball :west .")
	    ("L" (:control) "serve-ball :east .")
	    ("B" (:control) "serve-ball :southwest .")
	    ("J" (:control) "serve-ball :south .")
	    ("N" (:control) "serve-ball :southeast .")
	    ;;
	    ("Q" (:control) "quit ."))))

(define-method install-keybindings room-prompt ()
  (dolist (k (append *numpad-keybindings* *qwerty-keybindings*))
    (apply #'bind-key-to-prompt-insertion self k))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *world* :timer])])

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
	 (viewport (clone =viewport=)))
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
	  :address '(=forest=)
	  :player player
	  :narrator narrator
	  :prompt prompt
	  :viewport viewport]
    [set-tile-size viewport 16]
    [resize viewport :height 470 :width *room-window-width*]
    [move viewport :x 0 :y 0]
    [set-origin viewport :x 0 :y 0 
		:height (truncate (/ (- *room-window-height* 130) 16))
		:width (truncate (/ *room-window-width* 16))]
    [adjust viewport] 
   ;;
    (xe2:install-widgets prompt viewport narrator)))

(init-forest)
