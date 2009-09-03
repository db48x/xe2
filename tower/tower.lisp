;;; tower.lisp --- tower escape roguelike

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

;; Tower Escape is a computer game that attempts to simulate the
;; strategic aspects of being caught in a disaster similar to the
;; September 11th attack on the World Trade Center. (The skyscraper is
;; unnamed in the game, and no specific mention is made of the real
;; attack.)

;; An explosion has occured in the building just below your
;; floor. Smoke begins pouring from the stairwells. Your job as the
;; player is to make your way through the fiery, disintegrating
;; structure, clear debris, find unblocked stairwells, and descend five
;; floors to the undamaged part of the building within ten minutes. On
;; each floor, you must find the exit, and guide other people by means of
;; shouting or holding hands. One can find and use such objects as rags
;; or clothing for covering the mouth or eyes, steel rods for bashing
;; doors open, and bottled water.

;; The graphics are made of large pixelated tiles, similar to the
;; graphics of an old Atari 8-bit computer game.

;; There is no music in Tower Escape. Instead, sound is used to
;; communicate important features of the situation: heartbeat,
;; respiration, and so on.

;; Time pressure is real, as this roguelike is real-time. The fire and
;; smoke on all five floors are simulated simutaneously, and parts of the
;; floor and ceiling can collapse dynamically. Smoke can move upward
;; between floors through any collapsed floor panels, and burning
;; material can fall downward. Windows remove smoke from the floor, but
;; the amount of smoke increases constantly. There is no means of
;; recovering hit points when injured. Water can improve vision (by
;; splashing it in the eyes) and breathing (by drinking it). Light smoke
;; is not opaque, but normal smoke and fire are totally opaque. Fire
;; emits smoke, and fire also spreads based on the combustability of
;; nearby material (including material above or below).

;; No one dies in the game, you just "lose", and the building is not
;; shown to collapse. You get points for rescuing more people, but they
;; don't "die" if you fail. Their fate is left undetermined.

;; Close doors to prevent fire from spreading. Watch the sparks in the
;; unlit parts of the map that show where a shout (or other sound) is
;; coming from. Fire weakens the floor above it. Weakened floor is
;; visibly different.

;; Although Tower Escape is obviously inspired by September 11th, the
;; point is disaster simulation, not commentary. I have read a lot about
;; the attacks, especially the various U.S. Government reports and TV
;; documentaries analyzing the details of what happened in the air, on
;; the ground, and inside the buildings. This aspect intrigued me,
;; especially when I thought about what I might do in such an
;; attack. Since then I've had a morbid interest in disasters and
;; disaster-preparedness, and Tower Escape is meant to be a
;; thought-provoking and challenging game of survival.

;;; Packaging

(defpackage :tower
  (:documentation "-fi roguelike for Common Lisp.")
  (:use :rlx :common-lisp)
  (:export tower))

(in-package :tower)

;;; The rusty wrench

(define-prototype wrench (:parent rlx:=cell=)
  (name :initform "Rusty wrench")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "bar")
  (attack-power :initform (make-stat :base 4)) ;; points of raw damage
  (attack-cost :initform (make-stat :base 5))
  (accuracy :initform (make-stat :base 80 :unit :percent))
  (weight :initform 10000) ;; grams
  (equip-for :initform '(:left-hand :right-hand)))

(define-method step wrench (stepper)
  (when [is-player stepper]
    [equip stepper [add-item stepper (clone =wrench=)]]
    [remove-from-world self]))

;;; Our hero, the player

(defcell player 
  (tile :initform "player")
  (name :initform "Player")
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (strength :initform (make-stat :base 13))
  (dexterity :initform (make-stat :base 13))
  (defense :initform (make-stat :base 15))
  (equipment-slots :initform '(:left-hand :right-hand))
  (hearing-range :initform 15)
  (hit-points :initform (make-stat :base 30 :min 0 :max 30))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (oxygen :initform (make-stat :base 100 :min 0 :max 100))
  (stepping :initform t)
  (attacking-with :initform :right-hand)
  (light-radius :initform 3)
  (categories :initform '(:actor :player :target :container :light-source)))

(defparameter *pain-sounds* '("unh1" "unh2" "unh3" "oghh"))

(define-method damage player (points)
  [play-sample self (nth (random (length *pain-sounds*)) *pain-sounds*)]
  [parent>>damage self points])

(define-method enter player ()
  (let ((gateway [category-at-p *active-world* <row> <column> :gateway]))
    (if (null gateway)
	[>>say :narrator "No gateway to enter."]
	[activate gateway])))

(define-method loadout player ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =wrench=)]])

(define-method wait player ()
  [say self "Skipped one turn."]
  (when (not [in-category self :proxied])
    [stat-effect self :oxygen -1])
  [expend-action-points self <action-points>])

(define-method quit player ()
  (rlx:quit :shutdown))

(defparameter *player-oxygen-warning-level* 20)
(defparameter *player-low-hp-warning-level* 10)

(define-method phase-hook player ()
  ;; possibly breathe
  (when (and [in-category *active-world* :airless]
	     (null <proxy>)
	     (has-field :oxygen self))
    [stat-effect self :oxygen -1]
    [say self "You expend one unit of oxygen."])) 

(define-method run player ()
  ;; check stats
  (when (< [stat-value self :oxygen] *player-oxygen-warning-level*)
    [>>println :narrator "WARNING: OXYGEN SUPPLY CRITICAL!" :foreground ".yellow" :background ".red"]
    [play-sample self (if (= 0 (random 2))
			  "breath1" "breath2")])
  (when (< [stat-value self :hit-points] *player-low-hp-warning-level*)
    [>>println :narrator "WARNING: LOW HIT POINTS!" :foreground ".yellow" :background ".red"])
  (cond ((<= [stat-value self :oxygen] 0)
	 [>>say :narrator "Your oxygen runs out, suffocating you."]
	 [die self])
	((<= [stat-value self :speed] 0)
	 [>>say :narrator "You are paralyzed. You suffocate and die."]
	 [die self]))
  [update *status*])
  
(define-method die player ()
  [play-sample self "death"]
  (let ((skull (clone =skull= self)))
    [drop-cell *active-world* skull <row> <column> :loadout t :no-collisions nil]
    [set-player *active-world* skull]
    [parent>>die self]))

(define-method attack player (target)
  (rlx:play-sample "knock")
  [parent>>attack self target]
  [stat-effect self :oxygen -2])

(define-method show-location player ()
  (labels ((do-circle (image)
	     (multiple-value-bind (x y) 
		 [screen-coordinates self]
	       (draw-circle x y 30 :destination image)
	       (draw-circle x y 25 :destination image))))
    [>>add-overlay :viewport #'do-circle]))

;;; Fire 

;;; Floors

(defparameter *floor-tiles* '("floor-hole" "floor-breaking2" "floor-breaking" "floor"))

(defcell floor 
  (tile :initform "floor")
  (hit-points :initform (make-stat :base 3 :min 0 :max 3)))

(define-method update-tile floor ()
  (setf <tile> (nth (1- [stat-value self :hit-points]) *floor-tiles*)))

(define-method run floor ()
  [update-tile self])

;;; Walls

(defparameter *wall-tiles* '("wall-breaking" "wall"))

(defcell wall 
  (tile :initform "wall")
  (categories :initform '(:actor :obstacle))
  (hit-points :initform (make-stat :base 2 :min 0 :max 2)))
  
(define-method update-tile wall ()
  (setf <tile> (nth (1- [stat-value self :hit-points]) *wall-tiles*)))
  
(define-method run wall ()
  [update-tile self])

;;; Doors

;;; Debris

;;; Smoke

;;; Stairwells

;;; Blankets

;;; Water 

;;; The Tower

(defparameter *tower-size* 30)

(define-prototype tower (:parent rlx:=world=)
  (ambient-light :initform :total)
  (height :initform *tower-size*)
  (width :initform *tower-size*)
  (scale :initform '(1 m))
  (edge-condition :initform :block))

(define-method drop-floor tower ()
  (clon:with-field-values (height width) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))))

(define-method drop-wall tower (r0 c0 r1 c1)
  (trace-line #'(lambda (x y)
		       [drop-cell self (clone =wall=) y x])
		   c0 r0 c1 r1))

(define-method generate tower (&key (floor 5)
				    (height *tower-size*)
				    (width *tower-size*))
  [create-default-grid self]
  [drop-floor self]
  (dotimes (i 20)
    (let ((column (random *tower-size*))
	  (row (random *tower-size*))
	  (len (random (truncate (/ *tower-size* 2)))))
      [drop-wall self row column (+ row len) column]
      [drop-wall self column row row (+ column len)])))

;;; The people you are trying to save

;;; Controlling the game

(define-prototype tower-prompt (:parent rlx:=prompt=))

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
    ("KP7" (:alt) "attack :northwest .")
    ("KP8" (:alt) "attack :north .")
    ("KP9" (:alt) "attack :northeast .")
    ("KP4" (:alt) "attack :west .")
    ("KP6" (:alt) "attack :east .")
    ("KP1" (:alt) "attack :southwest .")
    ("KP2" (:alt) "attack :south .")
    ("KP3" (:alt) "attack :southeast .")
    ;;
    ("KP7" (:meta) "attack :northwest .")
    ("KP8" (:meta) "attack :north .")
    ("KP9" (:meta) "attack :northeast .")
    ("KP4" (:meta) "attack :west .")
    ("KP6" (:meta) "attack :east .")
    ("KP1" (:meta) "attack :southwest .")
    ("KP2" (:meta) "attack :south .")
    ("KP3" (:meta) "attack :southeast .")))

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
	    ("W" nil "wait .")
	    ("SPACE" nil "wait .")
	    ("PERIOD" (:control) "restart .")
	    ("KP-ENTER" nil "enter .")
	    ("RETURN" nil "enter .")
	    ("ESCAPE" (:control) "show-location .")
	    ("Q" (:control) "quit ."))))
  
(defparameter *alternate-qwerty-keybindings*
  (append *numpad-keybindings*
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
	    ("S" nil "wait .")
	    ("ESCAPE" (:control) "show-location .")
	    ("SPACE" nil "wait .")
	    ("PERIOD" (:control) "restart .")
	    ("P" (:control) "quit ."))))
  
;; g c r
;;  \|/
;; h-.-n
;;  /|\ 
;; m w v

(defparameter *dvorak-keybindings*
  (append *numpad-keybindings*
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
	    ("S" nil "wait .")
	    ("SPACE" nil "wait .")
	    ("KP-ENTER" nil "enter .")
	    ("RETURN" nil "enter .")
	    ("ESCAPE" (:control) "show-location .")
	    ("PERIOD" (:control) "restart .")
	    ("Q" (:control) "quit ."))))

(define-method install-keybindings tower-prompt ()
  (let ((keys (ecase rlx:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:alternate-qwerty *alternate-qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k)))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *active-world* :timer])])

;;; Status widget for player

(defvar *status* nil)

(define-prototype status (:parent rlx:=formatter=)
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
		       ".gray40")))
	[print self (symbol-name stat-name)
	       :foreground ".white"]
	[print self ":["]
	[print self (format nil "~S" value) 
	       :foreground ".yellow"
	       :background color]
	(when unit 
	  [print self " "]
	  [print self (symbol-name unit)])
	[print self "]"]
	))))

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

(define-method print-object-tag status (object)
  (clon:with-field-values (name tile) object
    [print self nil :image tile]
    [print self " "]
    [print self name]
    [print self "  "]))
  
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
  (assert <character>)
  (when <character>
    (let* ((char <character>)
	   (hits [stat-value char :hit-points]))
      [println self "PLAYER STATUS:" :foreground ".white" :background ".blue"]
      [print-stat self :hit-points :warn-below 10]
      [print-stat-bar self :hit-points :color ".red"]
      [newline self]
      [print-stat self :oxygen :warn-below 50]
      [print self " "]
      [print-stat self :speed :warn-below 5]
      [print self " "]
      ;; TODO left hand, right hand, face
      [newline self])))
      
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

;;; Main program. 

(defparameter *tower-window-width* 800)
(defparameter *tower-window-height* 600)

(defvar *viewport*)

(defun tower ()
  (rlx:message "Initializing Tower Escape...")
  (setf clon:*send-parent-depth* 2) 
  (rlx:set-screen-height *tower-window-height*)
  (rlx:set-screen-width *tower-window-width*)
  (rlx:set-frame-rate 30)
  (rlx:set-timer-interval 20)
  (rlx:enable-timer)
  (rlx:enable-held-keys 1 15)
  (let* ((prompt (clone =tower-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (ship-status (clone =status=))
	 (status (clone =status=))
	 (player (clone =player=))
	 (splash (clone =splash=))
	 (textbox (clone =textbox=))
	 (viewport (clone =viewport=))
	 (splash-prompt (clone =splash-prompt=))
	 (terminal (clone =narrator=))
	 (stack (clone =stack=)))
    ;;
    (setf *viewport* viewport)
    ;;
    [resize splash :height (- *tower-window-height* 20) :width *tower-window-width*]
    [move splash :x 0 :y 0]
    [resize splash-prompt :width 10 :height 10]
    [move splash-prompt :x 0 :y 0]
    [hide splash-prompt]
    [set-receiver splash-prompt splash]
    ;;
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    ;;
    (labels ((spacebar ()
	       (setf *status* status)
	       ;;
	       [resize status :height 105 :width *tower-window-width*]
	       [set-character status player]
	       [move status :x 0 :y 0]
	       ;;
	       [set-player universe player]
	       [play universe
	       	     :address '(=tower= :floor 5 :width *tower-size* :height *tower-size*)
	       	     :prompt prompt
	       	     :narrator terminal
	       	     :viewport viewport]
	       [loadout player]
	       ;;
	       [update status]
	       [set-tile-size viewport 32]
	       [resize viewport :height 580 :width *tower-window-width*]
	       [move viewport :x 0 :y 0]
	       [set-origin viewport :x 0 :y 0 
			   :height (truncate (/ (- *tower-window-height* 100) 32))
			   :width (truncate (/ *tower-window-width* 32))]
	       [adjust viewport]))
      (setf *space-bar-function* #'spacebar))
    ;;
    [set-buffer textbox
    		(find-resource-object "help-message")]
    [resize-to-fit textbox] 
    [move textbox :x 0 :y 0]
    
    ;; (play-music "theme" :loop t)
    (set-music-volume 255)	       
    ;;
    [resize stack :width *tower-window-width* :height *tower-window-height*]
    [move stack :x 0 :y 0]
    [set-children stack (list status viewport)]
    ;;
    [resize terminal :height 100 :width *tower-window-width*]
    [move terminal :x 0 :y (- *tower-window-height* 100)]
    [set-verbosity terminal 0]
    ;; [move stack2 :x *left-column-width* :y 0]
    ;; [resize stack2 :width *right-column-width* :height 580]
    ;; [set-children stack2 (list terminal)]
    ;;
    ;; HACK
    ;; (labels ((light-hack (sr sc r c &optional (color ".white"))
    ;; 	       (labels ((hack-overlay (image)
    ;; 			  (multiple-value-bind (sx sy)
    ;; 			      [get-screen-coordinates viewport sr sc]
    ;; 			    (multiple-value-bind (x y)
    ;; 				[get-screen-coordinates viewport r c]
    ;; 			      (draw-line x y sx sy :destination image
    ;; 					 :color color)))))
    ;; 		 [add-overlay viewport #'hack-overlay])))
    ;;   (setf rlx::*lighting-hack-function* #'light-hack))
    ;;
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    (rlx:install-widgets splash-prompt splash)
    [add-page *pager* :play stack prompt status viewport terminal]
    [add-page *pager* :help textbox]))

(tower)
