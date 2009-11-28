;;; xiobreak.lisp --- alternate-universe breakout clone

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

(defpackage :rlx-xiobreak
  (:documentation "Xiobreak rlx game.")
  (:use :rlx :common-lisp)
  (:export rlx-xiobreak))

(in-package :rlx-xiobreak)

;;; Turn on timing after SDL init

(add-hook 'rlx:*initialization-hook*
	  #'(lambda ()
	      (rlx:enable-timer)
	      (rlx:set-frame-rate 30)
	      (rlx:set-timer-interval 1)
	      (rlx:enable-held-keys 1 1)))

;;; Scoring points

(defun score (points)
  [score-points [get-player *active-world*] points])

;;; Counting bricks

(defvar *bricks* 0)

;;; Which way the player is going

(defvar *english* :left)

;;; Colors

(defparameter *colors* '(:purple :red :blue :orange :green :yellow :white))

(defparameter *color-schemes* '((:purple :green :blue)
				(:yellow :purple :blue)
				(:red :yellow :orange)))

;;; Floor tiles

(defcell floor 
  (tile :initform "floor"))

;;; The wall around the gameworld

(defcell wall-horizontal
  (tile :initform "wall")
  (orientation :initform :horizontal)
  (categories :initform '(:obstacle :oriented)))

(defcell wall-vertical
  (tile :initform "wall")
  (orientation :initform :vertical)
  (categories :initform '(:obstacle :oriented)))

(defcell pit
  (tile :initform "floor")
  (orientation :initform :horizontal)
  (categories :initform '(:obstacle :oriented :pit)))

;;; The bouncing ball

(defparameter *ball-bounce-time* 12)

(defsprite ball 
  (image :initform "ball")
  (speed :initform (make-stat :base 5))
  (bounce-clock :initform 0)
  (movement-cost :initform (make-stat :base 1))
  (categories :initform '(:actor))
  (direction :initform :north))

(define-method serve ball (direction)
  [play-sample self "serve"]
  (setf <direction> direction))

(defparameter *vertical-collision*
  '(:northeast :northwest
    :northwest :northeast
    :southeast :southwest
    :southwest :southeast))

(defparameter *horizontal-collision*
  '(:southeast :northeast
    :northeast :southeast
    :southwest :northwest
    :northwest :southwest))

(define-method do-collision ball (object)
  "Choose a new direction based on colliding with the object at DIRECTION."
  [expend-action-points self 1]
  [play-sample self "bip"]
  (block colliding
    (when object
      (unless (eq object self)
	(when [in-category object :brick]
	  [hit object])
	(setf <direction>
	      (if [in-category object :pit]
		  (progn [die self] (return-from colliding))
		  (if [in-category object :paddle]
		      (ecase *english* 
			(:west :northwest)
			(:east :northeast))
		      (if (has-field :orientation object)
			  (let ((rule (ecase (field-value :orientation object)
					(:horizontal *horizontal-collision*)
					(:vertical *vertical-collision*))))
			    (getf rule <direction>))
			  (random-direction)))))
	(multiple-value-bind (y x) (rlx:step-in-direction <y> <x> <direction> 7)
	  [update-position self x y])))))


(define-method run ball ()
  (if (zerop <bounce-clock>)
      (progn [expend-action-points self 2]
	     (multiple-value-bind (y x) (rlx:step-in-direction <y> <x> <direction> 7)
	       [update-position self x y]))
      (progn 
	(setf <bounce-clock> (max 0 (1- <bounce-clock>)))
	(multiple-value-bind (y x) (rlx:step-in-direction <y> <x> <direction> 7)
	  [update-position self x y]))))

 
;;; Bust these bricks

(defvar *brick-tiles* '(:purple "brick-purple"
			:black "brick-black"
			:red "brick-red"
			:blue "brick-blue"
			:orange "brick-orange"
			:green "brick-green"
			:white "brick-white"
			:yellow "brick-yellow"))

(defcell brick 
  (name :initform "Brick")
  (tile :initform "brick-purple")
  (orientation :initform :horizontal)
  (categories :initform '(:exclusive :actor :obstacle :brick :horizontal :oriented))
  (color :initform :purple))

(define-method run brick ()
  (clon:with-field-values (row column) self
    (multiple-value-bind (r0 c0) (step-in-direction row column :east)
      (multiple-value-bind (r1 c1) (step-in-direction row column :west)
	(unless (and [category-at-p *active-world* r0 c0 :brick]
		     [category-at-p *active-world* r1 c1 :brick])
	  (setf <orientation> :vertical))))))

(define-method paint brick (c)
  (setf <color> c)
  (let ((res (getf *brick-tiles* c)))
    (assert (stringp res))
    (setf <tile> res)))

(define-method initialize brick ()
  (incf *bricks*))

(define-method hit brick ()
  [die self])

(define-method die brick ()
  (score 100)
  (decf *bricks*)
  [parent>>die self])

;;; The paddle

(defparameter *serve-key-delay* 6)

(defcell paddle 
  (tile :initform "player")
  (next-piece :initform nil)
  (previous-piece :initform nil)
  (orientation :initform :horizontal)
  (initialized :initform nil)
  (name :initform "paddle")
  (serve-key-clock :initform 0)
  (lives :initform (make-stat :base 5))
  (score :initform (make-stat :base 0 :min 0))
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (movement-cost :initform (make-stat :base 10))
  (categories :initform '(:actor :player :obstacle :paddle :oriented)))

(define-method obstructed paddle (direction)
  (or (when <next-piece> [obstructed <next-piece> direction])
      [category-in-direction-p *active-world* <row> <column> direction :obstacle]))

(define-method restart paddle ()
  (let ((player (clone =paddle=)))
    [destroy *active-universe*]
    [set-player *active-universe* player]
    [play *active-universe*
	  :address '(=room=)]
    [loadout player]))

(define-method quit paddle ()
  (rlx:quit :shutdown))

(define-method run paddle ()
  (when (plusp <serve-key-clock>)
    (decf <serve-key-clock>))
  (setf <initialized> t))

(define-method lose-life paddle ()
  [stat-effect self :lives -1])

(define-method attach paddle (piece)
  (setf <next-piece> piece)
  (setf (clon:field-value :previous-piece piece) self))

(define-method initialize paddle ()
  (message "created paddle"))

(define-method serve-ball paddle (direction)
  (if (zerop <serve-key-clock>)
      (if (plusp [stat-value self :lives])
	  (let ((ball (clone =ball=)))
	    (setf <serve-key-clock> *serve-key-delay*)
	    (multiple-value-bind (x y) [viewport-coordinates self]
	      [drop-sprite self ball :x (+ x 30) :y (- y 20)]
	      [serve ball direction]))
	  [play-sound self "error"])
      (setf <serve-key-clock> (max 0 (- <serve-key-clock> 1)))))
 
(define-method move paddle (direction &optional slave)
  (assert (member direction '(:east :west)))
  (setf *english* direction)
  (if slave
      [parent>>move self direction :ignore-obstacles]
      (progn [parent>>move self direction :ignore-obstacles]
	     (when [obstructed self direction]
	       (let ((piece <next-piece>))
		 (loop do (when piece [>>move piece direction t])
			  (setf piece (clon:field-value :next-piece piece))
		       while piece))))))
  
(define-method loadout paddle ()
  (let ((last-piece self))
    (dotimes (n 4)
      (let ((piece (clone =paddle=)))
	[drop-cell *active-world* piece <row> (+ n 1 <column>)]
	[attach last-piece piece]
	(setf last-piece piece)))))

;;; The xiobreak room

(defcell drop-point 
  (categories :initform '(:player-entry-point))
  (tile :initform "floor"))

(defparameter *room-height* 29)
(defparameter *room-width* 40)

(define-prototype room (:parent rlx:=world=)
  (height :initform *room-height*)
  (width :initform *room-width*)
  (edge-condition :initform :block))

(define-method drop-border room ()
  (clon:with-field-values (height width) self
    (labels ((drop-horz-wall (r c)
	       (prog1 nil [drop-cell self (clone =wall-horizontal=) r c]))
	     (drop-vert-wall (r c)
	       (prog1 nil [drop-cell self (clone =wall-vertical=) r c]))
	     (drop-pit (r c)
	       (prog1 nil [drop-cell self (clone =pit=) r c])))
      (drop-horz-wall 0 0)
      (trace-row #'drop-horz-wall 0 0 width)
      (trace-row #'drop-pit (- height 1) 0 width)
      (trace-column #'drop-vert-wall 0 0 (- height 1))
      (trace-column #'drop-vert-wall (- width 1) 0 (- height 1)))))
	          
(define-method drop-floor room ()
  (clon:with-field-values (height width) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))))

(define-method drop-bricks room (row x0 x1 color)
  (labels ((drop-brick (r c)
	     (let ((brick (clone =brick=)))
	       [paint brick color]
	       [drop-cell self brick r c])))
    (rlx:trace-row #'drop-brick row x0 x1)))

(define-method generate room (&key (height *room-height*)
				   (width *room-width*))
  (setf <height> height)
  (setf <width> width)
  [create-default-grid self]
  [drop-floor self]
  [drop-border self]
  (let ((scheme (car (one-of *color-schemes*))))
    (let ((row 3))
      (dotimes (n 3)
	(dolist (color scheme)
	  [drop-bricks self row 5 35 color]
	  (incf row)))))
  [drop-cell self (clone =drop-point=) 25 5])

;; (define-method begin-ambient-loop room ()
;;   (play-music "frantix" :loop t))

;;; Controlling the game

(define-prototype room-prompt (:parent rlx:=prompt=))

(defparameter *numpad-keybindings* 
  '(("KP4" nil "mov :west .")
    ("KP6" nil "move :east .")
    ;;
    ("KP7" (:control) "serve-ball :northwest .")
    ("KP9" (:control) "serve-ball :northeast .")
    ("LEFT" nil "move :west .")
    ("RIGHT" nil "move :east .")
    ;;
    ("Z" nil "serve-ball :northwest .")
    ("X" nil "serve-ball :northeast .")))

(defparameter *qwerty-keybindings*
  (append *numpad-keybindings*
	  '(("H" nil "move :west .")
	    ("L" nil "move :east .")
	    ;;
	    ("Y" (:control) "serve-ball :northwest .")
	    ("U" (:control) "serve-ball :northeast .")
	    ;;
	    ("ESCAPE" nil "restart .")
	    ("Q" (:control) "quit ."))))

(define-method install-keybindings room-prompt ()
  (dolist (k (append *numpad-keybindings* *qwerty-keybindings*))
    (apply #'bind-key-to-prompt-insertion self k))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *active-world* :timer])])

;;; Main program. 

(defparameter *room-window-width* 800)
(defparameter *room-window-height* 600)

(defparameter *tile-size* 16)

(defun init-xiobreak ()
  (rlx:message "Initializing Xiobreak...")
  (clon:initialize)
  (rlx:set-screen-height *room-window-height*)
  (rlx:set-screen-width *room-window-width*)
  (let* ((prompt (clone =room-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (player (clone =paddle=))
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
	  :address '(=room=)
	  :player player
	  :narrator narrator
	  :prompt prompt
	  :viewport viewport]
    [loadout player]
    [set-tile-size viewport *tile-size*]
    [resize viewport :height 470 :width *room-window-width*]
    [move viewport :x 0 :y 0]
    [set-origin viewport :x 0 :y 0 
		:height (truncate (/ (- *room-window-height* 130) *tile-size*))
		:width (truncate (/ *room-window-width* *tile-size*))]
    [adjust viewport] 
    (rlx:install-widgets prompt viewport)))

(init-xiobreak)
