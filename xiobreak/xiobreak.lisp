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
	      (rlx:set-timer-interval 0)
	      (rlx:enable-held-keys 1 1)))

(defparameter *xiobreak-window-width* 800)
(defparameter *xiobreak-window-height* 600)
(defparameter *tile-size* 16)

;;; Being alive

(defvar *alive* t)

;;; Scoring points

(defun score (points)
  [stat-effect [get-player *active-world*] :score points])

;;; Counting bricks

(defvar *bricks* 0)

;;; Which way the player is going

(defvar *english* :left)

;;; Colors

(defparameter *colors* '(:purple :red :blue :orange :green :yellow :white))

(defparameter *color-schemes* '((:yellow :orange :blue)
				(:yellow :purple :blue)
				(:purple :red :orange)
				(:green :yellow :purple)
				(:red :yellow :orange)))

;;; The underlying floor

(defcell floor 
  (tile :initform "floor"))

;;; The dancefloor tiles

(defparameter *light-tiles* '("floor"
			      "rezlight5"
			      "rezlight4"
			      "rezlight3"
			      "rezlight2"
			      "rezlight1"))

(defparameter *light-clock* 12)

(defcell dancefloor 
  (tile :initform nil)
  (clock :initform *light-clock*)
  (categories :initform '(:actor :dancefloor)))

(define-method update-tile dancefloor ()
  (when [is-located self]
    (unless [category-at-p *active-world* <row> <column> '(:brick :wall)]
      (setf <tile> (nth (truncate (/ <clock> 2)) *light-tiles*)))))

(define-method light dancefloor (&optional (time *light-clock*))
  (setf <clock> (max 0 time))
  [update-tile self])

(define-method light-toward dancefloor (direction)
  (multiple-value-bind (r c) (step-in-direction <row> <column> direction)
    (unless [category-at-p *active-world* r c :dancefloor]
      (let ((dancefloor (clone =dancefloor=)))
	[drop-cell *active-world* dancefloor r c]
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
  

;;; The wall around the gameworld

(defcell wall-horizontal
  (tile :initform "wall")
  (orientation :initform :horizontal)
  (categories :initform '(:obstacle :oriented :wall)))

(defcell wall-vertical
  (tile :initform "wall")
  (orientation :initform :vertical)
  (categories :initform '(:obstacle :oriented :wall)))

(defcell pit
  (tile :initform "floor")
  (orientation :initform :horizontal)
  (categories :initform '(:obstacle :oriented :pit)))

;;; The bouncing ball

(defparameter *ball-bounce-time* 12)

(defvar *balls* 0)

(defsprite ball 
  (image :initform "ball")
  (speed :initform (make-stat :base 20))
  (bounce-clock :initform 0)
  (dead :initform nil)
  (movement-distance :initform (make-stat :base 7 :min 0 :max 14))
  (movement-cost :initform (make-stat :base 10))
  (categories :initform '(:actor))
  (direction :initform :north))

(define-method serve ball (direction)
  [play-sample self "serve"]
  (incf *balls*)
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
	  [hit object self])
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
	(when (null <direction>)
	  (setf <direction> (car (one-of '(:northeast :northwest :southeast :southwest)))))
	(multiple-value-bind (y x) (rlx:step-in-direction <y> <x> <direction> 
							  [stat-value self :movement-distance])
	  [update-position self x y])))))

(define-method run ball ()
  (if (zerop <bounce-clock>)
      (progn [expend-action-points self 2]
	     (multiple-value-bind (y x) (rlx:step-in-direction <y> <x> <direction> 
							       [stat-value self :movement-distance])
	       [update-position self x y]))
      (progn 
	(setf <bounce-clock> (max 0 (1- <bounce-clock>)))
	(multiple-value-bind (y x) (rlx:step-in-direction <y> <x> <direction> 
							  [stat-value self :movement-distance])
	  [update-position self x y]))))

(define-method die ball ()
  (unless <dead>
    (setf <dead> t)
    (decf *balls*)
    (when (and (zerop [stat-value [get-player *active-world*] :balls])
	       (zerop *balls*))
      [play-sample self "doom"]
      (setf *alive* nil))
    [remove-sprite *active-world* self]))

;;; Plasma

(defparameter *plasma-tiles* '("rezblur5"
			       "rezblur4"
			       "rezblur3"
			       "rezblur2"
			       "rezblur1"))

(defparameter *plasma-samples* '("zap1" "zap2" "zap3"))

(defparameter *plasma-alt-samples* '("zap4" "zap5" "zap6" "zap7"))

(defparameter *plasma-sample-schemes* (list *plasma-samples* *plasma-alt-samples*))

(defcell plasma
  (tile :initform "rezblur1")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (clock :initform 5)
  (samples :initform *plasma-samples*)
  (categories :initform '(:actor :paint-source :plasma))
  (description :initform "Spreading toxic paint gas. Avoid at all costs!"))

(define-method set-clock plasma (clock)
  (setf <clock> clock))

(define-method run plasma ()
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      (let ((dir (random-direction)))
	(setf <tile> (nth <clock> *plasma-tiles*))
	[play-sample self (car (one-of <samples>))]
	[move self dir])))

;;; Fuzz

(defparameter *fuzz-tiles* '("psifuzz2"
			     "psifuzz1"))

(defcell fuzz
  (tile :initform "psifuzz1")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (clock :initform 2)
  (categories :initform '(:actor :paint-source :fuzz)))

(define-method set-clock fuzz (clock)
  (setf <clock> clock))

(define-method run fuzz ()
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      (let ((dir (random-direction)))
	(setf <tile> (nth <clock> *fuzz-tiles*))
	[move self dir])))

;;; Psi energy

(defparameter *psi-tiles* '("psiblur5"
			       "psiblur4"
			       "psiblur3"
			       "psiblur2"
			       "psiblur1"))

(defparameter *psi-fuzz-tiles* '("psifuzz1" "psifuzz2"))

(defparameter *psi-samples* '("nextpiano" "nextpiano2"))

(defparameter *psi-alt-samples* '("nextpiano" "nextpiano2"))

(defparameter *psi-sample-schemes* (list *psi-samples* *psi-alt-samples*))

(defcell psi
  (tile :initform "psiblur1")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform (random-direction))
  (clock :initform 10)
  (samples :initform *psi-samples*)
  (categories :initform '(:actor :paint-source :psi)))

(define-method set-clock psi (clock)
  (setf <direction> (random-direction))
  (setf <clock> clock))

(define-method run psi ()
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      (progn (setf <tile> (nth (truncate (/ <clock> 2)) *psi-tiles*))
	     [play-sample self (car (one-of <samples>))]
	     [move self <direction>])))

(define-method die psi ()
  [drop self (clone =fuzz=)]
  [delete-from-world self])

;;; Chi energy

(defparameter *chi-tiles* '("chiblur5"
			       "chiblur4"
			       "chiblur3"
			       "chiblur2"
			       "chiblur1"))

(defparameter *chi-samples* '("bass1" "bass3" "bass3" "bass3" "bass3" "bass3" "bass3"))

(defparameter *chi-alt-samples* '("snare1" "snare2"))

(defvar *beat* 0)

(defparameter *beat-max* 80)

(defparameter *snare-beats* '(19 59))

(defparameter *bd-beats* '(0 19 39 59))

(defparameter *bass-beats* '(0 59))

(defun click-beat ()
  (incf *beat*)
  (when (>= *beat-max* *beat*)
    (setf *beat* 0)))

(defun do-beat (cell)
  (when (member *beat* *snare-beats*)
    [play-sample cell (car (one-of *chi-alt-samples*))])
  (when (member *beat* *bd-beats*)
    [play-sample cell "bd"])
  (when (member *beat* *bass-beats*)
    [play-sample cell (car (one-of *chi-samples*))]))

(defparameter *chi-sample-schemes* (list *chi-samples* *chi-alt-samples*))

(defcell chi
  (tile :initform "chiblur1")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform (car (one-of '(:north :south :east :west))))
  (clock :initform 10)
  (samples :initform *chi-samples*)
  (categories :initform '(:actor :paint-source :chi)))

(define-method set-clock chi (clock)
  (setf <direction> (random-direction))
  (setf <clock> clock))

(define-method run chi ()
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      (progn (setf <tile> (nth (truncate (/ <clock> 2)) *chi-tiles*))
	     (do-beat self)
	     [move self <direction>])))

(define-method die chi ()
;;  [drop self (clone =fuzz=)]
  [delete-from-world self])

;;; Level themes

(defparameter *psi-theme* (list :stuff =psi=
				:sample-schemes *psi-sample-schemes*
				:music "next"))

(defparameter *chi-theme* (list :stuff =chi=
				:sample-schemes *chi-sample-schemes*
				:music "buzzo"))

(defparameter *plasma-theme* (list :stuff =plasma=
				:sample-schemes *plasma-sample-schemes*
				:music "rappy"))

(defparameter *themes* (list *plasma-theme* *psi-theme* *chi-theme*))

(defvar *theme* *plasma-theme*)

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

(define-method hit brick (&optional ball)
  (let ((floor (clone =dancefloor=)))
    [drop self floor]
    [light floor]
    [die self]))

(define-method splash brick (&optional (add 0))
  (let ((stuff-type (getf *theme* :stuff))
	(schemes (getf *theme* :sample-schemes)))
    (dotimes (n (+ add 5 (random 10)))
      [drop self (let ((stuff (clone stuff-type)))
		   (prog1 stuff
		     (setf (field-value :samples stuff)
			   (car (one-of schemes)))))])))

(define-method die brick ()
  (score 100)
  (decf *bricks*)
  [splash self]
  [parent>>die self])

;;; Makes the ball grow

(define-prototype grow-brick (:parent =brick=)
  (name :initform "Grow brick")
  (tile :initform "grow")
  (orientation :initform :horizontal))

(define-method hit grow-brick (&optional ball)
  (when ball
    [update-image ball "big-ball"]
    [die self]))

(define-method die grow-brick ()
  (score 1000)
  [splash self]
  [play-sample self "speedup"]
  [delete-from-world self])

;;; Extra ball

(define-prototype extra-brick (:parent =brick=)
  (name :initform "Extra brick")
  (tile :initform "extra")
  (orientation :initform :horizontal))

(define-method hit extra-brick (&optional ball)
  [die self])

(define-method die extra-brick ()
  (score 1000)
  [splash self]
  [stat-effect [get-player *active-world*] :balls 1]
  [play-sample self "speedup"]
  [delete-from-world self])

;;; Explode to score points

(define-prototype bomb-brick (:parent =brick=)
  (name :initform "Grow brick")
  (tile :initform "bomb")
  (orientation :initform :horizontal))

(define-method explode bomb-brick ()
  [splash self]
    (labels ((do-circle (image)
	     (prog1 t
	       (multiple-value-bind (x y) 
		   [viewport-coordinates self]
		 (let ((x0 (+ x 8))
		       (y0 (+ y 8)))
		 (draw-circle x0 y0 40 :destination image)
		 (draw-circle x0 y0 35 :destination image))))))
    [>>add-overlay :viewport #'do-circle])
  (dolist (dir '(:north :south :east :west :northeast :southeast :northwest :southwest))
    (multiple-value-bind (r c) (step-in-direction <row> <column> dir)
      (let ((brick [category-at-p *active-world* r c :brick]))
	(when brick [hit brick])))))

(define-method hit bomb-brick (&optional ball)
  (when ball
    [explode self]
    [stat-effect ball :movement-distance 3]
    [die self]))

(define-method die bomb-brick ()
  (score 1000)
  [splash self]
  [play-sample self "explode"]
  [delete-from-world self])

;;; The paddle

(defparameter *serve-key-delay* 7)

(defparameter *paddle-size* 5)

(defcell paddle 
  (tile :initform "player")
  (next-piece :initform nil)
  (previous-piece :initform nil)
  (hearing-range :initform 1000)
  (orientation :initform :horizontal)
  (initialized :initform nil)
  (name :initform "paddle")
  (serve-key-clock :initform 0)
  (balls :initform (make-stat :base 5))
  (score :initform (make-stat :base 0 :min 0))
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (movement-cost :initform (make-stat :base 10))
  (categories :initform '(:actor :player :obstacle :paddle :oriented)))

(define-method obstructed paddle (direction)
  (or (when <next-piece> [obstructed <next-piece> direction])
      [category-in-direction-p *active-world* <row> <column> direction :obstacle]))

(define-method restart paddle ()
  (let ((player (clone =paddle=)))
    (setf *alive* t)
    (setf *balls* 0)
    [destroy *active-universe*]
    (setf *theme* (car (one-of *themes*)))
    [set-player *active-universe* player]
    [set-character *status* player]
    [play *active-universe*
	  :address '(=room=)]
    [loadout player]))

(define-method quit paddle ()
  (rlx:quit :shutdown))

(define-method run paddle ()
  (setf <tile> (if *alive* "player" "floor"))
  [update *status*]
  (when (plusp <serve-key-clock>)
    (decf <serve-key-clock>))
  (setf <initialized> t))

(define-method attach paddle (piece)
  (setf <next-piece> piece)
  (setf (clon:field-value :previous-piece piece) self))

(define-method initialize paddle ()
  (message "created paddle"))

(define-method serve-ball paddle (direction)
  (if (zerop <serve-key-clock>)
      (if (plusp [stat-value self :balls])
	  (let ((ball (clone =ball=)))
	    [stat-effect self :balls -1]
	    (setf <serve-key-clock> *serve-key-delay*)
	    (multiple-value-bind (x y) [viewport-coordinates self]
	      [drop-sprite self ball :x (+ x 30) :y (- y 20)]
	      [serve ball direction]))
	  [play-sample self "error"])
      (setf <serve-key-clock> (max 0 (- <serve-key-clock> 1)))))
 
(define-method move paddle (direction &optional slave)
  (if slave
      [parent>>move self direction :ignore-obstacles]
      (clon:with-field-values (width player) *active-world*
	;; don't allow paddle off screen
	(if (ecase direction
	      (:west (< 1 <column>))
	      (:east (< <column> (- width *paddle-size* 2))))
	    (progn (setf *english* direction)
		   [parent>>move self direction :ignore-obstacles]
		   (let ((piece <next-piece>))
		     (loop do (when piece [>>move piece direction t])
			      (setf piece (clon:field-value :next-piece piece))
			   while piece)))
	    [play-sample self "bip"]))))

(define-method loadout paddle ()
  (let ((last-piece self))
    (dotimes (n *paddle-size*)
      (let ((piece (clone =paddle=)))
	[drop-cell *active-world* piece <row> (+ n 1 <column>)]
	[attach last-piece piece]
	(setf last-piece piece)))))

;;; The xiobreak room

(defcell drop-point 
  (categories :initform '(:player-entry-point))
  (tile :initform "floor"))

(defparameter *room-height* (truncate (/ (- *xiobreak-window-height* 20) *tile-size*)))
(defparameter *room-width* (truncate (/ *xiobreak-window-width* *tile-size*)))

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

(define-method drop-brick-row room (row x0 x1 color)
  (labels ((drop-brick (r c)
	     (let ((brick (clone =brick=)))
	       [paint brick color]
	       [drop-cell self brick r c])))
    (rlx:trace-row #'drop-brick row x0 x1)))

(defparameter *classic-layout-horz-margin* 0)

(defparameter *classic-layout-top-margin* 4)

(defparameter *classic-layout-layers* 2)

(define-method drop-classic-layout room (&optional (row-delta 0))
  (let ((left *classic-layout-horz-margin*)
	(right (- <width> 2 *classic-layout-horz-margin*))
	(row (+ 1 row-delta *classic-layout-top-margin*))
	(scheme (car (one-of *color-schemes*))))
    (dotimes (n *classic-layout-layers*)
      (dolist (color scheme)
	[drop-brick-row self row left right color]
	(incf row)))))

(define-method generate room (&key (height *room-height*)
				   (width *room-width*)
				   (grow-bricks 2)
				   (bomb-bricks 12)
				   (extra-bricks 4))
  (setf <height> height)
  (setf <width> width)
  [create-default-grid self]
  [drop-floor self]
  [drop-border self]
  [drop-classic-layout self]
  (dotimes (n grow-bricks)
    (let ((row (1+ (random 5)))
	  (column (1+ (random (- width 1)))))
      [delete-category-at self row column :brick]
      [drop-cell self (clone =grow-brick=) row column]))
  (dotimes (n extra-bricks)
    (let ((row (1+ (random 5)))
	  (column (1+ (random (- width 1)))))
      [delete-category-at self row column :brick]
      [drop-cell self (clone =extra-brick=) row column]))
  (dotimes (n bomb-bricks)
    (let ((row (+ 6 (random 5)))
	  (column (1+ (random (- width 1)))))
      [delete-category-at self row column :brick]
      [drop-cell self (clone =bomb-brick=) row column]))
  [drop-cell self (clone =drop-point=) 32 5])

(define-method begin-ambient-loop room ()
  (play-music (getf *theme* :music) :loop t))

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
				   (click-beat)
				   [run-cpu-phase *active-world* :timer])])

;;; A status widget for score display

(defvar *status*)

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
	[print self (format nil "   SCORE: ~S" [stat-value char :score])]
	[print self (format nil "   BALLS: ~S" [stat-value char :balls])]
	[newline self])))

;;; Main program. 

(defun init-xiobreak ()
  (rlx:message "Initializing Xiobreak...")
  (clon:initialize)
  (rlx:set-screen-height *xiobreak-window-height*)
  (rlx:set-screen-width *xiobreak-window-width*)
  (let* ((prompt (clone =room-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (status (clone =status=))
	 (player (clone =paddle=))
	 (viewport (clone =viewport=)))
    (setf *alive* t)
    (setf *balls* 0)
    (setf *theme* (car (one-of (list *psi-theme* *chi-theme* *plasma-theme*))))
    ;;
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    ;;
    (setf *status* status)
    [resize status :height 20 :width *xiobreak-window-width*]
    [move status :x 0 :y (- *xiobreak-window-height* 20)]
    [set-character status player]
    ;;
    [resize narrator :height 80 :width *xiobreak-window-width*]
    [move narrator :x 0 :y (- *xiobreak-window-height* 80)]
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
    [resize viewport :height 470 :width *xiobreak-window-width*]
    [move viewport :x 0 :y 0]
    [set-origin viewport :x 0 :y 0 
		:height (truncate (/ *xiobreak-window-height* *tile-size*))
		:width (truncate (/ *xiobreak-window-width* *tile-size*))]
    [adjust viewport] 
    (rlx:install-widgets prompt viewport status)))

(init-xiobreak)
