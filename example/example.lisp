;;; example.lisp --- simple xe2 example game

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

(defpackage :xe2-example
  (:documentation "Example xe2 game.")
  (:use :xe2 :common-lisp)
  (:export xe2-example))

(in-package :xe2-example)

;;; Turn on timing after SDL init

(add-hook 'xe2:*initialization-hook*
	  #'(lambda ()
	      (xe2:enable-timer)
	      (xe2:set-frame-rate 30)
	      (xe2:set-timer-interval 1)
	      (xe2:enable-held-keys 1 3)))

;;; Floor tiles

(defcell floor 
  (tile :initform "floor"))

;;; Walls

(defcell wall 
  (tile :initform "wall")
  (categories :initform '(:obstacle :opaque :exclusive :wall))
  (hit-points :initform (make-stat :base 1 :min 0))) 
  
(define-method hit wall ()
  (setf <tile> "debris")
  [delete-category self :obstacle]
  [delete-category self :wall])

;;; The bouncing ball

(defcell ball 
  (tile :initform "ball")
  (categories :initform '(:actor))
  (direction :initform (xe2:random-direction))
  (hit-points :initform (make-stat :base 5 :min 0)))

(define-method serve ball (direction)
  [play-sample self "serve"]
  (setf <direction> direction))

(define-method run ball ()
  (when (eq <direction> :here) (setf <direction> (random-direction)))
  (clon:with-fields (direction row column) self
    (multiple-value-bind (r c) (xe2:step-in-direction row column direction)
      (if [obstacle-at-p *active-world* r c]
	  (progn 
	    ;; is it a wall or character? then hit it
	    (let ((object [category-at-p *active-world* r c '(:wall :person)]))
	      (when object
		[hit object]
		[damage self 1]))
	    ;; bounce
	    [play-sample self "bip"]
	    (setf direction (xe2:random-direction)))
	  ;; move along
	  [move self direction]))))

;;; The player

(defcell player 
  (tile :initform "player")
  (name :initform "Player")
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (movement-cost :initform (make-stat :base 10))
  (stepping :initform t)
  (categories :initform '(:actor :player :obstacle)))

(define-method quit player ()
  (xe2:quit :shutdown))

(define-method run player ()
  ;; if you are in category :actor, this is called every turn
  nil)

(define-method serve-ball player (direction)
  (let ((ball (clone =ball=)))
    [drop self ball]
    [serve ball direction]))
  
;;; Non player characters who wander around

(defcell person 
  (tile :initform (car (one-of '("character" "character-pink"))))
  (speed :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 30))
  (categories :initform '(:actor :obstacle :person)))

(define-method run person ()
  [move self (random-direction)])

(define-method hit person ()
  [play-sample self "ouch"])

;;; The example room

(defcell drop-point 
  (categories :initform '(:player-entry-point))
  (tile :initform "floor"))

(defparameter *room-size* 40)

(define-prototype room (:parent xe2:=world=)
  (height :initform *room-size*)
  (width :initform *room-size*)
  (edge-condition :initform :block))

(define-method drop-floor room ()
  (clon:with-field-values (height width) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))))

(define-method drop-people room ()
  (clon:with-field-values (height width) self
    (dotimes (i 10)
      [drop-cell self (clone =person=) (random height) (random width)])))

(define-method drop-wall room (r0 c0 r1 c1)
  (trace-line #'(lambda (x y)
		  (prog1 nil
		      (when [drop-cell self (clone =wall=) y x])))
	      c0 r0 c1 r1))

(define-method generate room (&key (height *room-size*)
				   (width *room-size*))
  (setf <height> height)
  (setf <width> width)
  [create-default-grid self]
  [drop-floor self]
  [drop-people self]
  (dotimes (i 20)
    (let ((column (1+ (random *room-size*)))
	  (row (1+ (random *room-size*)))
	  (len (random (truncate (/ *room-size* 2))))
	  (len2 (random (truncate (/ *room-size* 4)))))
      (progn 
	[drop-wall self row column 
		   (min (- height 1)
			(+ row len))
		   (min (- width 1)
			column)]
	(destructuring-bind (r c)
	    (midpoint (list row column)
		      (list (+ row len) column))
	  [drop-wall self r c r (+ column len)]))))
  [drop-cell self (clone =drop-point=) 
	     (1+ (random 10)) 
	     (1+ (random 10))
	     :exclusive t :probe t])

(define-method begin-ambient-loop room ()
  (play-music "frantix" :loop t))

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
				   [run-cpu-phase *active-world* :timer])])

;;; Main program. 

(defparameter *room-window-width* 800)
(defparameter *room-window-height* 600)

(defun init-example ()
  (xe2:message "Initializing Example...")
  (setf clon:*send-parent-depth* 2) ;; i'll fix this
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
	  :address '(=room=)
	  :player player
	  :narrator narrator
	  :prompt prompt
	  :viewport viewport]
    [set-tile-size viewport 32]
    [resize viewport :height 470 :width *room-window-width*]
    [move viewport :x 0 :y 0]
    [set-origin viewport :x 0 :y 0 
		:height (truncate (/ (- *room-window-height* 130) 32))
		:width (truncate (/ *room-window-width* 32))]
    [adjust viewport] 
    [narrateln narrator "You are the green guy."]
    [narrateln narrator "Use the numpad or nethack keys to move; Control-direction to fire."]
   ;;
    (xe2:install-widgets prompt viewport narrator)))

(init-example)
