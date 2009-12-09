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

;;; Reflects light 

(defparameter *earth-tiles* '("earth-1" 
			      "earth-2"
			      "earth-3"
			      "earth-4"
			      "earth-5"
			      "earth-6"
			      "floor"))

(defparameter *earth-light-radius* 14)

(defcell earth 
  (tile :initform "floor")
  (categories :initform '(:actor :reflective)))

(define-method run earth ()
  (let ((dist [distance-to-player self]))
    (setf <tile> (if (< dist *earth-light-radius*)
		     (nth (truncate (/ dist 2)) *earth-tiles*)
		     "floor"))))
    
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
  
;;; The forest

(defcell drop-point 
  (categories :initform '(:player-entry-point))
  (tile :initform nil))

(defparameter *forest-size* 80)

(define-prototype forest (:parent xe2:=world=)
  (height :initform *forest-size*)
  (width :initform *forest-size*)
  (ambient-light :initform *earth-light-radius*)
  (edge-condition :initform :block))

(define-method drop-earth forest ()
  (dotimes (i <height>)
    (dotimes (j <width>)
      [drop-cell self (clone =earth=) i j])))

(define-method generate forest (&key (height *forest-size*)
				     (width *forest-size*))
  (setf <height> height)
  (setf <width> width)
  [create-default-grid self]
  [drop-earth self]
  [drop-cell self (clone =drop-point=) 
	     (1+ (random 10)) 
	     (1+ (random 10))
	     :exclusive t :probe t])

(define-method begin-ambient-loop forest ()
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
