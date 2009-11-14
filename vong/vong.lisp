;;; vong.lisp --- vong escape roguelike

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

(defpackage :vong
  (:documentation "VONG is a variation on Pong.")
  (:use :rlx :common-lisp)
  (:export vong))

(in-package :vong)

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
  (stepping :initform t)
  (attacking-with :initform :right-hand)
  (light-radius :initform 3)
  (categories :initform '(:actor :player :target :container :light-source)))

(define-method quit player ()
  (rlx:quit :shutdown))

;;; Controlling the game

(define-prototype vong-prompt (:parent rlx:=prompt=))

(defparameter *numpad-keybindings* 
  '(("KP8" nil "move :north .")
    ("KP4" nil "move :west .")
    ("KP6" nil "move :east .")
    ("KP2" nil "move :south .")
    ;;
    ("KP8" (:control) "fire :north .")
    ("KP4" (:control) "fire :west .")
    ("KP6" (:control) "fire :east .")
    ("KP2" (:control) "fire :south .")))

(defparameter *qwerty-keybindings*
  (append *numpad-keybindings*
	  '(("K" nil "move :north .")
	    ("H" nil "move :west .")
	    ("L" nil "move :east .")
	    ("J" nil "move :south .")
	    ;;
	    ("K" (:control) "fire :north .")
	    ("H" (:control) "fire :west .")
	    ("L" (:control) "fire :east .")
	    ("J" (:control) "fire :south .")
	    ;;
	    ("W" nil "wait .")
	    ("SPACE" nil "wait .")
	    ("PERIOD" (:control) "restart .")
	    ("KP-ENTER" nil "enter .")
	    ("RETURN" nil "enter .")
	    ("ESCAPE" (:control) "show-location .")
	    ("Q" (:control) "quit ."))))
  
(define-method install-keybindings vong-prompt ()
  (dolist (k *qwerty-keybindings*)
      (apply #'bind-key-to-prompt-insertion self k))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *active-world* :timer])])

      
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

(defparameter *vong-window-width* 800)
(defparameter *vong-window-height* 600)

(defvar *viewport*)

(defun vong ()
  (rlx:message "Initializing Vong...")
  (setf clon:*send-parent-depth* 2) 
  (rlx:set-screen-height *vong-window-height*)
  (rlx:set-screen-width *vong-window-width*)
  ;; enable pseudo timing
  (rlx:set-frame-rate 30)
  (rlx:set-timer-interval 20)
  (rlx:enable-timer)
  (rlx:enable-held-keys 1 15)
  ;; go!
  (let* ((prompt (clone =vong-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
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
    [resize splash :height (- *vong-window-height* 20) :width *vong-window-width*]
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
	       ;;
	       [set-player universe player]
	       [play universe
	       	     :address '(=vong= :level 1)
	       	     :prompt prompt
	       	     :narrator terminal
	       	     :viewport viewport]
	       [loadout player]
	       ;;
	       [set-tile-size viewport 32]
	       [resize viewport :height 470 :width *vong-window-width*]
	       [move viewport :x 0 :y 0]
	       [set-origin viewport :x 0 :y 0 
			   :height (truncate (/ (- *vong-window-height* 130) 32))
			   :width (truncate (/ *vong-window-width* 32))]
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
    [resize stack :width *vong-window-width* :height *vong-window-height*]
    [move stack :x 0 :y 0]
    [set-children stack (list viewport terminal)]
    ;;
    [resize terminal :height 80 :width *vong-window-width*]
    [move terminal :x 0 :y (- *vong-window-height* 80)]
    [set-verbosity terminal 0]
    ;;
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    (rlx:install-widgets splash-prompt splash)
    [add-page *pager* :play prompt stack viewport terminal]
    [add-page *pager* :help textbox]))

(vong)
