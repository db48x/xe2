;;; xiotank.lisp --- 7DRL

;; Copyright (C) 2010  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: 

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

;;; Commentary:

;; 

;;; Code:

(defpackage :xiotank
  (:documentation "xiotank is a sonic roguelike combat game.")
  (:use :xe2 :common-lisp)
  (:export xiotank))

(in-package :xiotank)

(setf xe2:*dt* 10)
(setf xe2:*physics-function* #'(lambda (&rest ignore)
				 [run-cpu-phase *world* :timer]))

;;; Sound waves


(defparameter *wave-types* '(:sine :square :saw))
(defparameter *wave-colors* '(:yellow :cyan :magenta :green))

(defparameter *wave-samples*
  '((:sine "A-2-sine" "A-4-sine")
    (:saw "A-2-saw" "A-4-saw")
    (:square "A-2-square" "A-4-square")))

(defun wave-sample (type &optional (note "A-4"))
  (assert (member type *wave-types*))
  (concatenate 'string note "-" (string-downcase (symbol-name type))))

(defparameter *wave-images*
  '((:sine :green "sine-green" :yellow "sine-yellow" :magenta "sine-magenta" :cyan "sine-cyan")
    (:square :green "square-green" :yellow "square-yellow" :magenta "square-magenta" :cyan "square-cyan")
    (:saw :green "saw-green" :yellow "saw-yellow" :magenta "saw-magenta" :cyan "saw-cyan")))

(defun wave-image (type color)
  (assert (and (member type *wave-types*)
	       (member color *wave-colors*)))
  (getf (cdr (assoc type *wave-tiles*))
	color))

(defsprite wave
  (type :initform :sine)
  (image :initform nil)
  (direction :initform nil)
  (speed :initform (make-stat :base 20))
  (movement-distance :initform (make-stat :base 2))
  (movement-cost :initform (make-stat :base 10))
  (categories :initform '(:wave :actor)))

(define-method start wave (&key note (type :sine) (direction :north))
  (setf <type> type)
  (setf <image> (wave-image type))
  (setf <sample> (wave-sample type note))
  (setf <direction> direction))

(define-method run wave ()
  [expend-action-points self 2]
  (mutliple-value-bind (y x) (xe2:step-in-direction <y> <x> <direction>
						    [stat-value self :movement-distance])
		       [update-position self x y]))

;;(define-method do-collision wave ()
				
;;(define-method die wave 

(defcell tank 
  (tile :initform "tank-north")
  (hit-points :initform (make-stat :base 45 :min 0 :max 45))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (speed :initform (make-stat :base 10 :min 0 :max 25))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 15)
  (energy :initform (make-stat :base 40 :min 0 :max 40 :unit :gj))
  (hit-points :initform (make-stat :base 45 :min 0 :max 45))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (stepping :initform t)
  (direction :initform :north)
  (attacking-with :initform nil)
  (firing-with :initform :center-bay)
  (categories :initform '(:actor :player :target :container :light-source :vehicle :repairable))
  (equipment-slots :initform '(:left-bay :right-bay :center-bay :extension)))

(defparameter *tank-tiles* '(:north "tank-north"
			     :south "tank-south"
			     :east "tank-east"
			     :west "tank-west"
			     :northeast "tank-northeast"
			     :northwest "tank-northwest"
			     :southeast "tank-southeast"
			     :southwest "tank-southwest"))

(define-method move tank (direction)
  (setf <direction> direction)
  (setf <tile> (getf *tank-tiles* direction))
  [parent>>move self direction])

(define-method run tank ()
  nil)
  
;;;; Basic blue world

(defcell blue-space 
  (tile :initform "blue-space"))

(define-prototype blue-world (:parent xe2:=world=)
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (scale :initform '(3 m))
  (edge-condition :initform :block))

(define-method begin-ambient-loop blue-world ()
  (play-music "purity" :loop t))

;; (define-method drop-base blue-world (row column &optional (size 5))
;;   (labels ((drop-panel (r c)
;; 	     (prog1 nil [drop-cell self (clone =xiotank-base=) r c])))
;;     (trace-rectangle #'drop-panel row column size size :fill)
;;     (dotimes (i 8)
;;       [drop-cell self (clone =guardic-eye=) 
;; 		 (+ row (random size)) (+ column (random size)) :loadout t])
;;     (dotimes (i (* 2 size))
;;       [drop-cell self (clone =xiotank-wires=)
;; 		 (+ row (random size)) (+ column (random size))])))

(define-method generate blue-world (&key (height 200)
					    (width 30)
					    sequence-number)
  (setf <height> height <width> width)
  [create-default-grid self]
  (dotimes (i height)
    (dotimes (j width)
      [drop-cell self (clone =blue-space=)
		 i j]))
  ;; (dotimes (i 20)
  ;;   [drop-cell self (clone =star=) (random height) (random width)]
  ;;   [drop-base self (random height) (random width) (+ 5 (random 10))])
  [drop-cell self (clone =launchpad=) (- height 8) 5])

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
					   (background-color ".gray40")
					   (divisor 1))
  (let ((value (truncate (/ [stat-value <character> stat] divisor)))
	(max (truncate (/ [stat-value <character> stat :max] divisor))))
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
	[print-stat self :hit-points :warn-below 10 :show-max t]
	[print-stat-bar self :hit-points :color ".red" :divisor 2]
	[print-stat self :energy :warn-below 10 :show-max t]
	[print-stat-bar self :energy :color ".yellow" :divisor 2]

;;	[print self (format nil "   SCORE:~S" [stat-value char :score])]
	[newline self])))

;;; Custom bordered viewport

(define-prototype view (:parent xe2:=viewport=))

(define-method render view ()
  [parent>>render self]
  (xe2:draw-rectangle 0 0 
		      <width>
		      <height>
		      :color ".blue" :destination <image>))

(defvar *view* (clone =view=))

;;; Keyboard controls

(define-prototype xiotank-prompt (:parent xe2:=prompt=))

(defparameter *basic-keybindings* 
  '(("KP7" nil "move :northwest .")
    ("KP8" nil "move :north .")
    ("KP9" nil "move :northeast .")
    ("KP4" nil "move :west .")
    ("KP6" nil "move :east .")
    ("KP1" nil "move :southwest .")
    ("KP2" nil "move :south .")
    ("KP3" nil "move :southeast .")
    ;;
    ("KP7" (:control) "fire :northwest .")
    ("KP8" (:control) "fire :north .")
    ("KP9" (:control) "fire :northeast .")
    ("KP4" (:control) "fire :west .")
    ("KP6" (:control) "fire :east .")
    ("KP1" (:control) "fire :southwest .")
    ("KP2" (:control) "fire :south .")
    ("KP3" (:control) "fire :southeast .")
    ;;
    ("KP7" (:alt) "fire :northwest .")
    ("KP8" (:alt) "fire :north .")
    ("KP9" (:alt) "fire :northeast .")
    ("KP4" (:alt) "fire :west .")
    ("KP6" (:alt) "fire :east .")
    ("KP1" (:alt) "fire :southwest .")
    ("KP2" (:alt) "fire :south .")
    ("KP3" (:alt) "fire :southeast .")
    ;;
    ("KP7" (:meta) "fire :northwest .")
    ("KP8" (:meta) "fire :north .")
    ("KP9" (:meta) "fire :northeast .")
    ("KP4" (:meta) "fire :west .")
    ("KP6" (:meta) "fire :east .")
    ("KP1" (:meta) "fire :southwest .")
    ("KP2" (:meta) "fire :south .")
    ("KP3" (:meta) "fire :southeast .")))
    ;; ;;
    ;; ("JOYSTICK" (:north :circle) "attack :north .")
    ;; ("JOYSTICK" (:northeast :circle) "attack :northeast .")
    ;; ("JOYSTICK" (:northwest :circle) "attack :northwest .")
    ;; ("JOYSTICK" (:east :circle) "attack :east .")
    ;; ("JOYSTICK" (:west :circle) "attack :west .")
    ;; ("JOYSTICK" (:south :circle) "attack :south .")
    ;; ("JOYSTICK" (:southwest :circle) "attack :southwest .")
    ;; ("JOYSTICK" (:southeast :circle) "attack :southeast .")
    ;; ;;
    ;; ("JOYSTICK" (:north :cross) "move :north .")
    ;; ("JOYSTICK" (:northeast :cross) "move :northeast .")
    ;; ("JOYSTICK" (:northwest :cross) "move :northwest .")
    ;; ("JOYSTICK" (:east :cross) "move :east .")
    ;; ("JOYSTICK" (:west :cross) "move :west .")
    ;; ("JOYSTICK" (:south :cross) "move :south .")
    ;; ("JOYSTICK" (:southwest :cross) "move :southwest .")
    ;; ("JOYSTICK" (:southeast :cross) "move :southeast .")
    ;; ;;
    ;; ("JOYSTICK" (:north :square) "fire :north .")
    ;; ("JOYSTICK" (:northeast :square) "fire :northeast .")
    ;; ("JOYSTICK" (:northwest :square) "fire :northwest .")
    ;; ("JOYSTICK" (:east :square) "fire :east .")
    ;; ("JOYSTICK" (:west :square) "fire :west .")
    ;; ("JOYSTICK" (:south :square) "fire :south .")
    ;; ("JOYSTICK" (:southwest :square) "fire :southwest .")
    ;; ("JOYSTICK" (:southeast :square) "fire :southeast .")))

(defparameter *qwerty-keybindings*
  (append *basic-keybindings*
	  '(("Y" nil "move :northwest .")
	    ("K" nil "move :north .")
	    ("U" nil "move :northeast .")
	    ("H" nil "move :west .")
	    ("L" nil "move :east .")
	    ("B" nil "move :southwest .")
	    ("J" nil "move :south .")
	    ("N" nil "move :southeast .")
	    ;;
	    ("Y" (:alt) "fire :northwest .")
	    ("K" (:alt) "fire :north .")
	    ("U" (:alt) "fire :northeast .")
	    ("H" (:alt) "fire :west .")
	    ("L" (:alt) "fire :east .")
	    ("B" (:alt) "fire :southwest .")
	    ("J" (:alt) "fire :south .")
	    ("N" (:alt) "fire :southeast .")
	    ;;
	    ("Y" (:meta) "fire :northwest .")
	    ("K" (:meta) "fire :north .")
	    ("U" (:meta) "fire :northeast .")
	    ("H" (:meta) "fire :west .")
	    ("L" (:meta) "fire :east .")
	    ("B" (:meta) "fire :southwest .")
	    ("J" (:meta) "fire :south .")
	    ("N" (:meta) "fire :southeast .")
	    ;;
	    ("Y" (:control) "fire :northwest .")
	    ("K" (:control) "fire :north .")
	    ("U" (:control) "fire :northeast .")
	    ("H" (:control) "fire :west .")
	    ("L" (:control) "fire :east .")
	    ("B" (:control) "fire :southwest .")
	    ("J" (:control) "fire :south .")
	    ("N" (:control) "fire :southeast .")
	    ;;
	    ("W" nil "wait .")
	    ("SPACE" nil "wait .")
	    ("PERIOD" (:control) "restart .")
	    ("KP-ENTER" nil "enter .")
	    ("RETURN" nil "enter .")
	    ("ESCAPE" (:control) "show-location .")
	    ("Q" (:control) "quit ."))))
  
(defparameter *alternate-qwerty-keybindings*
  (append *basic-keybindings*
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
	    ("Q" (:control) "fire :northwest .")
	    ("W" (:control) "fire :north .")
	    ("E" (:control) "fire :northeast .")
	    ("A" (:control) "fire :west .")
	    ("D" (:control) "fire :east .")
	    ("Z" (:control) "fire :southwest .")
	    ("X" (:control) "fire :south .")
	    ("C" (:control) "fire :southeast .")
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
  (append *basic-keybindings*
	  '(("G" nil "move :northwest .")
	    ("C" nil "move :north .")
	    ("R" nil "move :northeast .")
	    ("H" nil "move :west .")
	    ("N" nil "move :east .")
	    ("M" nil "move :southwest .")
	    ("W" nil "move :south .")
	    ("V" nil "move :southeast .")
	    ;;
	    ("G" (:alt) "fire :northwest .")
	    ("C" (:alt) "fire :north .")
	    ("R" (:alt) "fire :northeast .")
	    ("H" (:alt) "fire :west .")
	    ("N" (:alt) "fire :east .")
	    ("M" (:alt) "fire :southwest .")
	    ("W" (:alt) "fire :south .")
	    ("V" (:alt) "fire :southeast .")
	    ;;
	    ("G" (:meta) "fire :northwest .")
	    ("C" (:meta) "fire :north .")
	    ("R" (:meta) "fire :northeast .")
	    ("H" (:meta) "fire :west .")
	    ("N" (:meta) "fire :east .")
	    ("M" (:meta) "fire :southwest .")
	    ("W" (:meta) "fire :south .")
	    ("V" (:meta) "fire :southeast .")
	    ;;
	    ("G" (:control) "fire :northwest .")
	    ("C" (:control) "fire :north .")
	    ("R" (:control) "fire :northeast .")
	    ("H" (:control) "fire :west .")
	    ("N" (:control) "fire :east .")
	    ("M" (:control) "fire :southwest .")
	    ("W" (:control) "fire :south .")
	    ("V" (:control) "fire :southeast .")
	    ;;
	    ("S" nil "wait .")
	    ("SPACE" nil "wait .")
	    ("KP-ENTER" nil "enter .")
	    ("RETURN" nil "enter .")
	    ("ESCAPE" (:control) "show-location .")
	    ("PERIOD" (:control) "restart .")
	    ("Q" (:control) "quit ."))))

(define-method install-keybindings xiotank-prompt ()
  (let ((keys (ecase xe2:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:alternate-qwerty *alternate-qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k))))
  ;; ;; we also want to respond to timer events. this is how. 
  ;; [define-key self nil '(:timer) (lambda ()
  ;; 				   [run-cpu-phase *world* :timer])])

;;; Main program. 

(defun generate-level-address (ignore)
  '(=blue-world=))

(defparameter *xiotank-window-width* 800)
(defparameter *xiotank-window-height* 600)

(defvar *viewport*)

(defun xiotank ()
  (xe2:message "Initializing Xiotank...")
  (setf xe2:*window-title* "Xiotank")
  (setf clon:*send-parent-depth* 2) 
  (xe2:set-screen-height *xiotank-window-height*)
  (xe2:set-screen-width *xiotank-window-width*)
  ;; go!
  (let* ((prompt (clone =xiotank-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (player (clone =tank=))
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
    [resize splash :height (- *xiotank-window-height* 20) :width *xiotank-window-width*]
    [move splash :x 0 :y 0]
    [resize splash-prompt :width 10 :height 10]
    [move splash-prompt :x 0 :y 0]
    [hide splash-prompt]
    [set-receiver splash-prompt splash]
    ;;
    [resize *status* :height 20 :width *xiotank-window-width*]
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
	       ;; (xe2:enable-timer)
	       ;; (xe2:set-frame-rate 30)
	       ;; (xe2:set-timer-interval 1)

	       (xe2:enable-held-keys 1 3)
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
	       [resize viewport :height 470 :width *xiotank-window-width*]
	       [move viewport :x 0 :y 0]
	       [set-origin viewport :x 0 :y 0 
			   :height (truncate (/ (- *xiotank-window-height* 130) 16))
			   :width (truncate (/ *xiotank-window-width* 16))]
	       [adjust viewport]))
      (setf *space-bar-function* #'spacebar))
    ;;
    ;; [resize help :height 540 :width 800] 
    ;; [move help :x 0 :y 0]
    ;; (let ((text	(find-resource-object "help-message")))
    ;;   (dolist (line text)
    ;; 	(dolist (string line)
    ;; 	  (funcall #'send nil :print-formatted-string help string))
    ;; 	[newline help]))
    ;; ;;
    ;; [resize quickhelp :height 85 :width 250] 
    ;; [move quickhelp :y (- *xiotank-window-height* 130) :x (- *xiotank-window-width* 250)]
    ;; (let ((text	(find-resource-object "quickhelp-message")))
    ;;   (dolist (line text)
    ;; 	(dolist (string line)
    ;; 	  (funcall #'send nil :print-formatted-string quickhelp string))
    ;; 	[newline quickhelp]))
    ;; ;;
    ;;(play-music "xiotank-theme" :loop t)
    (set-music-volume 255)	       
    ;;
    [resize stack :width *xiotank-window-width* :height (- *xiotank-window-height* 20)]
    [move stack :x 0 :y 0]
    [set-children stack (list viewport terminal status)]
    ;;
    [resize terminal :height 80 :width *xiotank-window-width*]
    [move terminal :x 0 :y (- *xiotank-window-height* 80)]
    [set-verbosity terminal 0]
    ;;
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    (xe2:install-widgets splash-prompt splash)
    [add-page *pager* :game prompt stack viewport terminal *status* quickhelp]
    [add-page *pager* :help help]))

(xiotank)


;;; xiotank.lisp ends here

