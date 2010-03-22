;;; cons.lisp --- a game about lisp

;; Copyright (C) 2010  David O'Toole

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

(defpackage :cons-game
  (:documentation "CONS is an alternate universe sci-fi shooter game.")
  (:use :xe2 :common-lisp)
  (:export cons-game))

(in-package :cons-game)

(setf xe2:*dt* 10)

;;; Text labels

(defcell label 
  (categories :initform '(:drawn :actor :label))
  text stroke-color background-color timeout)

(define-method initialize label (&key text (stroke-color ".white") (background-color ".gray30")
					(style :label) (timeout nil) name tile description)
  (setf <text> text) 
  (when tile (setf <tile> tile))
  (when name (setf <name> name))
  (when description (setf <description> description))
  (setf <stroke-color> stroke-color)
  (setf <background-color> background-color)
  (setf <style> style)
  (setf <timeout> (if (floatp timeout)
		      ;; specify in (roughly) seconds if floating
		      (truncate (* 15 timeout)) ;; TODO fixme
		      ;; leave as frames if integer
		      timeout)))
  
(define-method draw label (x y image)
  (clon:with-field-values (text style) self
    (let* ((offset (ecase style
		     (:label 16)
		     (:flat 0)))
	   (x0 x)
	   (y0 y)
	   (x1 (+ x0 offset))
	   (y1 y0)
	   (margin 4)
	   (height (+ (* 2 margin) (apply #'+ (mapcar #'formatted-line-height text))))
	   (width (+ (* 2 margin) (apply #'max (mapcar #'formatted-line-width text)))))
      (draw-box x1 y1 width height 
		:stroke-color <stroke-color>
		:color <background-color>
		:destination image)
      ;; (when (eq style :label)
      ;; 	(draw-line x0 y0 x1 y1 :destination image))
      (let ((x2 (+ margin x1))
	    (y2 (+ margin y1)))
	(dolist (line text)
	  (render-formatted-line line x2 y2 :destination image)
	  (incf y2 (formatted-line-height line)))))))

(define-method run label ()
  [expend-default-action-points self]
  (when (integerp <timeout>)
    (when (minusp (decf <timeout>))
      [die self])))

;;; Agent: the player

(defcell agent 
  (tile :initform "agent-north")
  (description :initform "You are a sentient warrior cons cell.")
  (car :initform nil)
  (cdr :initform nil)
  (dead :initform nil)
  (last-turn-moved :initform 0)
  (team :initform :player)
  (hit-points :initform (make-stat :base 20 :min 0 :max 20))
  (movement-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 10 :min 0 :max 25))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 15)
  (movement-cost :initform (make-stat :base 10))
  (stepping :initform t)
  (direction :initform :north)
  (light-radius :initform 7)
  (categories :initform '(:actor :obstacle :player :target :container :light-source)))

(define-method loadout agent () nil)

(define-method hit agent (&optional object)
  [play-sample self "ouch"]
  [parent>>damage self 1])

(define-method pause agent ()
  [pause *world*])

(defparameter *agent-tiles* '(:north "agent-north"
			     :south "agent-south"
			     :east "agent-east"
			     :west "agent-west"))

(define-method aim agent (direction)
  (setf <direction> direction)
  (setf <tile> (getf *agent-tiles* direction)))

(define-method move agent (&optional direction)
  (unless <dead>
    (let ((phase (field-value :phase-number *world*))
	  (dir (or direction <direction>)))
      (unless (= <last-turn-moved> phase)
	(setf <last-turn-moved> phase)
	[aim self dir]
	[parent>>move self dir]))))

(define-method push agent () nil)
(define-method pop agent () nil)
(define-method rotate agent () nil)

(define-method run agent () nil)
  
(define-method quit agent ()
  (xe2:quit :shutdown))

(define-method die agent ()
  (unless <dead>
    (setf <tile> "agent-disabled")
    [play-sample self "gameover"]
    [say self "You died. Press escape to reset."]
    (setf <dead> t)))

(define-method restart agent ()
  (let ((agent (clone =agent=)))
    [say self "Restarting CONS..."]
    (halt-sample t)
    [destroy *universe*]
    [set-player *universe* agent]
    [set-character *status* agent]
    [play *universe*
	  :address '(=highway=)]
    [loadout agent]))

;;; Green level

(defcell road
  (description :initform "Security vehicle transit area.")
  (tile :initform "greenworld"))

(defcell barrier
  (description :initform "Impenetrable barrier.")
  (tile :initform "darkgreenworld")
  (categories :initform '(:obstacle)))

(define-prototype highway (:parent xe2:=world=)
  gen-row gen-column 
  ;;
  (description :initform "You enter a long corridor.")
  (level :initform 1)
  ;;
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (scale :initform '(3 m))
  (edge-condition :initform :block))

(define-method generate highway (&key (height 80)
					    (width 50))
  (setf *notes* nil)
  (setf <height> height <width> width)
  [create-default-grid self]
  (labels ((drop-barrier (r c)
	     (prog1 nil
	       [drop-cell self (clone =barrier=) r c])))
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =road=)
		 i j]))
    (dotimes (i 25)
      (let ((draw-function (if (= 0 (random 3))
			       #'trace-row #'trace-column)))
	(funcall draw-function #'drop-barrier
		 (+ 10 (random 50))
		 (+ 10 (random 50))
		 (+ 10 (random 50)))))
    [drop-cell self (clone =launchpad=) 10 10]))

(define-method begin-ambient-loop highway ()
  (play-music "beatup" :loop t))

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

;;; Key bindings

(defparameter *numpad-keybindings* 
  '(("KP8" nil "aim :north .")
    ("KP4" nil "aim :west .")
    ("KP6" nil "aim :east .")
    ("KP2" nil "aim :south .")
    ;; arrows
    ("UP" nil "aim :north .")
    ("LEFT" nil "aim :west .")
    ("RIGHT" nil "aim :east .")
    ("DOWN" nil "aim :south .")))

(defparameter *qwerty-keybindings*
  (append *numpad-keybindings*
	  '(("K" nil "aim :north .")
	    ("H" nil "aim :west .")
	    ("L" nil "aim :east .")
	    ("J" nil "aim :south .")
	    ;;
	    ("Z" nil "push .")
	    ("X" nil "pop .")
	    ("C" nil "call .")
	    ("V" nil "rotate .")
	    ("SPACE" nil "move .")
	    ;;
	    ("P" (:control) "pause .")
	    ("PAUSE" nil "pause .")
	    ("ESCAPE" nil "restart .")
	    ("Q" (:control) "quit ."))))
  
(define-prototype cons-prompt (:parent xe2:=prompt=))

(define-method install-keybindings cons-prompt ()
  (dolist (k *qwerty-keybindings*)
      (apply #'bind-key-to-prompt-insertion self k)))

(define-method install-keybindings cons-prompt ()
  (let ((keys (ecase xe2:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:alternate-qwerty *alternate-qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k))))

;;; Custom formatter.

(define-prototype cons-formatter (:parent xe2:=formatter=))

(define-method render cons-formatter ()
  [pause *world* :always]
  [parent>>render self])

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
	[print-stat self :hit-points :warn-below 7 :show-max t]
	[print-stat-bar self :hit-points :color ".red"]
	[space self]
	[newline self])))

;;; Custom bordered viewport

(define-prototype view (:parent xe2:=viewport=))

(defvar *viewport*)

(define-method render view ()
  [parent>>render self]
  (xe2:draw-rectangle 0 0 
		      <width>
		      <height>
		      :color ".blue" :destination <image>))


;;; Joystick screen.

(defvar *form*)
  
(define-prototype joystick-world (:parent =world=)
  (height :initform 12)
  (width :initform 16))

(define-method generate joystick-world ()
  [create-default-grid self]
  (dotimes (n 5)
    (let ((data-cell (clone =data-cell=)))
      [drop-cell self data-cell (random <height>) (random <width>)]
      [set data-cell (random 10)]))
  (dotimes (n 6)
    (let ((var-cell (clone =var-cell= (car (one-of '(:foo :bar :baz))))))
      [drop-cell self var-cell (random <height>) (random <width>)]))
  (dotimes (n 3)
    (let ((event-cell (clone =event-cell=)))
      [drop-cell self event-cell (random <height>) (random <width>)])))



;;; Main program. 

(defparameter *cons-window-width* 800)
(defparameter *cons-window-height* 600)

(defun cons-game ()
  (xe2:message "Initializing CONS...")
  (setf xe2:*window-title* "CONS")
  (clon:initialize)
  (xe2:set-screen-height *cons-window-height*)
  (xe2:set-screen-width *cons-window-width*)
  (let* ((prompt (clone =cons-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (player (clone =agent=))
	 (splash (clone =splash=))
	 (help (clone =cons-formatter=))
	 (quickhelp (clone =formatter=))
	 (form (clone =form=))
	 (viewport (clone =view=))
	 (status (clone =status=))
	 (form-prompt (clone =prompt=))
	 (splash-prompt (clone =splash-prompt=))
	 (terminal (clone =narrator=))
	 (stack (clone =stack=)))
    ;;
    (setf *form* form)
    (setf *viewport* viewport)
    (setf *status* status)
    ;;
    [resize splash :height (- *cons-window-height* 20) :width *cons-window-width*]
    [move splash :x 0 :y 0]
    [resize splash-prompt :width 10 :height 10]
    [move splash-prompt :x 0 :y 0]
    [hide splash-prompt]
    [set-receiver splash-prompt splash]
    ;;
    [resize *status* :height 20 :width *cons-window-width*]
    [move *status* :x 0 :y 0]
    ;;
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    ;; 
    [resize form :height 500 :width 800]
    [move form :x 0 :y 0]
    ;;[show form]
    ;;
    [resize form-prompt :height 20 :width *cons-window-width*]
    [move form-prompt :x 0 :y (- *cons-window-height* 20)]
    [show form-prompt]
    [install-keybindings form-prompt]
    [set-receiver form-prompt form]
    ;;
    (labels ((spacebar ()
	       ;;
	       (xe2:halt-music 1000)
	       (setf xe2:*physics-function* #'(lambda (&rest ignore)
						(when *world* [run-cpu-phase *world* :timer])))
	       (xe2:enable-held-keys 1 3)
	       ;;
	       [set-player universe player]
	       [play universe
	       	     :address '(=highway=)
	       	     :prompt prompt
	       	     :narrator terminal
	       	     :viewport viewport]
	       [loadout player]
	       (let ((config-screen (clone =joystick-world=)))
		 [generate config-screen]
		 [configure form config-screen])
	       ;;
	       [set-character *status* player]
	       ;;
	       [set-tile-size viewport 16]
	       [resize viewport :height 470 :width *cons-window-width*]
	       [move viewport :x 0 :y 0]
	       [set-origin viewport :x 0 :y 0 
			   :height (truncate (/ (- *cons-window-height* 130) 16))
			   :width (truncate (/ *cons-window-width* 16))]
	       [adjust viewport]))
      (setf *space-bar-function* #'spacebar))
    
    [resize help :height 540 :width 800] 
    [move help :x 0 :y 0]
    (let ((text	(find-resource-object "help-message")))
      (dolist (line text)
    	(dolist (string line)
    	  (funcall #'send nil :print-formatted-string help string))
    	[newline help]))
    ;;
    [resize quickhelp :height 85 :width 250] 
    [move quickhelp :y (- *cons-window-height* 130) :x (- *cons-window-width* 250)]
    (let ((text	(find-resource-object "quickhelp-message")))
      (dolist (line text)
    	(dolist (string line)
    	  (funcall #'send nil :print-formatted-string quickhelp string))
    	[newline quickhelp]))
    ;;
    (play-music "cons" :loop t)
    (set-music-volume 255)	       
    ;;
    [resize stack :width *cons-window-width* :height (- *cons-window-height* 20)]
    [move stack :x 0 :y 0]
    [set-children stack (list viewport terminal status)]
    ;;
    [resize terminal :height 80 :width *cons-window-width*]
    [move terminal :x 0 :y (- *cons-window-height* 80)]
    [set-verbosity terminal 0]
    ;;
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    (xe2:install-widgets splash-prompt splash)
    [add-page *pager* :testing form-prompt form]
    [add-page *pager* :game prompt stack viewport terminal quickhelp *status* ]
    [add-page *pager* :help help]
))


(cons-game)



(provide 'cons)
;;; cons.lisp ends here
