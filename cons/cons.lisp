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

(in-package :cons-game)

(setf xe2:*dt* 20)

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

;;; Custom formatter pauses when shown; it's the help screen

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
  
(defparameter *default-commands*
  '(("aim :north ." "UP")
    ("aim :south ." "DOWN")
    ("aim :east ." "RIGHT")
    ("aim :west ." "LEFT")
    ("push ." "Z")
    ("pop ." "X")
    ("call ." "C")
    ("rotate ." "V")
    ("move ." "SPACE")
    ("quit ." "Q" :control)))

(define-prototype joystick-world (:parent =world=)
  (height :initform 18)
  (width :initform 4)
  (prompt :initform nil))

(define-method set-prompt joystick-world (prompt)
  (setf <prompt> prompt))

(define-method configure-keybindings joystick-world ()
  (clon:with-field-values (prompt variables) self
    (assert (hash-table-p variables))
    [clear-keymap prompt]
    (labels ((install (command event)
	       (destructuring-bind (key &rest modifiers) event
		 (message "Installing ~S" event)
		 (bind-key-to-prompt-insertion prompt key modifiers command))))
      (message "Configuring ~S keybindings." (hash-table-count variables))
      (maphash #'install variables))))

(define-method generate joystick-world ()
  [create-default-grid self]
  ;; todo write 
  (let ((row 1))
    (labels ((drop-config-row (command event)
	       (let ((event-cell (clone =event-cell=))
		     (var-cell (clone =var-cell= command)))
		 [drop-cell self event-cell row 1]
		 [set event-cell event]
		 [drop-cell self var-cell row 2])
	       (incf row)))
      (let ((c1 (clone =comment-cell= "Keypress"))
	    (c2 (clone =comment-cell= "Resulting action")))
	[drop-cell self c1 row 1]
	[drop-cell self c2 row 2]
	(incf row))
      (dolist (command *default-commands*)
	(destructuring-bind (command-string &rest event) command
	  (drop-config-row command-string event)))
      (incf row)
      [drop-cell self (clone =button-cell= 
			     :closure #'(lambda () 
					  [configure-keybindings self])
			     :text "  APPLY  ")
		 row 2])))
			
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
						(when *world* [run-cpu-phase *world* :timer]
						      ;; (maphash #'(lambda (&rest args)
						      ;; 		   (message "VAR: ~S" args))
						      ;; 	       (field-value :variables *world*))
						      )))
	       ;; (xe2:enable-held-keys 1 3)
	       ;; (xe2:enable-held-keys 1 3)
 	       ;;
	       [set-player universe player]
	       [play universe
	       	     :address '(=storage=)
	       	     :prompt prompt
	       	     :narrator terminal
	       	     :viewport viewport]
	       [loadout player]
	       (let ((config-screen (clone =joystick-world=)))
		 [generate config-screen]
		 [set-prompt config-screen prompt]
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
    (play-music "reprise" :loop t)
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
    [add-page *pager* :config (list form)]
    [add-page *pager* :game (list prompt stack viewport terminal quickhelp *status*)]
    (xe2:enable-classic-key-repeat 100 100)
    ;; [set-page-property *pager* :game :held-keys t]
    [add-page *pager* :help (list help)]
))


(cons-game)



(provide 'cons)
;;; cons.lisp ends here
