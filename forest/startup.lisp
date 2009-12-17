;;; startup.lisp --- Sanctuary startup file

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

(in-package :forest)

;;; Custom bordered viewport

(define-prototype view (:parent xe2:=viewport=))

(define-method render view ()
  [parent>>render self]
  (xe2:draw-rectangle 0 0 
		      <width>
		      <height>
		      :color ".blue" :destination <image>)
  [message *pager* (list "")])

(defvar *view* (clone =view=))

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
    ("KP8" (:shift) "fire :north .")
    ("KP4" (:shift) "fire :west .")
    ("KP6" (:shift) "fire :east .")
    ("KP2" (:shift) "fire :south .")
    ("KP7" (:shift) "fire :northwest .")
    ("KP9" (:shift) "fire :northeast .")
    ("KP1" (:shift) "fire :southwest .")
    ("KP3" (:shift) "fire :southeast .")
    	    ;;
    ("1" nil "use-item 0 .")
    ("2" nil "use-item 1 .")
    ("1" (:control) "drop-item 0 .")
    ("2" (:control) "drop-item 1 .")
    ("3" nil "use-item 2 .")
    ("4" nil "use-item 3 .")
    ("3" (:control) "drop-item 2 .")
    ("4" (:control) "drop-item 3 .")
    ;;
    ("P" (:control) "pause .")
    ("C" (:control) "camp .")
    ("E" (:control) "eat .")
    ("PAUSE" nil "pause .")
    ("ESCAPE" nil "restart .")
    ("RETURN" nil "enter .")
    ;;
    ("Q" (:control) "quit .")))

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
	    ("G" (:shift) "fire :northwest .")
	    ("C" (:shift) "fire :north .")
	    ("R" (:shift) "fire :northeast .")
	    ("H" (:shift) "fire :west .")
	    ("N" (:shift) "fire :east .")
	    ("M" (:shift) "fire :southwest .")
	    ("W" (:shift) "fire :south .")
	    ("V" (:shift) "fire :southeast ."))))

(defparameter *qwertz-keybindings*
  (append *numpad-keybindings*
	  '(("Q" nil "move :northwest .")
	    ("W" nil "move :north .")
	    ("E" nil "move :northeast .")
	    ("A" nil "move :west .")
	    ("D" nil "move :east .")
	    ("Y" nil "move :southwest .")
	    ("X" nil "move :south .")
	    ("C" nil "move :southeast .")
	    ;;
	    ("Q" (:shift) "fire :northwest .")
	    ("W" (:shift) "fire :north .")
	    ("E" (:shift) "fire :northeast .")
	    ("A" (:shift) "fire :west .")
	    ("D" (:shift) "fire :east .")
	    ("Y" (:shift) "fire :southwest .")
	    ("X" (:shift) "fire :south .")
	    ("C" (:shift) "fire :southeast ."))))

(defparameter *qwerty-keybindings*
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
	    ("Q" (:shift) "fire :northwest .")
	    ("W" (:shift) "fire :north .")
	    ("E" (:shift) "fire :northeast .")
	    ("A" (:shift) "fire :west .")
	    ("D" (:shift) "fire :east .")
	    ("Z" (:shift) "fire :southwest .")
	    ("X" (:shift) "fire :south .")
	    ("C" (:shift) "fire :southeast ."))))

(defparameter *roguelike-keybindings*
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
	    ("Y" (:shift) "fire :northwest .")
	    ("K" (:shift) "fire :north .")
	    ("U" (:shift) "fire :northeast .")
	    ("H" (:shift) "fire :west .")
	    ("L" (:shift) "fire :east .")
	    ("B" (:shift) "fire :southwest .")
	    ("J" (:shift) "fire :south .")
	    ("N" (:shift) "fire :southeast ."))))

(define-method install-keybindings room-prompt ()
  (dolist (k (append *numpad-keybindings* (ecase *user-keyboard-layout*
					    (:qwerty *qwerty-keybindings*)
					    (:qwertz *qwertz-keybindings*)
					    (:dvorak *dvorak-keybindings*)
					    (:roguelike *roguelike-keybindings*))))
    (apply #'bind-key-to-prompt-insertion self k))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *world* :timer])])

;;; A character status widget.

(define-prototype status (:parent xe2:=formatter=)
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
		       ".gray20")))
	[print self (symbol-name stat-name)
	       :foreground ".white"]
	[print self " "]
	[print self (format nil "~S" value) 
	       :foreground ".yellow"
	       :background color]
	[print self " "]))))

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

(define-method print-inventory-slot status (slot-number &key show-as)
  [print self (format nil "[~D]: " (or show-as slot-number))]
  (let ((item [item-at <character> slot-number]))
    (if item
	(clon:with-field-values (name tile) item
				[print self nil :image tile]
				[print self " "]
				[print self (get-some-object-name item)]
				[print self "  "])
	[print self "EMPTY  "])))

(define-method update status ()
  [delete-all-lines self]
  (let ((char <character>))
    [print self "  Statistics:  "]
    [print-stat self :hit-points :warn-below 12]
    [print-stat-bar self :hit-points :color ".red" :background-color ".gray30"]
    [print self " "]
    [print-stat self :strength :warn-below 10]
    [print self " "]
    [print-stat self :defense :warn-below 10]
    [print self " "]
    [print-stat self :speed :warn-below 2]
    [print self " "]
    [newline self]
    ;;
    [print self "  Equipment:  "]
    [print-equipment-slot self :right-hand]
    [print-equipment-slot self :left-hand]
    [print self (format nil "  ARROWS: ~S " [stat-value char :arrows])]
    [print self nil :image "arrows"]
    [print self (format nil "  RATIONS: ~S " [stat-value char :rations])]
    [print self nil :image "ration"]
    [print self (format nil "  FIREWOOD: ~S " [stat-value char :firewood])]
    [print self nil :image "firewood-1"]
    [newline self]
    ;;
    [print self "  Inventory:  "]
    [print-inventory-slot self 0 :show-as 1]
    [print-inventory-slot self 1 :show-as 2]
    [print-inventory-slot self 2 :show-as 3]
    [print-inventory-slot self 3 :show-as 4]
    [newline self]
    ;; status ailments
    (when [in-category char :freezing]
      [print self "  Freezing " :foreground ".blue"])
    (when [in-category char :hungry]
      [print self "  Hungry " :foreground ".green"])
    (when [in-category char :starving]
      [print self "  Starving " :foreground ".red"])
    (when [in-category char :dying]
      [print self "  Dying " :foreground ".yellow" :background ".red"])
    (when [in-category char :reloading]
      [print self "  Reloading " :foreground ".black" :background ".yellow"])
    [print self " "]
    [newline self]))

;;; Pager and splash screen

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
  ;; TODO ugh this is a hack!
  (xe2:show-widgets))

(define-prototype splash-prompt (:parent =prompt=)
  (default-keybindings :initform '(("SPACE" nil "dismiss ."))))

;;; Help prompt and textbox

(define-prototype help-prompt (:parent =prompt=)
  (default-keybindings :initform '(("N" nil "page-down .")
				   ("P" nil "page-up ."))))

(define-prototype help-textbox (:parent =textbox=))

(define-method render help-textbox ()
  [parent>>render self]
  [message *pager* 
	   (list (format nil " --- Line ~A of ~A. Use N (NEXT) and P (PREVIOUS) to scroll the text." 
			 <point-row> (length <buffer>)))])


;;; Main program. 

(defparameter *pager-height* 16)

(defparameter *room-window-width* 800)
(defparameter *room-window-height* 600)

(defparameter *start-level* 3)

(defun init-forest ()
  (xe2:message "Initializing Forest...")
  (clon:initialize)
  (xe2:set-screen-height *room-window-height*)
  (xe2:set-screen-width *room-window-width*)
  (let* ((prompt (clone =room-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (help (clone =help-textbox=))
	 (quickhelp (clone =formatter=))
	 (splash (clone =splash=))
	 (splash-prompt (clone =splash-prompt=))
	 (help-prompt (clone =help-prompt=))
	 (player (clone =player=))
	 (status (clone =status=))
	 (viewport (clone =view=)))
    ;;
    [resize splash :height *room-window-height* :width *room-window-width*]
    [move splash :x 0 :y 0]
    [resize splash-prompt :width 10 :height 10]
    [move splash-prompt :x 0 :y 0]
    [hide splash-prompt]
    [set-receiver splash-prompt splash]
    ;;
    [resize help-prompt :width 10 :height 10]
    [move help-prompt :x 0 :y 0]
    [hide help-prompt]
    [set-receiver help-prompt help]
    ;;
    (labels ((spacebar ()
	       (setf *status* status)
	       [resize status :height 80 :width 800]
	       [move status :x 5 :y 0]
	       [set-character status player]
	       ;;
	       [resize prompt :height 20 :width 100]
	       [move prompt :x 0 :y 0]
	       [hide prompt]
	       [install-keybindings prompt]
	       ;;
	       [resize narrator :height 80 :width *room-window-width*]
	       [move narrator :x 0 :y (- *room-window-height* 100 *pager-height*)]
	       [set-verbosity narrator 0]
	       ;;
	       [resize quickhelp :height 85 :width 280] 
	       [move quickhelp :y (- *room-window-height* 100 *pager-height*) :x (- *room-window-width* 280)]
	       (let ((text	(find-resource-object "quickhelp-message")))
		 (dolist (line text)
		   (dolist (string line)
		     (funcall #'send nil :print-formatted-string quickhelp string))
		   [newline quickhelp]))
	       ;;
	       [play universe
		     :address (generate-level-address *start-level*)
		     :player player
		     :narrator narrator
		     :prompt prompt
		     :viewport viewport]
	       [set-tile-size viewport 16]
	       [resize viewport :height 450 :width *room-window-width*]
	       [move viewport :x 0 :y 80]
	       [set-origin viewport :x 0 :y 0 
			   :height (truncate (/ (- *room-window-height* 200) 16))
			   :width (truncate (/ *room-window-width* 16))]
	       [adjust viewport] 
	       [select *pager* 2]
	       [loadout player]))
      (setf *space-bar-function* #'spacebar))
    ;;
    [resize-to-scroll help :height 540 :width 800] 
    [move help :x 0 :y 0]
    (let ((text	(find-resource-object "help-message")))
      [set-buffer help text])
    ;;
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    (xe2:install-widgets splash-prompt splash)
    [add-page *pager* :play prompt viewport narrator status quickhelp]
    [add-page *pager* :help help-prompt help]))


(init-forest)

