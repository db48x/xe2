;;; blast.lisp --- a micro anti-shmup in common lisp

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

;; Blast Tactics is a mini space-roguelike puzzler incorporating
;; elements of the classic Asteroids, plus gravity and a unique weapon
;; system. Shoot asteroids and enemies, or sweep your trail across
;; them. Powerups extend your trail's length and enable higher scores.

;;; Packaging

(defpackage :blast
  (:documentation "Blast Tactics: A sci-fi roguelike for Common Lisp.")
  (:use :rlx :common-lisp)
  (:export blast))

(in-package :blast)

;;; Custom bordered viewport

(define-prototype view (:parent rlx:=viewport=))

(define-method render view ()
  [parent>>render self]
  (rlx:draw-rectangle 0 0 
		      <width>
		      <height>
		      :color ".blue" :destination <image>))

(defvar *view* (clone =view=))

;;; Controlling the game.

(define-prototype blast-prompt (:parent rlx:=prompt=))

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
    ("KP7" (:control) "fire :northwest .")
    ("KP8" (:control) "fire :north .")
    ("KP9" (:control) "fire :northeast .")
    ("KP4" (:control) "fire :west .")
    ("KP6" (:control) "fire :east .")
    ("KP1" (:control) "fire :southwest .")
    ("KP2" (:control) "fire :south .")
    ("KP3" (:control) "fire :southeast .")
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
;;	    ("SPACE" nil "respawn .")
	    ("PERIOD" (:control) "restart .")
	    ("KP-ENTER" nil "enter .")
	    ("RETURN" nil "enter .")
	    ("ESCAPE" (:control) "show-location .")
	    ("3" nil "activate-extension .")
	    ("2" nil "activate-pulse-cannon .")
	    ("1" nil "activate-bomb-cannon .")
	    ("P" (:control) "disembark .")
	    ("P" nil "embark .")
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
;;	    ("SPACE" nil "respawn .")
	    ("PERIOD" (:control) "restart .")
	    ("3" nil "activate-extension .")
	    ("2" nil "activate-pulse-cannon .")
	    ("1" nil "activate-bomb-cannon .")
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
;;	    ("SPACE" nil "respawn .")
	    ("KP-ENTER" nil "enter .")
	    ("RETURN" nil "enter .")
	    ("ESCAPE" (:control) "show-location .")
	    ("PERIOD" (:control) "restart .")
	    ("3" nil "activate-extension .")
	    ("2" nil "activate-pulse-cannon .")
	    ("1" nil "activate-bomb-cannon .")
	    ("P" (:control) "disembark .")
	    ("P" nil "embark .")
	    ("Q" (:control) "quit ."))))

(define-method install-keybindings blast-prompt ()
  (let ((keys (ecase rlx:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:alternate-qwerty *alternate-qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k)))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *active-world* :timer])])

;;; Status widgets for ship and dude

(defvar *ship-status* nil)
(defvar *dude-status* nil)

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
  (let* ((char <character>))
    (when char
      (let ((hits [stat-value char :hit-points])
	    (energy [stat-value char :energy]))
	[println self "VEHICLE STATUS:" :foreground ".white" :background ".blue"]
	[print-stat self :hit-points :warn-below 10]
	[print-stat-bar self :hit-points :color ".red"]
	[newline self]
	[print-stat self :energy :warn-below 10]
	[print-stat-bar self :energy :color ".cyan"]
	[newline self]
	[print-stat self :bomb-ammo :warn-below 2]
	[print-stat-bar self :bomb-ammo :color ".green"]
	[space self]
	[print-stat self :oxygen :warn-below 50]
	[print self " "]
	[print-stat self :technetium]
	[print self " "]
	[print-stat self :endurium :warn-below 10]
	[print self " "]
	[newline self]
	[print-stat self :strength :warn-below 10]
	[print self " "]
	[print-stat self :defense :warn-below 10]
	[print self " "]
	[print-stat self :speed :warn-below 5]
	[newline self]))
    [print self "LOCATION: "]
    [print self (format nil "[~A]" [location-name *active-world*])]
    [print self " SCALE:"]
    (destructuring-bind (num unit) (field-value :scale *active-world*)
      [print self (format nil "[~A ~A]" num unit)])
    [space self]
    [print self " COORDINATES:"]
    [print self (format nil "[~A ~A] " [player-row *active-world*]
			[player-column *active-world*])]
    [println self (format nil " MODES: ~A" (field-value :required-modes *active-world*))]
    [print self "TERRAIN: "]
    (let ((player [get-player *active-world*]))
      (when (and player [is-located player])
	(do-cells (cell [cells-at *active-world* 
				  (field-value :row player)
				  (field-value :column player)])
	  (unless [is-player cell]
	    [print-object-tag self cell])))
      [newline self])))

(define-prototype dude-status (:parent =status=))

(define-method update dude-status ()
  [delete-all-lines self]
  (assert <character>)
  (when <character>
    (let* ((char <character>)
	   (hits [stat-value char :hit-points])
	   (energy [stat-value char :energy]))
      [println self "CONTRACTOR STATUS:" :foreground ".white" :background ".blue"]
      [print-stat self :hit-points :warn-below 10]
      [print-stat-bar self :hit-points :color ".red"]
      [newline self]
      ;; energy display
      [print-stat self :energy :warn-below 10]
      [print-stat-bar self :energy :color ".cyan"]
      [newline self]
      [print-stat self :oxygen :warn-below 50]
      [print self " "]
      [print-stat self :strength :warn-below 10]
      [print self " "]
      [print-stat self :defense :warn-below 10]
      [print self " "]
      [print-stat self :speed :warn-below 5]
      [print self " "]
      [newline self])))
    
      
;;; Splash screen
  
(defvar *pager* nil)

(define-prototype splash (:parent =widget=))

(define-method render splash ()
  (rlx:draw-resource-image "splash" 0 0 
			   :destination <image>))

(defvar *space-bar-function*)

(define-method dismiss splash ()
  (play-sample "go")
  [select *pager* :play]
  (when (functionp *space-bar-function*)
    (funcall *space-bar-function*))
  ;; TODO ugh this is a hack!
  (rlx:show-widgets))

(define-prototype splash-prompt (:parent =prompt=)
  (default-keybindings :initform '(("SPACE" nil "dismiss ."))))

;;; Main program. 

(defparameter *blast-window-width* 1180)
(defparameter *blast-window-height* 600)

(defparameter *right-column-width* 550)
(defparameter *left-column-width* (- *blast-window-width* 
				     *right-column-width*))


(defun blast ()
  (rlx:message "Initializing Blast Tactics...")
  (setf clon:*send-parent-depth* 2) 
  (rlx:set-screen-height *blast-window-height*)
  (rlx:set-screen-width *blast-window-width*)
  ;; (rlx:set-frame-rate 30)
  ;; (rlx:set-timer-interval 20)
  ;; (rlx:enable-timer)
  (rlx:enable-held-keys 1 15)
  (setf *asteroid-count* 0)
  (setf *level* 0)
  (let* ((prompt (clone =blast-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (ship-status (clone =status=))
	 (dude-status (clone =dude-status=))
	 (minimap (clone =minimap=))
	 (ship (clone =olvac=))
	 (dude (clone =contractor=))
	 (splash (clone =splash=))
	 (splash-prompt (clone =splash-prompt=))
	 (textbox (clone =textbox=))
	 (terminal (clone =narrator=))
	 (stack (clone =stack=))
	 (stack2 (clone =stack=)))
    ;;
    (setf *view* (clone =view=))
    ;;
    [resize splash :height 580 :width *blast-window-width*]
    [move splash :x 200 :y 0]
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
    [resize narrator :height 100 :width *left-column-width*]
    [set-verbosity narrator 0]
    ;;
    (labels ((spacebar ()
	       (setf *ship-status* ship-status)
	       (setf *dude-status* dude-status)
	       ;;
	       [resize ship-status :height 105 :width *blast-window-width*]
	       [set-character ship-status ship]
	       [move ship-status :x 0 :y 0]
	       ;;
	       [resize dude-status :height 160 :width 500]
	       [set-character dude-status dude]
	       [move dude-status :x 0 :y 400]
	       ;;
	       [set-player universe ship]
	       [play universe
		     :address '(=star-sector= :width 80 :height 80 
				:stars 80 :freighters 6 :sequence-number (genseq))
		     :prompt prompt
		     :narrator terminal
		     :viewport *view*]
	       [proxy ship dude]
	       [loadout dude]
	       [loadout ship]
	       ;;
	       [update ship-status]
	       [update dude-status]
	       [set-tile-size *view* 16]
	       ;; the default is to track the current world:
	       ;; [set-world *view* world] 
	       [resize *view* :height 432 :width *left-column-width*]
	       [move *view* :x 0 :y 0]
	       [set-origin *view* :x 0 :y 0 :height 24 :width (truncate (/ *left-column-width*
									   16))]
	       [adjust *view*]
	       ;;    [set-tile-size minimap 1]
	       [resize minimap :height 80 :width 120]
	       [move minimap :x 500 :y 490]
	       [set-origin minimap :x 0 :y 0 :height 100 :width 120]
	       [adjust minimap]))
      (setf *space-bar-function* #'spacebar))
    ;;
    [set-buffer textbox
		(find-resource-object "help-message")]
    [resize-to-fit textbox] 
    [move textbox :x 0 :y 0]
    ;;
    (play-music "theme" :loop t)
    (set-music-volume 255)	       
    ;;
    [resize stack :width *left-column-width* :height 580]
    [move stack :x 0 :y 0]
    [set-children stack (list ship-status *view*)]
    ;;
    [resize terminal :height (- *blast-window-height* 100) :width *right-column-width*]
    [move terminal :x *left-column-width* :y 100]
    [set-verbosity terminal 0]
    ;; [move stack2 :x *left-column-width* :y 0]
    ;; [resize stack2 :width *right-column-width* :height 580]
    ;; [set-children stack2 (list terminal)]
    ;;
    ;; HACK
    ;; (labels ((light-hack (sr sc r c &optional (color ".white"))
    ;; 	       (labels ((hack-overlay (image)
    ;; 			  (multiple-value-bind (sx sy)
    ;; 			      [get-screen-coordinates *view* sr sc]
    ;; 			    (multiple-value-bind (x y)
    ;; 				[get-screen-coordinates *view* r c]
    ;; 			      (draw-line x y sx sy :destination image
    ;; 					 :color color)))))
    ;; 		 [add-overlay *view* #'hack-overlay])))
    ;;   (setf rlx::*lighting-hack-function* #'light-hack))
    ;;
    (setf *pager* (clone =pager=))
    [auto-position *pager*]; :width *left-column-width*]
    (rlx:install-widgets splash-prompt splash)
    [add-page *pager* :play stack prompt dude-status ship-status *view* minimap terminal]
    [add-page *pager* :help textbox]))

