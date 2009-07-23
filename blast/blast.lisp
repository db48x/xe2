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

;;; Billboard shows most recent attention data.

(defvar *billboard*)

(defvar *billboard-strings* '(:go ("GO!" :foreground ".black" :background ".yellow" 
				   :font "display-font")
			      :react ("REACT!" :foreground ".red" :background ".navy blue"
				      :font "display-font")
			      :extend ("EXTEND!" :foreground ".yellow" :background ".blue"
				       :font "display-font")
			      :pulse-ammo ("PULSE +2" :foreground ".red" :background ".black"
				       :font "display-font")
			      :bomb-ammo ("BOMB +2" :foreground ".red" :background ".black"
				       :font "display-font")
			      :shield ("SHIELD +1" :foreground ".cyan" :background ".blue"
				       :font "display-font")
			      :warning ("WARNING!" :foreground ".yellow" :background ".red"
					:font "display-font")
			      :hit ("HIT!" :foreground ".yellow" :background ".purple" 
				    :font "display-font")
			      :dead ("YOU DIE!" :foreground ".yellow" :background ".red"
				     :font "display-font")
			      :destroy ("DESTROY!" :foreground ".white" :background ".red"
					:font "display-font") 
			      :probe-kill ("DESTROY!" :foreground ".white" :background ".forestgreen"
					:font "display-font")
                             :sweep ("SWEEP!" :foreground ".yellow" :background ".forest green"
				      :font "display-font")))

(defun billboard-string (key)
  (getf *billboard-strings* key '("STANDBY")))

(define-prototype billboard (:parent rlx:=formatter=)
  (font :initform "display-font"))

(define-method say billboard (key)
  [delete-all-lines self]
  [print-formatted-string self (billboard-string key)]
  [newline self])

;;; Custom bordered viewport

(define-prototype view (:parent rlx:=viewport=))

(define-method render view ()
  [parent>>render self]
  (rlx:draw-rectangle 0 0 
		      <width>
		      <height>
		      :color ".blue" :destination <image>))

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
    ("KP3" (:control) "fire :southeast .")))

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
	    ;; ("Y" (:alt) "attack :northwest .")
	    ;; ("K" (:alt) "attack :north .")
	    ;; ("U" (:alt) "attack :northeast .")
	    ;; ("H" (:alt) "attack :west .")
	    ;; ("L" (:alt) "attack :east .")
	    ;; ("B" (:alt) "attack :southwest .")
	    ;; ("J" (:alt) "attack :south .")
	    ;; ("N" (:alt) "attack :southeast .")
	    ;; ;;
	    ;; ("Y" (:meta) "attack :northwest .")
	    ;; ("K" (:meta) "attack :north .")
	    ;; ("U" (:meta) "attack :northeast .")
	    ;; ("H" (:meta) "attack :west .")
	    ;; ("L" (:meta) "attack :east .")
	    ;; ("B" (:meta) "attack :southwest .")
	    ;; ("J" (:meta) "attack :south .")
	    ;; ("N" (:meta) "attack :southeast .")
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
	    ("RETURN" nil "enter .")
	    ("3" nil "activate-extension .")
	    ("2" nil "activate-pulse-cannon .")
	    ("1" nil "activate-bomb-cannon .")
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
	    ;; ("Q" (:alt) "attack :northwest .")
	    ;; ("W" (:alt) "attack :north .")
	    ;; ("E" (:alt) "attack :northeast .")
	    ;; ("A" (:alt) "attack :west .")
	    ;; ("D" (:alt) "attack :east .")
	    ;; ("Z" (:alt) "attack :southwest .")
	    ;; ("X" (:alt) "attack :south .")
	    ;; ("C" (:alt) "attack :southeast .")
	    ;; ;;
	    ;; ("Q" (:meta) "attack :northwest .")
	    ;; ("W" (:meta) "attack :north .")
	    ;; ("E" (:meta) "attack :northeast .")
	    ;; ("A" (:meta) "attack :west .")
	    ;; ("D" (:meta) "attack :east .")
	    ;; ("Z" (:meta) "attack :southwest .")
	    ;; ("X" (:meta) "attack :south .")
	    ;; ("C" (:meta) "attack :southeast .")
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
	    ("ESCAPE" nil "activate-pulse-cannon .")
;;	    ("SPACE" nil "respawn .")
	    ("3" nil "activate-extension .")
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
	    ;; ("G" (:alt) "attack :northwest .")
	    ;; ("C" (:alt) "attack :north .")
	    ;; ("R" (:alt) "attack :northeast .")
	    ;; ("H" (:alt) "attack :west .")
	    ;; ("N" (:alt) "attack :east .")
	    ;; ("M" (:alt) "attack :southwest .")
	    ;; ("W" (:alt) "attack :south .")
	    ;; ("V" (:alt) "attack :southeast .")
	    ;; ;;
	    ;; ("G" (:meta) "attack :northwest .")
	    ;; ("C" (:meta) "attack :north .")
	    ;; ("R" (:meta) "attack :northeast .")
	    ;; ("H" (:meta) "attack :west .")
	    ;; ("N" (:meta) "attack :east .")
	    ;; ("M" (:meta) "attack :southwest .")
	    ;; ("W" (:meta) "attack :south .")
	    ;; ("V" (:meta) "attack :southeast .")
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
	    ("ESCAPE" nil "activate-pulse-cannon .")
	    ("3" nil "activate-extension .")
	    ("1" nil "activate-bomb-cannon .")
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

;;; A shield status and score widget.

(define-prototype status (:parent rlx:=formatter=)
  (character :documentation "The character cell whose status is shown."))

(define-method set-character status (char)
  (setf <character> char))

(defparameter *status-bar-character* "-")

(define-method update status ()
  [delete-all-lines self]
  (let* ((char <character>)
	 (hits [stat-value char :hit-points])
	 (energy [stat-value char :energy])
	 (pulse-ammo [stat-value char :pulse-ammo])
	 (bomb-ammo [stat-value char :bomb-ammo]))
    [print self " HITS: "]
    (dotimes (i [stat-value char :hit-points :max])
      [print self *status-bar-character* 
	     :foreground ".yellow"
	     :background (if (< i hits)
			     ".red"
			     ".gray20")])
    [newline self]
    ;; energy display
    [print self " ENERGY: "]
    (dotimes (i 40)
      [print self *status-bar-character* 
	     :foreground ".red"
	     :background (if (< i energy)
			     ".cyan"
			     ".gray20")])
    [newline self]
    [print self " PULSE: "]
    (dotimes (i 6)
      [print self *status-bar-character* 
	     :foreground ".red"
	     :background (if (< i pulse-ammo)
			     ".yellow"
			     ".gray20")])
    [space self]
    [print self " BOMBS: "]
    (dotimes (i 6)
      [print self *status-bar-character* 
	     :foreground ".red"
	     :background (if (< i bomb-ammo)
			     ".green"
			     ".gray20")])
    [space self]
    [print self " DEPTH: "]
    [print self (format nil "~D" (field-value :row char))]
    [print self " LEVEL: "]
    [print self (format nil "~D" *level*)]
    [print self " SPEED: "]
    [print self (format nil "~D" [stat-value char :speed])]
    [print self " SCORE: "]
    [println self (format nil "~D" [stat-value char :score])]
    [space self]
    [print self " LOCATION: "]
    [print self (format nil "~s" [location-name *active-world*])]
    [print self " OXYGEN REMAINING: "]
    [print self (format nil "~D" [stat-value char :oxygen])]
    [print self " ENDURIUM: "]
    [print self (format nil "~D U" [stat-value char :endurium])]
    [newline self]))

(defvar *status*)

;;; Splash screen

(defvar *pager* nil)

(define-prototype splash (:parent =widget=))

(define-method render splash ()
  (rlx:draw-resource-image "splash" 0 0 
			   :destination <image>))

(define-method dismiss splash ()
  (play-sample "go")
  [select *pager* :play]
  ;; TODO ugh this is a hack!
  (rlx:show-widgets))

(define-prototype splash-prompt (:parent =prompt=)
  (default-keybindings :initform '(("SPACE" nil "dismiss ."))))

;;; Main program. 

(defun blast ()
  (rlx:message "Initializing Blast Tactics...")
  (setf clon:*send-parent-depth* 2)
  (rlx:set-screen-height 600)
  (rlx:set-screen-width 800)
;  (rlx:set-frame-rate 30)
  ;; (rlx:set-timer-interval 20)

  ;; (rlx:enable-timer)
  (rlx:enable-held-keys 1 15)
  (setf *billboard* (clone =billboard=))
  (setf *asteroid-count* 0)
  (setf *level* 0)
  (let* ((prompt (clone =blast-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (status (clone =status=))
	 (player (clone =ship=))
	 (viewport (clone =view=))
	 (splash (clone =splash=))
	 (splash-prompt (clone =splash-prompt=))
	 (textbox (clone =textbox=))
	 (stack (clone =stack=)))
    ;;
    [resize splash :height 580 :width 800]
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
    [resize narrator :height 120 :width 800]
    [set-verbosity narrator 0]
    ;;
    [set-player universe player]
    [play universe
	  :address '(=star-sector= :width 80 :height 80 :stars 80 :freighters 12)
	  :prompt prompt
	  :narrator narrator]
    [loadout player]
    ;;
    [resize status :height 60 :width 700]
    [set-character status player]
    (setf *status* status)
    [update status]
    ;;
    [resize *billboard* :height 20 :width 100]
    [move *billboard* :x 700 :y 0]
   ;;
    [set-tile-size viewport 16]
    ;; the default is to track the current world:
    ;; [set-world viewport world] 
    [resize viewport :height 432 :width 800]
    [set-origin viewport :x 0 :y 0 :height 24 :width 50]
    [adjust viewport]
    ;;
    [resize textbox :height 100 :width 800]
    [move textbox :x 0 :y 0]
    (setf (field-value :buffer textbox)
	  (find-resource-object "help-message"))
    ;;
    (play-music "xiomacs" :loop t)
    (set-music-volume 255)	       
    ;;
    [resize stack :width 800 :height 580]
    [move stack :x 0 :y 0]
    [set-children stack (list status viewport narrator)]
    ;;
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    [add-page *pager* :main splash-prompt splash]
    [add-page *pager* :play stack prompt status viewport narrator *billboard*]
    [add-page *pager* :help textbox]
    [select *pager* :main]))
