;;; vong.lisp --- hockey paintball snake pong

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

;;; Colors

(defparameter *colors* '(:purple :red :blue :orange :green :yellow :white))

;;; The player's tail

(defcell tail 
  (categories :initform '(:actor))
  (clock :initform 4))
  
(define-method initialize tail (&key direction clock)
  (setf <clock> clock)
  (setf <tile> (ecase direction
		 (:north "tail-north")
		 (:south "tail-south")
		 (:east "tail-east")
		 (:west "tail-west"))))

(define-method run tail ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (< <clock> 0) (setf <clock> 0))
  (when (zerop <clock>)
    [die self]))

(define-method step tail (stepper)
  (when [in-category stepper :puck]
    [play-sample self "freeze"]
    [expend-action-points stepper 200]))

;;; A tail extender powerup

(defcell extender 
  (tile :initform "plus"))

(define-method step extender (stepper)
  (when [in-category stepper :tailed]
    [play-sample self "powerup"]
    [stat-effect stepper :tail-length 7]
    [stat-effect stepper :score 2000]
    [die self]))

;;; Chevrons change the direction of the puck

(defcell chevron
  (tile :initform "chevron-east")
  (categories :initform '(:chevron)))

(defvar *chevron-tiles* '(:north "chevron-north"
			  :south "chevron-south"
			  :east "chevron-east"
			  :west "chevron-west"))

(define-method orient chevron (direction)
  (assert (member direction '(:north :south :east :west)))
  (setf <tile> (getf *chevron-tiles* direction))
  (setf <direction> direction))

(define-method step chevron (stepper)
  (when [in-category stepper :puck]
    [play-sample self "chevron"]
    [kick stepper <direction>]))

;;; Tracers lay down deadly red wires

(defcell wire 
  (categories :initform '(:actor :damaging))
  (stepping :initform t)
  (speed :initform (make-stat :base 1))
  (clock :initform 20))
  
(define-method initialize wire (&key direction clock)
  (setf <clock> clock)
  (setf <tile> (ecase direction
		 (:north "wire-north")
		 (:south "wire-south")
		 (:east "wire-east")
		 (:west "wire-west"))))

(define-method run wire ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (< <clock> 0) (setf <clock> 0))
  (when (zerop <clock>)
    [die self]))

(define-method step wire (stepper)
  (if [is-player stepper]
      [damage stepper 1]
      (when [in-category stepper :puck]
	(unless [in-category stepper :tracer]
	  [die stepper]))))

(defvar *tracer-tiles* '(:north "tracer-north"
			  :south "tracer-south"
			  :east "tracer-east"
			  :west "tracer-west"))

(defcell tracer 
  (tile :initform "tracer-north")
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper :puck :tailed :tracer))
  (speed :initform (make-stat :base 5 :min 5))
  (max-items :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 30))
  (stepping :initform t)
  (tail-length :initform (make-stat :base 20))
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (direction :initform (car (one-of '(:north :south :east :west))))
  (strength :initform (make-stat :base 4 :min 0 :max 30))
  (dexterity :initform (make-stat :base 5 :min 0 :max 30))
  (intelligence :initform (make-stat :base 11 :min 0 :max 30))
  (hit-points :initform (make-stat :base 10 :min 0 :max 10)))

(define-method update-tile tracer ()
  (setf <tile> (getf *tracer-tiles* <direction>)))

(define-method kick tracer (direction)
  (setf <direction> direction))
    
(define-method run tracer ()
  (clon:with-field-values (row column) self
    (let ((world *active-world*))
      (when [obstacle-in-direction-p world row column <direction>]
	(setf <direction> (car (one-of '(:north :south :east :west)))))
      [move self <direction>])))

(define-method drop-wire tracer ()
  [drop-cell *active-world* (clone =wire= :direction <direction> :clock 5)
	     <row> <column>])

(define-method move tracer (direction)
  (let ((wall (clone =wall=)))
    [drop-wire self]
    [update-tile self]
    [parent>>move self direction]))

;;; Replacement puck

(defcell puckup 
  (tile :initform "puckup"))

(define-method step puckup (stepper)
  (when [is-player stepper]
    (let ((puck (clone =puck=)))
      [drop self puck]
      [grab stepper puck]
      [die self])))

;;; Black hole eats anything in category :puck

(defcell hole 
  (tile :initform "hole")
  (categories :initform '(:exclusive :hole)))

(define-method step hole (stepper)
  (when [in-category stepper :puck]
    [play-sample self "hole-suck"]
    [die stepper]))

;; (define-method die tracer ()
;;   (when (> 5 (random 10))
;;     [drop self (clone (random-powerup))])
;;   [play-sample self "blaagh"]
;;   [parent>>die self])


;;; Bricks

(defvar *brick-tiles* '(:purple "brick-purple"
			:black "brick-black"
			:red "brick-red"
			:blue "brick-blue"
			:orange "brick-orange"
			:green "brick-green"
			:white "brick-white"
			:yellow "brick-yellow"))

(defcell brick 
  (tile :initform "brick-purple")
  (categories :initform '(:obstacle :exclusive :paintable)))

(define-method paint brick (c)
  (setf <color> c)
  (let ((res (getf *brick-tiles* c)))
    (assert (stringp res))
    (setf <tile> res)))
	
;;; Breakable paint walls re-color the ball

(defvar *wall-tiles* '(:purple "wall-purple"
			:black "wall-black"
			:red "wall-red"
			:blue "wall-blue"
			:orange "wall-orange"
			:green "wall-green"
			:white "wall-white"
			:yellow "wall-yellow"))

(defcell wall 
  (tile :initform "wall-purple")
  (categories :initform '(:exclusive :obstacle :wall))
  (color :initform :purple))

(define-method paint wall (c)
  (setf <color> c)
  (let ((res (getf *wall-tiles* c)))
    (assert (stringp res))
    (setf <tile> res)))

;; (define-method step wall (puck)
;;   (when [in-category puck :puck]
;;     [paint puck <color>]
;;     [bounce puck]
;;     [die self]))
  
;;; Our hero, the player

(defcell player 
  (tile :initform "player")
  (name :initform "Player")
  (score :initform (make-stat :base 0))
  (last-direction :initform :north)
  (dead :initform nil)
  (puck :initform nil)
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (strength :initform (make-stat :base 13))
  (tail-length :initform (make-stat :base 20 :min 0))
  (dexterity :initform (make-stat :base 13))
  (defense :initform (make-stat :base 15))
  (equipment-slots :initform '(:left-hand :right-hand))
  (hearing-range :initform 1000)
  (hit-points :initform (make-stat :base 1 :min 0 :max 30))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (stepping :initform t)
  (attacking-with :initform :right-hand)
  (light-radius :initform 3)
  (categories :initform '(:actor :tailed :player :target :container :light-source)))

(define-method run player ()
  nil)

(define-method quit player ()
  (rlx:quit :shutdown))

(define-method step player (stepper)
  (if [in-category stepper :puck]
      [grab self stepper]
      (when [in-category stepper :damaging]
	[damage self 1])))

(define-method drop-tail player ()
  [drop self (clone =tail= 
		    :direction <last-direction> 
		    :clock [stat-value self :tail-length])])

(define-method restart player ()
  (let ((player (clone =player=)))
    [destroy *active-universe*]
    [set-player *active-universe* player]
    [play *active-universe*
	  :address '(=vong= :level 1)]
    [loadout player]
    [play-sample self "go"]))

(define-method drop-chevron player ()
  (unless <dead>
    (let ((chevron (clone =chevron=)))
      [drop self chevron]
      [orient chevron <last-direction>])))

(define-method move player (direction)
  (unless <dead>
    (setf <last-direction> direction)
    [drop-tail self]
    [parent>>move self direction]))
  
(define-method loadout player ()
  (setf <puck> (clone =puck=)))

(define-method throw player (direction)
  (assert (member direction '(:north :south :east :west)))
  (unless <dead>
    (clon:with-fields (puck) self
      (when puck
	(multiple-value-bind (r c)
	    (step-in-direction <row> <column> direction)
	  [drop-cell *active-world* puck r c])
	[kick puck direction]
	(setf puck nil)
	[play-sample self "serve"]))))

(define-method grab player (puck)
  (assert [in-category puck :puck])
  (if (null <puck>)
      (progn (setf <puck> puck)
	     [delete-from-world puck]
	     [play-sample self "grab"])))

(define-method die player ()
  (setf <tile> "skull")
  [play-sample self "death"]
  [say self "You died. Press ESCAPE to try again."]
  (setf <dead> t))

;;; Controlling the game

(define-prototype vong-prompt (:parent rlx:=prompt=))

(defparameter *numpad-keybindings* 
  '(("KP8" nil "move :north .")
    ("KP4" nil "move :west .")
    ("KP6" nil "move :east .")
    ("KP2" nil "move :south .")
    ;;
    ("KP8" (:control) "throw :north .")
    ("KP4" (:control) "throw :west .")
    ("KP6" (:control) "throw :east .")
    ("KP2" (:control) "throw :south .")
    ;; arrows
    ("UP" nil "move :north .")
    ("LEFT" nil "move :west .")
    ("RIGHT" nil "move :east .")
    ("DOWN" nil "move :south .")
    ;;
    ("UP" (:control) "throw :north .")
    ("LEFT" (:control) "throw :west .")
    ("RIGHT" (:control) "throw :east .")
    ("DOWN" (:control) "throw :south .")))

(defparameter *qwerty-keybindings*
  (append *numpad-keybindings*
	  '(("K" nil "move :north .")
	    ("H" nil "move :west .")
	    ("L" nil "move :east .")
	    ("J" nil "move :south .")
	    ;;
	    ("K" (:control) "throw :north .")
	    ("H" (:control) "throw :west .")
	    ("L" (:control) "throw :east .")
	    ("J" (:control) "throw :south .")
	    ;;
	    ("ESCAPE" nil "restart .")
	    ("SPACE" nil "drop-chevron .")
	    ("Q" (:control) "quit ."))))
  
(define-method install-keybindings vong-prompt ()
  (dolist (k *qwerty-keybindings*)
      (apply #'bind-key-to-prompt-insertion self k))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *active-world* :timer])])

;;; The floor

(defcell floor
  (tile :initform "floor")
  (color :initform ".black"))

;;; The puck

(defvar *puck-tiles* '(:purple "puck-purple"
			:black "puck-black"
			:red "puck-red"
			:blue "puck-blue"
			:orange "puck-orange"
			:green "puck-green"
			:white "puck-white"
			:yellow "puck-yellow"))

(defcell puck
  (tile :initform "puck")
  (categories :initform '(:puck :obstacle :target :actor :paintable))
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform :here)
  (stepping :initform t)
  (color :initform :white))

(define-method kick puck (direction)
  (setf <direction> direction))

(define-method bounce puck ()
  (setf <direction> (opposite-direction <direction>))
  [play-sample self "bounce"])

(define-method paint puck (color)
  (setf <color> color)
  (setf <tile> (getf *puck-tiles* color)))

(define-method move puck (direction)
  (multiple-value-bind (r c) 
      (step-in-direction <row> <column> direction)
    (let ((obstacle [obstacle-at-p *active-world* r c]))
      (when obstacle
	[bounce self]
	(when (clon:object-p obstacle)
	  (if [is-player obstacle]
	      [grab obstacle self]
	      ;; it's not the player. see if we can color, or get paint
	      (progn 
		(when [in-category obstacle :paintable]
		  [paint obstacle <color>])
		(when [in-category obstacle :wall]
		  [paint self (field-value :color obstacle)]
		  [die obstacle]))))))
      [parent>>move self <direction>]))

(define-method run puck ()
  ;; pucks don't stop moving.
  (if (eq :here <direction>)
      [die self]
      [move self <direction>]))

(define-method die puck ()
  [play-sample self "buzz"]
  [parent>>die self])

;;; Vong game board

(define-prototype vong (:parent rlx:=world=)
  (name :initform "Vong board")
  (description :initform "Welcome to Vong, level 1.")
  (edge-condition :initform :block)
  (width :initform 50)
  (height :initform 29)
  (scale :initform '(1 nm))
  (ambient-light :initform :total))

(define-method generate vong (&key (level 1))
  [create-default-grid self]
  (clon:with-fields (height width grid) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))
    (dotimes (n 12)
      (let ((brick (clone =brick=)))
	[drop-cell self brick (random height) (random width) :exclusive t]
	[paint brick :white]))
    (dotimes (n 9)
      [drop-cell self (clone =tracer=) (random height) (random width)])
    (dotimes (n 9)
      [drop-cell self (clone =extender=) (random height) (random width)])
    (dotimes (n 12)
      [drop-cell self (clone =puckup=) (random height) (random width)])
    (dotimes (n 23)
      (let ((color (car (one-of *colors*))))
	(labels ((drop-wall (r c)
		   (prog1 nil
		     (let ((wall (clone =wall=)))
		       [drop-cell self wall r c :exclusive t]
		       [paint wall color]))))
	  (let ((r (random height))
		(c (random width)))
	    [drop-cell self (clone =hole=) 
		       (+ r 2 (random 3))
		       (+ c 2 (random 3))
		       :exclusive t]
	    (trace-rectangle #'drop-wall r c
			     (+ 4 (random 8)) (+ 4 (random 8)) :fill)))))))
  
(define-method begin-ambient-loop vong ()  
  (play-music "flyby" :loop t))
      
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
  (rlx:enable-timer)
  (rlx:set-timer-interval 1)
  (rlx:enable-held-keys 1 3)
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
	       [set-tile-size viewport 16]
	       [resize viewport :height 470 :width *vong-window-width*]
	       [move viewport :x 0 :y 0]
	       [set-origin viewport :x 0 :y 0 
			   :height (truncate (/ (- *vong-window-height* 130) 16))
			   :width (truncate (/ *vong-window-width* 16))]
	       [adjust viewport]))
      (setf *space-bar-function* #'spacebar))
    ;;
    [set-buffer textbox
    		(find-resource-object "help-message")]
    [resize-to-fit textbox] 
    [move textbox :x 0 :y 0]
    
    (play-music "techworld" :loop t)
    (set-music-volume 255)	       
    ;;
    [resize stack :width *vong-window-width* :height (- *vong-window-height* 20)]
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
