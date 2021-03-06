;;; xiotank.lisp --- xiotank!

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

(in-package :cons-game)

;;; Sound waves

(defparameter *waveforms* '(:sine :square :saw :bass))
(defparameter *wave-colors* '(:yellow :cyan :magenta :green))

(defparameter *wave-samples*
  '((:sine "A-2-sine" "A-4-sine")
    (:saw "A-2-saw" "A-4-saw")
    (:square "A-2-square" "A-4-square")))

(defun wave-sample (type &optional (note "A-4"))
  (assert (member type *waveforms*))
  (concatenate 'string note "-" (string-downcase (symbol-name type))))

(defparameter *wave-images*
  '((:sine :green "sine-green" :yellow "sine-yellow" :magenta "sine-magenta" :cyan "sine-cyan")
    (:square :green "square-green" :yellow "square-yellow" :magenta "square-magenta" :cyan "square-cyan")
    (:saw :green "saw-green" :yellow "saw-yellow" :magenta "saw-magenta" :cyan "saw-cyan")))

(defun wave-image (type &optional (color :green))
  (assert (and (member type *waveforms*)
	       (member color *wave-colors*)))
  (getf (cdr (assoc type *wave-images*))
	color))

(defparameter *pulse-delay* 8)

(defsprite wave
  (description :initform "A sonic wave.")
  (team :initform :player)
  (color :initform :green)
  (waveform :initform :sine)
  (note :initform "A-4")
  (clock :initform 60)
  (pulse :initform (random *pulse-delay*))
  (image :initform nil)
  (direction :initform nil)
  (speed :initform (make-stat :base 20))
  (movement-distance :initform (make-stat :base 2))
  (movement-cost :initform (make-stat :base 20))
  (categories :initform '(:wave :actor)))

(define-method start wave (&key (note "A-4") (waveform :sine) (direction :north) (team :player) (color :green))
  (setf <waveform> waveform)
  (setf <team> team)
  (setf <note> note)
  [update-image self (wave-image waveform color)]
  (setf <sample> (wave-sample waveform note))
  (setf <direction> direction))

(define-method run wave ()
  (decf <clock>)
  (if (minusp <clock>)
      [die self]
      (progn [expend-action-points self 2]
	     (when <direction> 
	       (multiple-value-bind (y x) (xe2:step-in-direction <y> <x> <direction>
								 [stat-value self :movement-distance])
		 [update-position self x y])
	       ;; decide whether to beep.
	       (if (zerop <pulse>)
		   (progn (setf <pulse> *pulse-delay*)
			  [play-sample self <sample>])
		   (decf <pulse>))))))

(define-method refresh wave ()
  (setf <clock> 60))

(define-method do-collision wave (object)
  (when (and (not [in-category object :wave])
	     [in-category object :target]
	     (has-field :team object)
	     (not (eq <team> (field-value :team object))))
    [hit object self]
    [die self]))

;;; Pulsators 

(defparameter *default-pulsator-delay* 20)

(defparameter *pulsing* nil)

(defcell pulsator 
  (name :initform "Pulsator")
  (description :initform "Pulsates to a given beat, activating nearby objects.")
  (tile :initform "pulsator")
  (delay :initform *default-pulsator-delay*)
  (clock :initform 0)
  (trip :initform nil)
  (team :initform :neutral)
  (state :initform nil)	 
  (categories :initform '(:obstacle :exclusive :target :actor)))

(define-method update-tile pulsator (&optional pulsing)
  (setf <tile> (if pulsing "pulsator-pulse"
		   (if <state> "pulsator-on" "pulsator"))))

(define-method tap pulsator (delay)
  (setf <delay> delay))

(define-method start pulsator (&optional delay)
  (unless <state>
    [say self "A steady pulsation begins."]
    (setf <clock> 0)
    (when delay (setf <delay> delay))
    (setf <state> t)
    (setf <trip> nil)
    (setf *pulsing* t)
    [update-tile self]))

(define-method stop pulsator ()
  (unless (null <state>)
    [say self "The pulsation stops."]
    (setf <state> nil)
    (setf *pulsing* nil)
    [update-tile self]
    (setf <clock> 0)))

(define-method run pulsator ()
  [update-tile self]
  (when <state>
    (if (zerop <clock>)
	(progn (xe2:play-sample "pulse")
	       [update-tile self t]
	       (setf *pulsing* t)
	       (setf <trip> nil)
	       (labels ((do-circle (image)
			  (prog1 t
			    (multiple-value-bind (x y) 
				[image-coordinates self]
			      (let ((x0 (+ x 8))
				    (y0 (+ y 8)))
				(draw-circle x0 y0 40 :destination image)
				(draw-circle x0 y0 35 :destination image))))))
		 [>>add-overlay :viewport #'do-circle])
	       (setf <clock> <delay>))
	(progn (if <trip>
		   (setf *pulsing* nil)
		   (progn (setf <trip> t)
			  (setf *pulsing* t)))
	       (decf <clock>)))))
  
(define-method hit pulsator (&optional object)
  (if <state> [stop self] [start self]))

;;; Wave delays that respond to pulses

(defcell delay 
  (tile :initform "delay")
  (description :initform "Holds the last wave it receives, releasing when a pulse is heard.")
  (wave :initform nil)
  (coords :initform nil)
  (team :initform :neutral)
  (default-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 20))
  (categories :initform '(:actor :obstacle :exclusive :target)))

(define-method hit delay (&optional object)
  (when [in-category object :wave]
    (setf <wave> object)
    (setf <coords> (multiple-value-bind (x y)
		       [xy-coordinates object]
		     (list x y)))
    [remove-sprite *world* object]))

(define-method run delay ()
  [expend-action-points self 10]
  (setf <tile> (if <wave> "delay-on" "delay"))
  (when (and *pulsing* <wave>)
    [add-sprite *world* <wave>]
    (destructuring-bind (x y) <coords>
      [update-position <wave> x y]
      [move <wave> (field-value :direction <wave>) 17]
      [refresh <wave>])
    (setf <wave> nil)))

;;; Turrets

(defcell turret
  (tile :initform "turret-east-on")
  (description :initform "Fires waves when a pulse is received. When shot, turns 90 degrees.")
  (direction :initform :east)
  (team :initform :neutral) wave
  (default-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 20))
  (categories :initform '(:actor :obstacle :exclusive :target)))

(define-method run turret ()
  [expend-action-points self 10]
  [update-tile self]
  (when *pulsing*
    (setf <wave> (clone =wave=))
    [start <wave> :direction <direction>
	   :team :neutral
	   :waveform :saw]
    [add-sprite *world* <wave>]
    (multiple-value-bind (x y) [xy-coordinates self]
      [update-position <wave> (+ x 4) (+ y 4)]
      [refresh <wave>])))

(define-method update-tile turret ()
  (setf <tile>
	(if *pulsing*
	    (ecase <direction>
	      (:east "turret-east-on")
	      (:west "turret-west-on")
	      (:south "turret-south-on")
	      (:north "turret-north-on"))
	    (ecase <direction>
	      (:east "turret-east")
	      (:west "turret-west")
	      (:south "turret-south")
	      (:north "turret-north")))))
    
(define-method hit turret (&optional object)
  [say self "The turret turned 90 degrees clockwise."]
  (setf <direction> 
	(ecase <direction>
	  (:east :south)
	  (:south :west)
	  (:west :north)
	  (:north :east)))
  [update-tile self])

;;; Fences

(defsprite wire 
  (image :initform "wire-east")
  (clock :initform 20)
  (speed :initform (make-stat :base 10))
  (direction :initform :east)
  (categories :initform '(:actor :obstacle :target)))

(define-method orient wire (dir &optional (clock 5))
  (setf <direction> dir)
  (setf <clock> clock))

(define-method run wire ()
  (if (zerop <clock>) 
      [die self]
      (progn [move self <direction> 5]
	     (decf <clock>)
	     (percent-of-time 10 [play-sample self "woom"])
	     (setf <image> (ecase <direction>
			    (:east "wire-east")
			    (:south "wire-south")
			    (:west "wire-west")
			    (:north "wire-north"))))))

(define-method do-collision wire (object)
  (when (and (has-field :team object)
	     (eq :player (field-value :team object))
	     [in-category object :wave])
    [die object])
  (when (and (has-field :hit-points object)
	     (not (and (has-field :team object)
		       (eq :enemy (field-value :team object)))))
    [damage object 20]))

;; (define-method step wire (stepper)
;;   (when [is-player stepper]
;;     [die stepper]))

(defcell fence
  (tile :initform "fence-east-on")
  (description :initform "This object creates a deadly wire fence. 
Only opens when the right tone is heard.")
  (note :initform "C-1")
  (free :initform nil)
  (fence-length :initform 8)
  (clock2 :initform 0)
  (clock :initform 0)
  (team :initform :neutral)
  (direction :initform :east)
  (sample :initform nil)
  (default-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 3))
  (categories :initform '(:actor :obstacle :exclusive :target)))

(define-method tune fence (note)
  (setf <note> note))

(define-method free fence ()
  (setf <free> t))

(define-method orient fence (dir &optional (length 8))
  (setf <direction> dir)
  (setf <fence-length> length))

(define-method run fence ()
  [expend-action-points self 10]
  [update-tile self]
  (when (null (note-playing-p <note>))
    (setf <clock> (truncate (/ <fence-length> 2))))
  (when (plusp <clock>)
    (when (equal (truncate (/ <fence-length> 2)) <clock>)
      (let ((wire (clone =wire=)))
	[orient wire <direction> (* 3 <fence-length>)]
	(multiple-value-bind (x y) [xy-coordinates self]
	  [drop-sprite self wire (+ x 2) (+ y 2)])))
    (decf <clock>)))

(define-method update-tile fence ()
  (setf <tile>
	(if *pulsing*
	    (ecase <direction>
	      (:east "fence-east-on")
	      (:west "fence-west-on")
	      (:south "fence-south-on")
	      (:north "fence-north-on"))
	    (ecase <direction>
	      (:east "fence-east")
	      (:west "fence-west")
	      (:south "fence-south")
	      (:north "fence-north")))))
    
(define-method hit fence (&optional object)
  nil)

(defcell antifence
  (tile :initform "antifence-east-on")
  (description :initform "This object creates a deadly wire antifence. 
An ANTI-fence only opens when an offending tone is silenced.")
  (note :initform "C-1")
  (free :initform nil)
  (antifence-length :initform 8)
  (clock2 :initform 0)
  (clock :initform 0)
  (team :initform :neutral)
  (direction :initform :east)
  (sample :initform nil)
  (default-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 3))
  (categories :initform '(:actor :obstacle :exclusive :target)))

(define-method tune antifence (note)
  (setf <note> note))

(define-method free antifence ()
  (setf <free> t))

(define-method orient antifence (dir &optional (length 8))
  (setf <direction> dir)
  (setf <antifence-length> length))

(define-method run antifence ()
  [expend-action-points self 10]
  [update-tile self]
  (when (note-playing-p <note>)
    (setf <clock> (truncate (/ <antifence-length> 2))))
  (when (plusp <clock>)
    (when (equal (truncate (/ <antifence-length> 2)) <clock>)
      (let ((wire (clone =wire=)))
	[orient wire <direction> (* 3 <antifence-length>)]
	(multiple-value-bind (x y) [xy-coordinates self]
	  [drop-sprite self wire (+ x 2) (+ y 2)])))
    (decf <clock>)))

(define-method update-tile antifence ()
  (setf <tile>
	(if *pulsing*
	    (ecase <direction>
	      (:east "antifence-east-on")
	      (:west "antifence-west-on")
	      (:south "antifence-south-on")
	      (:north "antifence-north-on"))
	    (ecase <direction>
	      (:east "antifence-east")
	      (:west "antifence-west")
	      (:south "antifence-south")
	      (:north "antifence-north")))))
    
(define-method hit antifence (&optional object)
  nil)

;;; Triggers just play a sample once per hit

(defcell trigger
  (tile :initform "trigger")
  (sample :initform nil)
  (description :initform "Each trigger is tuned to a different note; plays the note when struck.")
  (team :initform :neutral)
  wave
  (default-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 20))
  (categories :initform '(:actor :obstacle :exclusive :target :sequenced)))

(define-method run trigger ()
  [expend-action-points self 10])

(define-method intone trigger (sample)
  (setf <sample> sample))

(define-method hit trigger (&optional object)
  (when <sample> 
    [play-sample self <sample>]
    (dotimes (n 6)
      [drop-cell *world* (clone =particle=) <row> <column> :exclusive nil :probe nil])))

;;; Oscillators and tone clusters

(defparameter *scale* '("C-2" "C#2" "D-2" "D#2" "E-2" 
			"F-2" "F#2" "G-2" "G#2" "A-2" "A#2"
			"B-2" "C-3"))

(defparameter *scale-white-keys* '("C-2" "D-2" "E-2" 
			"F-2"  "G-2"  "A-2"
			"B-2" "C-3"))

(defun random-note (&optional (scale *scale*))
  (car (one-of scale)))

(defun random-cluster (&optional (size 3) (scale *scale*))
  (let (tones (n 0))
    (loop while (< (length tones) size)
	  do (pushnew (random-note scale) tones :test 'equal))
    tones))

(defparameter *example-cluster* '("C-2" "E-2" "G-2"))

(defvar *notes* nil)

(defun add-note (note)
  (pushnew note *notes* :test 'equal))

(defun remove-note (note)
  (setf *notes* (delete note *notes* :test 'equal)))

(defun note-playing-p (note)
  (member note *notes* :test 'equal))

(defparameter *oscillator-tiles* '((:sine "osc-sine-off" "osc-sine-on")
				   (:square "osc-square-off" "osc-square-on")
				   (:saw "osc-saw-off" "osc-saw-on")))

(defun oscillator-tile (waveform &optional state)
  (assert (member waveform *waveforms*))
  (let ((tiles (cdr (assoc waveform *oscillator-tiles*))))
    (if (null state) (first tiles) (second tiles))))

(defcell oscillator 
  (categories :initform '(:actor :obstacle :exclusive :target))
  channel
  (description :initform "This object emits a tone when struck. Shoot to toggle.")
  (team :initform :neutral)
  (waveform :initform :sine)
  (note :initform "A-2")
  (state :initform nil))

(define-method intone oscillator (waveform &optional (note "A-2"))
  [stop self]
  (setf <waveform> waveform)
  (setf <note> note)
  [update-tile self])
  
(define-method update-tile oscillator ()
  (setf <tile> (oscillator-tile <waveform> <state>)))

(define-method start oscillator (waveform &optional (note "A-2"))
  (unless <channel>
    [say self (format nil "The note ~A has begun playing." note)]
    (add-note note)
    [intone self waveform note]
    (setf <state> t)
    [update-tile self]
    (let ((label (clone =label= 
			:name "Oscillator" 
			:description (concatenate 'string "Currently playing tone " <note>)
			:tile "osc-sine-on"
			:text (list (list (list <note>))))))
      [drop-cell *world* label <row> <column> :exclusive nil]))
    (setf <channel> (xe2:play-sample (wave-sample waveform note) :loop t)))

(define-method stop oscillator ()
  (unless (null <channel>)
    [say self (format nil "The note ~A has stopped." <note>)]
    (remove-note <note>)
    (setf <state> nil)
    [update-tile self]
    (let ((label [category-at-p *world* <row> <column> :label]))
      (when label
	[die label]))
    (xe2:halt-sample <channel>)
    (setf <channel> nil)))

(define-method run oscillator () nil)

(define-method hit oscillator (&optional object)
  (if <state> [stop self] [start self <waveform> <note>]))
				
;;; Resonators

(defcell resonator 
  (tile :initform "resonator")
  (description :initform "Emits energy particles on the beat, when a given tone is heard.")
  (categories :initform '(:actor :obstacle :exclusive :target))
  channel
  (team :initform :neutral)
  (waveform :initform :sine)
  (note :initform "A-2")
  (state :initform nil))

(define-method tune resonator (note)
  (setf <note> note))

(define-method run resonator ()
  (setf <tile> (if (note-playing-p <note>)
		   "resonator-on"
		   "resonator"))
  (when (and *pulsing* (note-playing-p <note>))
    (dotimes (n 5)
      [drop-cell *world* (clone =phi=) <row> <column> :exclusive nil])))

(define-method hit resonator (&optional object)
  nil)

;;; The sonic cannon

(defparameter *wave-cannon-reload-time* 20)

(defcell wave-cannon
  (tile :initform "gun")
  (reload-clock :initform 0)
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:center-bay))
  (weight :initform 7000)
  (accuracy :initform (make-stat :base 100))
  (attack-power :initform (make-stat :base 12))
  (attack-cost :initform (make-stat :base 10))
  (energy-cost :initform (make-stat :base 0)))

(define-method fire wave-cannon (direction)
  (if (plusp <reload-clock>)
      nil ;; (decf <reload-clock>)
      (progn 
	(setf <reload-clock> *wave-cannon-reload-time*)
	(if [expend-energy <equipper> [stat-value self :energy-cost]]
	    (let ((wave (clone =wave=)))
	      (multiple-value-bind (x y) [viewport-coordinates <equipper>]
		[drop-sprite <equipper> wave (+ x 4) (+ y 4)]
		[start wave :direction direction :team (field-value :team <equipper>)
		       :color (field-value :color <equipper>)
;;		       :note (car (one-of (list "A-4"  "A-2")))
		       :waveform (field-value :waveform <equipper>)]))
	    (when [is-player <equipper>]
	      [say <equipper> "Not enough energy to fire!"])))))

(define-method recharge wave-cannon ()
  (decf <reload-clock>))

;;; The tank!

(defcell tank 
  (tile :initform "tank-north")
  (description :initform "Your trusty XENG Industries Model X10 battle tank supports 8-way movement and firing.")
  (dead :initform nil)
  (last-turn-moved :initform 0)
  (team :initform :player)
  (color :initform :green)
  (waveform :initform :sine)
  (hit-points :initform (make-stat :base 20 :min 0 :max 20))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (speed :initform (make-stat :base 10 :min 0 :max 25))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 15)
  (energy :initform (make-stat :base 40 :min 0 :max 40 :unit :gj))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (stepping :initform t)
  (direction :initform :north)
  (attacking-with :initform nil)
  (firing-with :initform :center-bay)
  (categories :initform '(:actor :obstacle :player :target :container :light-source :vehicle :repairable))
  (equipment-slots :initform '(:left-bay :right-bay :center-bay :extension)))

(define-method loadout tank ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =wave-cannon=)]])

(define-method hit tank (&optional object)
  [play-sample self "ouch"]
  [parent>>damage self 1])

(define-method pause tank ()
  [pause *world*])

(defparameter *tank-tiles* '(:north "tank-north"
			     :south "tank-south"
			     :east "tank-east"
			     :west "tank-west"
			     :northeast "tank-northeast"
			     :northwest "tank-northwest"
			     :southeast "tank-southeast"
			     :southwest "tank-southwest"))

(define-method move tank (direction)
  (unless <dead>
    (let ((phase (field-value :phase-number *world*)))
      (unless (= <last-turn-moved> phase)
	(setf <last-turn-moved> phase)
	(setf <direction> direction)
	(setf <tile> (getf *tank-tiles* direction))
	[parent>>move self direction]))))

(define-method fire tank (direction)
  (unless <dead>
    [play-sample self "pop"]
    [parent>>fire self direction]))

;; (define-method shield tank ()
;;   (unless <dead>
;;     (if (>= [stat-value self :energy] 5) 
;; 	(labels ((drop-shield (r c)
;; 		   (prog1 nil
;; 		     [drop-cell *world* (clone =shield=) r c :no-collisions t])))
;; 	  (trace-rectangle #'drop-shield (- <row> 2) (- <column> 2) 5 5)
;; 	  [play-sample self "saddown"]
;; 	  ;;[stat-effect self :energy -10]
;; 	  )
;; 	[say self "Not enough energy to activate shield."])))

(define-method run tank ()
  (let ((cannon [equipment-slot self :center-bay]))
    (when cannon [recharge cannon])))
  
(define-method quit tank ()
  (xe2:quit :shutdown))

(define-method die tank ()
  (unless <dead>
    (setf <tile> "tank-disabled")
    [play-sample self "gameover"]
    [say self "YOU DIED. PRESS ESCAPE TO TRY AGAIN!"]
    (setf <dead> t)))

(define-method restart tank ()
  (let ((tank (clone =tank=)))
    [say self "Restarting XIOTANK..."]
    (halt-sample t)
    [destroy *universe*]
    [set-player *universe* tank]
    [set-character *status* tank]
    [play *universe*
	  :address (generate-level-address 3)]
    [loadout tank]))

;;; Blue Space

(defparameter *xiotank-grammar* 
  '((puzzle >> (:drop-border :goto-origin 
		:pushloc goto-pulsator-location :drop-pulsator :poploc
		drop-room tone :goto-east maybe-south drop-room tone :goto-east maybe-south drop-room tone
	         enemies powerups :goto-origin :drop-extras player :drop-exit))
    (drop-room >> (:drop-room :drop-shockers))
    (tone >> :drop-tone-pair)
    (maybe-south >> :noop :goto-south (:goto-south :pushloc :goto-east :drop-ruin :poploc))
    (enemies >> (:drop-corruptors))
    (goto-pulsator-location >> :goto-bottom-right :goto-top-right)
    (powerups >> :drop-powerups)
    (player >> :drop-player)
    (pulsator >> :drop-pulsator)))

;; (setf xe2:*grammar* *xiotank-grammar*)
;; (xe2:generate 'puzzle)

;; (:DROP-TONE-PAIR :GOTO-EAST :DROP-TONE-PAIR :DROP-SHOCKERS :DROP-POWERUPS
;;  :GOTO-RANDOM-POSITION :DROP-PLAYER)

(defcell block 
  (tile :initform "block")
  (description :initform "An impenetrable wall.")
  (team :initform :neutral)
  (categories :initform '(:obstacle :opaque :exclusive :target)))

(define-method hit block (&optional other)
  nil)

(defcell blue-space 
  (description :initform "The mysterious blue substrate of Frequency World.")
  (tile :initform "blue-space"))

(define-prototype blue-world (:parent xe2:=world=)
  (locations :initform nil)
  gen-row gen-column 
  ;;
  (description :initform "You enter an unknown data facility in Blue Space.")
  (level :initform 1)
  (cluster :initform nil)
  (free-tones :initform nil)
  (n :initform 0)
  ;;
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (spacing :initform 12)
  (room-size :initform 10)
  (scale :initform '(3 m))
  (edge-condition :initform :block))

(defparameter *bass-notes* '("C-2-bass" "C#2-bass" "D-2-bass" "D#2-bass" "E-2-bass" 
	      "F-2-bass" "F#2-bass" "G-2-bass" "G#2-bass" "A-2-bass" "A#2-bass"
	      "B-2-bass" "C-3-bass"))

(define-method pushloc blue-world ()
  (push (list <gen-row> <gen-column>) <locations>))

(define-method poploc blue-world ()
  (let ((loc (pop <locations>)))
    (if (listp loc)
	(destructuring-bind (r c) loc
	  (setf <gen-row> r <gen-column> c))
	(message "Warning; popping empty location stack."))))

(define-method generate blue-world (&key (height 50)
					    (width 50)
					    (level 1))
  (setf *notes* nil)
  (setf <level> level)
  (setf <height> height <width> width)
  [create-default-grid self]
  (setf <gen-row> 0 <gen-column> 0)
  (dotimes (i height)
    (dotimes (j width)
      [drop-cell self (clone =blue-space=)
		 i j]))
  ;; create a puzzle
  (loop do (progn (setf <cluster> (random-cluster level *scale*))
		  (setf <free-tones> (random-cluster level *scale*)))
	while (intersection <cluster> <free-tones> :test 'equal))
  (setf xe2:*grammar* *xiotank-grammar*)
  (let ((puzzle (generate 'puzzle)))
    (dolist (op puzzle)
      (when (and (keywordp op) (clon:has-method op self))
	(send nil op self)))
    (message (prin1-to-string puzzle)))
  [drop-cell self (clone =launchpad=) 10 10])

(define-method drop-shockers blue-world ()
  (dotimes (n 4)
    [drop-cell self (clone =shocker=) (+ <gen-row> (random <room-size>)) 
	       (+ <gen-column> (random <width>)) :loadout t]))

(define-method drop-corruptors blue-world ()
  (dotimes (n 3) 
    [drop-cell self (clone =corruptor=) (random <height>) (random <width>) :loadout t]))

(define-method drop-pulsator blue-world ()
  (let ((pulse (clone =pulsator=)))
    [drop-cell self pulse (+ <gen-row> 2) (+ <gen-column> 2)]
    [tap pulse 20]))

(define-method drop-room blue-world ()
  (let (points)
    (labels ((collect (&rest point)
	       (prog1 nil
		 (push point points))))
      (trace-rectangle #'collect <gen-row> <gen-column>
		       <room-size> <room-size>)
      (percent-of-time 15 [drop-cell self (clone =health=) (+ <gen-row> 1 (random 3)) (+ <gen-column> 2 (random 4))])
      (dotimes (n 4)
	(setf points (cdr points)))
	      ;;(delete (car (one-of points)) points :test 'equal)))
      (dolist (point points)
	(destructuring-bind (r c) point
	  [drop-cell self (clone =block=) r c])))))

(define-method drop-border blue-world ()
  (labels ((drop-block (r c)
	     (prog1 nil [drop-cell self (clone =block=) r c])))
    (trace-rectangle #'drop-block 0 0 <height> <width>)))

(define-method drop-exit blue-world ()
  (let ((r (- <height> 10))
	(c (+ 3 (random 13))))
    (labels ((drop-block (r c)
	       (prog1 nil [drop-cell self (clone =block=) r c])))
      (trace-column #'drop-block c r <height>)
      (trace-column #'drop-block (+ c 9) r <height>))
    (dolist (tone <cluster>)
      (incf r)
      (let ((fence (clone =fence=)))
	[drop-cell self fence r (+ c 1)]
	[tune fence tone]))
    (dolist (tone <free-tones>)
      (incf r)
      (let ((fence (clone =antifence=)))
	[drop-cell self fence r (+ c 1)]
	[tune fence tone]
	[free fence]))
    (let ((exit (clone =exit=)))
      (incf r)
      ;; [drop-cell self exit (- r 1) (- c 2)]
      [drop-cell self exit r (+ c 3)]
      [level exit (+ 1 <level>)])))

(define-method drop-ruin blue-world ()
  (labels ((drop-block (r c)
	     (prog1 nil [drop-cell self (clone =delay=) r c])))
    (let ((row <gen-row>) (column <gen-column>))
      (trace-row #'drop-block (+ row 2 (random 3))
		 (+ column 2 (random 2)) (+ column 6 (random 5)))
      (trace-column #'drop-block (+ column 2 (random 3))
		 (+ row 2 (random 2)) (+ row 6 (random 5))))))

(define-method goto-origin blue-world ()
  (setf <gen-row> (+ 2 (random 3)))
  (setf <gen-column> (+ 2 (random 3))))

(define-method goto blue-world (r c)
  (setf <gen-column> (* <spacing> c))
  (setf <gen-row> (* <spacing> r)))

(define-method goto-east blue-world ()
  (incf <gen-column> <spacing>))

(define-method goto-west blue-world ()
  (decf <gen-column> <spacing>))

(define-method goto-south blue-world ()
  (incf <gen-row> <spacing>))

(define-method goto-north blue-world ()
  (decf <gen-row> <spacing>))

(define-method goto-bottom-right blue-world ()
  (setf <gen-row> (* <spacing> 3))
  (setf <gen-column> (* <spacing> 3)))

(define-method goto-top-right blue-world ()
  (setf <gen-row> 3)
  (setf <gen-column> (* <spacing> 3)))

(define-method noop blue-world ()
  nil)

(define-method drop-extras blue-world ()
  (dotimes (n 10)
    (let ((trigger (clone =trigger=)))
      [drop-cell self trigger (random <height>) (random <width>)]
      [intone trigger (car (one-of *bass-notes*))]))
  (dotimes (n <level>)
    (let ((drone (clone =drone=)))
      [add-sprite self drone]
      [update-position drone (+ 200 (random 400)) (+ 200 (random 500))])))

(define-method goto-random-position blue-world ()
  [goto self (random 5) (random 5)])

(define-method drop-tone-pair blue-world ()
  (let ((note (nth <n> <cluster>))
	(free-tone (nth <n> <free-tones>))
	(osc (clone =oscillator=))
	(res (clone =resonator=))
	(osc2 (clone =oscillator=))
	(res2 (clone =resonator=))
	(fence (clone =fence=)))
    (if (> <n> (length <cluster>))
	(message "Dropping tone pair.")
	(progn [drop-cell self osc (+ 1 <gen-row> (random 3)) (+ <gen-column> 3 (random 5))]
	       (incf <n>)
	       (assert (and note free-tone))
	       [intone osc :sine note] 
	       [drop-cell self res (+ 2 <gen-row> (random 5)) (+ <gen-column> 3 (random 5))]
	       [tune res note]
	       (let ((fence (clone =fence=)))
		 [tune fence free-tone]
		 [drop-cell self fence (+ <gen-row> 4) (+ <gen-column> 1)])
	       ;; 
	       [pushloc self]
	       (if (percent-of-time 10 (prog1 t [goto-south self]))
		   nil
		   [goto-east self])
	       [intone osc2 :sine free-tone] 
	       [drop-cell self osc2 (+ 10 <gen-row> (random 5)) (+ <gen-column> 3 (random 5))]
	       [drop-cell self res2 (+ 10 <gen-row> (random 5)) (+ <gen-column> 3 (random 5))]
	       [tune res2 free-tone]
	       [poploc self]))))
    
;;; xiotank.lisp ends here

