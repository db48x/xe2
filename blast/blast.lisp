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
;; system. Instead of shooting asteroids, you sweep your trail across
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
			      :ammo ("AMMO!" :foreground ".red" :background ".black"
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

;;; Empty space.

(defcell space 
  (tile :initform "space"))

;;; Colored space.

(defcell space2
  (tile :initform "space2"))

;;; A destructible wall.

(defcell wall
  (tile :initform "wall")
  (categories :initform '(:obstacle))
  (hit-points :initform (make-stat :base 10 :min 0)))

;;; An explosion.

(define-prototype explosion (:parent rlx:=cell=)
  (name :initform "Explosion")
  (categories :initform '(:actor))
  (tile :initform "explosion")
  (speed :initform (make-stat :base 10))
  (damage-per-turn :initform 1)
  (clock :initform 2))

(define-method run explosion ()
  (if (zerop <clock>)
      [die self]
      (progn
	(decf <clock>)
	[expend-action-points self 10]
	(let* ((cells [cells-at *active-world* <row> <column>])
	       (x (1- (fill-pointer cells))))
	  (loop while (not (minusp x))
	       do (progn 
		    [>>damage (aref cells x) <damage-per-turn>]
		    (decf x)))))))

;;; Your explosive vapor trail. 

(defcell trail 
  (categories :initform '(:actor))
  (clock :initform 4))
  
(define-method initialize trail (&key direction clock)
  (setf <clock> clock)
  (setf <tile> (ecase direction
		 (:north "trail-north")
		 (:south "trail-south")
		 (:east "trail-east")
		 (:west "trail-west")
		 (:northeast "trail-northeast")
		 (:northwest "trail-northwest")
		 (:southeast "trail-southeast")
		 (:southwest "trail-southwest"))))

(define-method run trail ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (zerop <clock>)
    [die self]))

(define-method step trail (stepper)
  [drop self (clone =explosion=)]	       
  [damage stepper 1])

;;; Death icon.

(defparameter *death-message* "You are dead. Press SPACE BAR to respawn.")
(defparameter *game-over-message* "No lives remaining. GAME OVER.")

(define-prototype skull (:parent rlx:=cell=)
  (tile :initform "skull")
  (player :initform nil)
  (lives :initform nil)
  (categories :initform '(:dead :player :actor))
  (action-points :initform 0))

(define-method forward skull (&rest args)
  (declare (ignore args))
  [queue>>narrateln :narrator *death-message*])
  
(define-method move skull (&rest args)
  (declare (ignore args))
 [queue>>narrateln :narrator *death-message*])

(define-method attack skull (&rest args)
  (declare (ignore args))
 [queue>>narrateln :narrator *death-message*])

(define-method fire skull (&rest args)
  (declare (ignore args))
 [queue>>narrateln :narrator *death-message*])

(define-method quit skull ()
  (rlx:quit :shutdown))

(define-method initialize skull (player lives)
  [say *billboard* :dead]
  (setf <player> player
	<lives> lives)
  (if (plusp lives)
      (progn [>>say :narrator *death-message*]
	     [>>say :narrator "You have ~D lives remaining." lives])
      [>>say :narrator *game-over-message*]))

(define-method respawn skull ()
  (if (plusp <lives>)
      (progn
	[>>say :narrator "Respawning."]
        [die self]
	(play-sample "go")
	[revive <player>])
      (progn
	[>>say :narrator *game-over-message*])))

;;; Pulse particle

(defcell pulse
  (tile :initform "pulse")
  (categories :initform '(:actor :target))
  (hit-points :initform (make-stat :base 10 :min 0))
  (clock :initform (+ 4 (random 4))))

(define-method run pulse ()
  (when (zerop <clock>)
    [die self])
  (decf <clock>)
  (rlx:do-cells (cell [cells-at *active-world* <row> <column>])
    (when (not [is-player cell])
      [damage cell 5])))

;;; Pulse wave cannon

(defcell pulse-cannon 
  (categories :initform '(:item :weapon :equipment))
  (attack-power :initform (make-stat :base 10))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (weight :initform 3000)
  (equip-for :initform '(:left-bay))
  (tile :initform "pulse-cannon"))

(define-method activate pulse-cannon ()
  (let* ((world *active-world*)
	 (row [player-row world])
	 (column [player-column world]))
    (if (plusp [stat-value <equipper> :pulse-ammo])
	(progn 
	  [>>say :narrator "Activating pulse cannon."]
	  (labels ((drop-pulse (r c)
		     (prog1 nil 
		       [drop-cell world (clone =pulse=) r c])))
	    (trace-rectangle #'drop-pulse (- row 2) (- column 2)
			     5 5 :fill)))
	[>>say :narrator "Out of pulse ammo."])))

;;; Your ship.

(defcell ship 
  (tile :initform "player-ship-north-shield")
  (name :initform "Olvac 2")
  (speed :initform (make-stat :base 10))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hit-points :initform (make-stat :base 5 :min 0 :max 5))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (trail-length :initform (make-stat :base 12 :min 0))
  (pulse-ammo :initform (make-stat :base 3 :min 0 :max 5))
  (invincibility-clock :initform 0)
  (stepping :initform t)
  (lives :initform (make-stat :min 0 :base 3 :max 3))
  (score :initform (make-stat :base 0))
  (categories :initform '(:actor :player :target :container :light-source))
  (equipment-slots :initform '(:left-bay))
  (boost-clock :initform 0))

(define-method initialize ship ()
  [say *billboard* :go])

(define-method quit ship ()
  (rlx:quit :shutdown))

(define-method run ship ()
  [update-tile self]
  [update-react-shield self]
  [update *status*])	       

(define-method wait ship ()
  [expend-action-points self <action-points>])

(define-method respawn ship ()
  nil)

(define-method activate-pulse-cannon ship ()
  (when (plusp [stat-value self :pulse-ammo])
    [activate [equipment-slot self :left-bay]]
    [stat-effect self :pulse-ammo -1]))

(define-method update-react-shield ship ()
  (when (not (<= <invincibility-clock> 0))
    (decf <invincibility-clock>)
    [>>say :narrator "React shield up with ~D turns remaining." <invincibility-clock>]))

(define-method move ship (direction)
  [update-react-shield self]
  [drop self (clone =trail= 
		    :direction direction 
		    :clock [stat-value self :trail-length])]
  [parent>>move self direction]
  [update-tile self]
  [update *status*])

(define-method update-tile ship ()
  (setf <tile> 
	(if  (plusp <invincibility-clock>)
	     "player-ship-invincible"
	     (ecase [stat-value self :hit-points]
	       (5 "player-ship-north-shield")
	       (4 "player-ship-north-shield")
	       (3 "player-ship-north")
	       (2 "player-ship-north")
	       (1 (prog1 "player-ship-north-dying"
		    [say *billboard* :warning]))
	       (0 "skull")))))
		 
(define-method damage ship (points)
  (if (= 0 <invincibility-clock>)
    (progn (play-sample "warn")
	   [parent>>damage self points]
	   [say *billboard* :react]
	   (setf <invincibility-clock> 5)
	   [update-tile self]
	   [>>say :narrator "React Shield up with 5 turns remaining."])
    (progn 
      [>>say :narrator "React shield blocks 1 damage."]
      (decf <invincibility-clock>)
      [update-tile self])))
  
(define-method loadout ship ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =pulse-cannon=)]])

(define-method step ship (stepper)
  (when (eq =asteroid= (object-parent stepper))
    [say *billboard* :hit]
    [damage self 1]
    [>>say :narrator "You were damaged by a floating asteroid!"]))

(define-method die ship ()
  (play-sample "death")
  [stat-effect self :lives -1]
  (let ((skull (clone =skull= self [stat-value self :lives])))
    [drop-cell *active-world* skull <row> <column> :loadout t :no-collisions nil]
    (setf <action-points> 0)
    [add-category self :dead]
    [>>delete-from-world self]
    [set-player *active-world* skull]))

(define-method revive ship ()
  [say *billboard* :go]
  [drop-cell *active-world* self (random 10) (random 10)]
  [stat-effect self :hit-points 5]	       
  [update-tile self]
  [delete-category self :dead]
  [set-player *active-world* self])

;;; A life crystal powerup.

(defcell diamond
  (tile :initform "diamond"))

(define-method step diamond (stepper)
  (when [is-player stepper]
   (play-sample "powerup")
   [say *billboard* :shield]
   [>>say :narrator "You gain 1 hit point and 2000 score."] 
   [stat-effect stepper :hit-points 1]
   [stat-effect stepper :score 2000]
   [die self]))

;;; A trail extender powerup.

(defcell extender 
  (tile :initform "plus"))

(define-method step extender (stepper)
  (when [is-player stepper]
    (play-sample "powerup")
    [say *billboard* :extend]
    [>>say :narrator "Trail extend!"]
    [stat-effect stepper :trail-length 2]
    [stat-effect stepper :score 2000]
    [die self]))

;;; Extra ammo for pulse protector

(defcell pulse-ammo 
  (tile :initform "pulse-ammo"))

(define-method step pulse-ammo (stepper)
  (when [is-player stepper]
    (play-sample "powerup")
    [say *billboard* :ammo]
    [>>say :narrator "Ammo +2!"]
    [stat-effect stepper :pulse-ammo 2]
    [stat-effect stepper :score 2000]
    [die self]))

;;; Random powerup function

(defun random-powerup ()
  (clone (ecase (random 3)
	   (0 =diamond=)
	   (1 =pulse-ammo=)
	   (2 =extender=))))

;;; Radiation graviceptors

(defcell graviceptor
 (tile :initform "gravicept")
 (hit-points :initform (make-stat :base 3 :max 3 :min 0))
 (speed :initform (make-stat :base 3))
 (strength :initform (make-stat :base 10))
 (defense :initform (make-stat :base 10))
 (stepping :initform t)
 (movement-cost :initform (make-stat :base 10))
 (max-items :initform (make-stat :base 2))
 (direction :initform (random-direction))
 (movement-cost :initform (make-stat :base 10))
 (categories :initform '(:actor :obstacle :enemy :target)))

(define-method run graviceptor ()
  (clon:with-field-values (row column) self
    (let* ((world *active-world*)
	   (direction [direction-to-player world row column]))
      (if [adjacent-to-player world row column]
	  [explode self]
	  (if [obstacle-in-direction-p world row column direction]
	      (let ((target [target-in-direction-p world row column direction]))
		(if (and target (not [in-category target :enemy]))
		    [explode self]
		    (progn (setf <direction> (random-direction))
			   [>>move self direction])))
	      (progn (when (< 7 (random 10))
		       (setf <direction> (random-direction)))
		     [>>move self direction]))))))

(define-method explode graviceptor ()
  ;; only when not in space debris... debris are "safe zones" from mines
  (when (notany #'(lambda (ob)
		    (eq =debris= (object-parent ob)))
		[cells-at *active-world* <row> <column>])
    (labels ((boom (r c &optional (probability 50))
	       (prog1 nil
		 (when (and (< (random 100) probability)
			    [in-bounds-p *active-world* r c])
		   [drop-cell *active-world* (clone =explosion=) r c :no-collisions nil]))))
      (dolist (dir rlx:*compass-directions*)
	(multiple-value-bind (r c)
	    (step-in-direction <row> <column> dir)
	  (boom r c 100)))
      ;; randomly sprinkle some fire around edges
      (trace-rectangle #'boom 
		       (- <row> 2) 
		       (- <column> 2) 
		       5 5)
      [die self])))
  
(define-method damage graviceptor (points)
  (declare (ignore points))
  [stat-effect [get-player *active-world*] :score 5000]
  [>>say :narrator "Graviceptor destroyed. 5000 Bonus Points."]
  [explode self])

;;; A radiation probe releases a trail of toxic graviceptor particles.

(defcell radiation 
  (categories :initform '(:actor))
  (clock :initform 4))
  
(define-method initialize radiation (&key direction clock)
  (setf <clock> clock)
  (setf <tile> (ecase direction
		 (:north "radiation-north")
		 (:south "radiation-south")
		 (:east "radiation-east")
		 (:west "radiation-west")
		 (:northeast "radiation-northeast")
		 (:northwest "radiation-northwest")
		 (:southeast "radiation-southeast")
		 (:southwest "radiation-southwest")
		 (:here "explosion"))))

(define-method run radiation ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (zerop <clock>)
    [die self]))

(define-method damage radiation (points)
  (declare (ignore points))
  [die self])

(define-method die radiation ()
  (when (> 1 (random 230))
    [drop self (clone =graviceptor=)])
  [parent>>die self])

(define-method step radiation (stepper)
  (when (eq =ship= (object-parent stepper))
    [drop self (clone =explosion=)]	       
    [damage stepper 1]))
	   
(defcell probe 
  (tile :initform "probe")
  (hit-points :initform (make-stat :base 3 :max 3 :min 0))
  (speed :initform (make-stat :base 6))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (direction :initform (random-direction))
  (movement-cost :initform (make-stat :base 10))
  (categories :initform '(:actor :obstacle :enemy :target))
  (trail-length :initform (make-stat :base 10)))

(define-method move probe (direction)
  [drop self (clone =radiation= 
		    :direction direction 
		    :clock [stat-value self :trail-length])]
  [parent>>move self direction])

(define-method run probe ()
  (clon:with-field-values (row column) self
    (let* ((world *active-world*)
	   (direction [direction-to-player *active-world* row column])
	   (distance [distance-to-player *active-world* row column]))
      (if (< distance 8)
	  (progn (setf <direction> (if (< distance 4)
				       (random-direction)
				       direction))
		 [>>move self <direction>])
	  ;; bounce around
	  (progn 
	    (when [obstacle-in-direction-p *active-world* <row> <column> <direction>]
	      (setf <direction> (random-direction)))
	    [>>move self <direction>])))))

(define-method die probe ()
  (play-sample "death-alien")
  [say *billboard* :destroy]
  [parent>>die self])

;;; Magnetic space debris can damage you or slow movement

(defcell debris 
;  (categories :initform '(:obstacle))
  (tile :initform "debris"))

(define-method step debris (stepper)
  (when [is-player stepper]	
    (if (> 1 (random 80))
	(progn [>>say :narrator "Your ship is damaged by the space debris!"]
	       [damage stepper 1])
	(progn [>>say :narrator "Your movement is slowed by the space debris."]
	       [expend-action-points stepper 5]))))

(define-method damage debris (points)
  (declare (ignore points))
  [die self])

;;; An asteroid.

(defvar *asteroid-count* 0)

(defcell asteroid
  (categories :initform '(:actor :sticky))
  (hit-points :initform (make-stat :base 1 :min 0))
  (movement-cost :initform (make-stat :base 10))
  (stuck-to :initform nil)
  (direction :initform :north)
  (stepping :initform t))

(define-method is-stuck asteroid ()
  <stuck-to>)

(define-method die asteroid ()
  (decf *asteroid-count*)
  (when (< *asteroid-count* 25 )
    ;; drop more asteroids!!
    [drop-random-asteroids *active-world* 70])
  ;;
  [>>say :narrator "You destroyed an asteroid!"]
  [say *billboard* :destroy]
  (play-sample "bleep")
  (when (< (random 3) 1)
    [drop self (random-powerup)])
  [stat-effect [get-player *active-world*] :score 120]
  (when <stuck-to>
    [unstick <stuck-to> self])
  [parent>>die self])

(define-method initialize asteroid (&key speed direction color)
  (incf *asteroid-count*)
  (setf <speed> (make-stat :base speed))
  (setf <direction> direction)
  (setf <tile>
	(ecase color
	  (:orange "asteroid-orange")
	  (:red "asteroid-red")
	  (:blue "asteroid-blue")
	  (:brown "asteroid-brown"))))

(define-method run asteroid ()
  (when (eq :here <direction>)
    (setf <direction> :north))
  (if (<= [stat-value self :hit-points] 0)
      [die self]
      ;; if free, float
      (if (and (not <stuck-to>)  
	       (not [obstacle-in-direction-p *active-world* <row> <column> <direction>]))
	  [move self <direction>]
	  ;; otherwise bounce (when free)
	  (unless <stuck-to>
	    (setf <direction> (rlx:random-direction))))))

(define-method step asteroid (stepper)
  (when [in-category stepper :player]
    [damage stepper 1]
    [say *billboard* :hit]
    [>>say :narrator "You took a hit!"]
    [die self]))

;;; Polaris collects asteroids

(defcell polaris
  (tile :initform "polaris")
  (asteroids :initform '())
  (categories :initform '(:actor))
  (direction :initform (rlx:random-direction)))

(define-method scan-neighborhood polaris ()
  (dolist (dir *compass-directions*)
    (multiple-value-bind (r c) (rlx:step-in-direction <row> <column> dir)
      (do-cells (cell [cells-at *active-world* r c])
	(when (and cell [in-category cell :sticky])
	  [stick self cell])))))

(define-method change-direction polaris (direction)
  (dolist (asteroid <asteroids>)
    (assert (clon:object-p asteroid))
    (setf (field-value :direction asteroid) direction))
  (setf <direction> direction))

(define-method move-as-group polaris (direction)
  ;; move self first so that nobody steps on us
  [move self direction]
  ;; now move the stuck asteroids
  (dolist (a <asteroids>)
    [move a direction]))

(define-method run polaris ()
  [scan-neighborhood self]	       
  (let ((direction <direction>))	       
    (labels ((obstructed (asteroid)
	       [obstacle-in-direction-p *active-world*
					(field-value :row asteroid)
					(field-value :column asteroid)
					direction]))
      (let ((timeout 8)) 
	(loop while (and (plusp timeout)
			 (or (some #'obstructed <asteroids>)
			     (obstructed self)))
	   do [change-direction self (rlx:random-direction)]
	     (decf timeout))
	(unless (zerop timeout)
	  ;; it's safe. move as a group. 
	  [move-as-group self <direction>])))))
	      
(define-method stick polaris (asteroid)
  (when (and [in-category asteroid :sticky]
	     (not [is-stuck asteroid]))
    (setf (field-value :stuck-to asteroid) self)
    (setf (field-value :direction asteroid) <direction>)
    ;; put it back where it was
    [move asteroid (rlx:opposite-direction (field-value :direction asteroid))]
    (pushnew asteroid <asteroids>)))

(define-method unstick polaris (asteroid)
  (setf <asteroids> (delete asteroid <asteroids>))
  (when (= 0 (length <asteroids>))
    [stat-effect [get-player *active-world*] :score 2000]
    (rlx:play-sample "sweep")
    [say *billboard* :sweep]
    [>>say :narrator "You get 2000 extra points for wiping the polaris mine clean of asteroids."]))

;;; The inescapable game grid.

(define-prototype void-world (:parent rlx:=world=)
  (width :initform 200)
  (height :initform 46)
  (asteroid-count :initform 500)
  (polaris-count :initform 90)
  (probe-count :initform 40)
  (room-count :initform 20)
  (ambient-light :initform :total))

(define-method generate void-world (&optional parameters)
  (declare (ignore parameters))
  (clon:with-field-values (height width) self
    [drop-plasma-space self]
    [drop-random-asteroids self <asteroid-count>]
    [drop-plasma-debris self]
    (dotimes (i <polaris-count>)
      [drop-cell self (clone =polaris=)
		 (random height) (random width)])
    (dotimes (i <probe-count>)
      [drop-cell self (clone =probe=)
		 (random height) (random width)])
    (dotimes (i <room-count>)
      (let ((r (random height))
	    (c (random width)))
	[drop-room self r c]))))
	    
(define-method drop-random-asteroids void-world (count)
  (clon:with-field-values (height width) self
    (dotimes (i count)
      [drop-cell self (clone =asteroid= :speed (+ 3 (random 7))
			     :direction (rlx:random-direction)
			     :color (nth (random 4)
					 '(:red :blue :brown :orange)))
		 (random height) (random width)])))

(define-method drop-plasma-debris void-world ()
  (clon:with-field-values (height width) self
    (let ((plasma (rlx:render-plasma height width :graininess 0.4))
	  (value nil))
      (dotimes (i (- height 10))
	(dotimes (j (- width 10))
	  (setf value (aref plasma i j))
	  (when (< 0 value)
	    (let ((object =debris=))
	      [drop-cell self (clone object) (+ 10 i) (+ 10 j) :no-collisions t])))))))

(define-method drop-plasma-space void-world ()
  (clon:with-field-values (height width) self
    (let ((plasma (rlx:render-plasma height width :graininess 0.1))
	  (value nil))
`      (dotimes (i height)
	(dotimes (j width)
	  (setf value (aref plasma i j))
	  [drop-cell self (clone (if (minusp value)
				     =space= =space2=))
		     i j])))))

(define-method drop-room void-world (row column &key 
                                         (height (+ 3 (random 10)))
					 (width (+ 3 (random 10))))
  (let (rectangle)
    (labels ((collect-point (&rest args)
	       (prog1 nil (push args rectangle))))
      (trace-rectangle #'collect-point row column height width)
      ;; make sure there are openings
      (dotimes (i 3)
	(let ((n (random (length rectangle))))
	  (delete (nth n rectangle) rectangle)))
      (dolist (point rectangle)
	(destructuring-bind (r c) point
	[drop-cell self (clone =wall=) r c :no-collisions t])))))

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

(defparameter *qwerty-keybindings*
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
    ("SPACE" nil "respawn .")
    ("ESCAPE" nil "activate-pulse-cannon .")
    ("Q" (:control) "quit .")))

(defparameter *alternate-qwerty-keybindings*
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
    ("ESCAPE" nil "activate-pulse-cannon .")
    ("SPACE" nil "respawn .")
    ("P" (:control) "quit .")))

;; g c r
;;  \|/
;; h-.-n
;;  /|\ 
;; m w v

(defparameter *dvorak-keybindings*
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
    ("SPACE" nil "respawn .")
    ("ESCAPE" nil "activate-pulse-cannon .")
    ("Q" (:control) "quit .")))

(define-method install-keybindings blast-prompt ()
  (let ((keys (ecase rlx:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:alternate-qwerty *alternate-qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k)))
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *active-world* :timer])])

;;; A shield status and score widget.

(define-prototype status (:parent rlx:=formatter=)
  (character :documentation "The character cell whose status is shown."))

(define-method set-character status (char)
  (setf <character> char))

(define-method update status ()
  [delete-all-lines self]
  (let* ((char <character>)
	 (hits [stat-value char :hit-points])
	 (lives [stat-value char :lives])
	 (ammo [stat-value char :pulse-ammo]))
    [print self " HITS: "]
    (dotimes (i 5)
      [print self "  " 
	     :foreground ".yellow"
	     :background (if (< i hits)
			     ".red"
			     ".gray20")]
      [space self])
    [print self " LIVES: "]
    (dotimes (i 3)
      [print self "  " 
	     :foreground ".yellow"
	     :background (if (< i lives)
			     ".blue"
			     ".gray20")]
      [space self])
    [print self " AMMO: "]
    (dotimes (i 6)
      [print self "  " 
	     :foreground ".yellow"
	     :background (if (< i ammo)
			     ".yellow"
			     ".gray20")]
     [space self])
    [print self "     SCORE: "]
    [print self (format nil "~D" [stat-value char :score])]
    [space self]
    [println self (format nil "    REACT SHIELD TIME: ~D " 
			  (field-value :invincibility-clock char))]))
    

(defvar *status*)

;;; Main program. 

(defun blast ()
  (setf clon:*send-parent-depth* 2)
  (rlx:set-screen-height 600)
  (rlx:set-screen-width 800)
;;  (rlx:set-frame-rate 30)
  (rlx:set-timer-interval 20)
  (rlx:enable-timer)
  (rlx:enable-held-keys 1 15)
  (setf *billboard* (clone =billboard=))
  (let* ((prompt (clone =blast-prompt=))
	 (world (clone =void-world=))
	 (narrator (clone =narrator=))
	 (status (clone =status=))
	 (player (clone =ship=))
	 (viewport (clone =view=)))
    (setf *active-world* world)
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    [set-receiver prompt world]
    ;;
    [create-default-grid world]
    [generate world]
    [set-player world player]
    [drop-cell world player 5 5 :loadout t]
    ;;
    [resize narrator :height 80 :width 800]
    [move narrator :x 0 :y 520]
    [set-narrator world narrator]
    [set-verbosity narrator 0]
    ;;
    [resize status :height 30 :width 700]
    [move status :x 10 :y 10]
    [set-character status player]
    (setf *status* status)
    [update status]
    ;;
    [resize *billboard* :height 20 :width 100]
    [move *billboard* :x 700 :y 0]
   ;;
    (setf (clon:field-value :tile-size viewport) 10)
    [set-world viewport world]
    [resize viewport :height 462 :width 800]
    [move viewport :x 0 :y 40]
    [set-origin viewport :x 0 :y 0 :height 46 :width 80]
    [adjust viewport]
    ;;
    [start world]
    (play-music "technogirl" :loop t)
    (play-sample "go")
    
    (install-widgets prompt status viewport narrator *billboard*)))

(blast)

