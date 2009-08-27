(in-package :blast)

;;; enemy ships a la tac scan

(define-prototype xr7 (:parent =rook=)
  (name :initform "XR-7 Phalanx Interdictor")
  (tile :initform "xr7")
  (speed :initform (make-stat :base 5))
  (description :initform "The deadly XR7 can fire lasers from a distance."))

(define-method fire xr7 (direction)
  [expend-action-points self 15]
  (let* ((world *active-world*)
	 (player [get-player *active-world*]))
    (labels ((draw-beam (image)
	       (multiple-value-bind (x0 y0) 
		   [screen-coordinates self]
		 (multiple-value-bind (x1 y1)
		     [screen-coordinates player]
		   (rlx:draw-line x0 y0 x1 y1 
				  :destination image)))))
      [damage player 2]
      [say self "You sustain 2 damage from the laser."]
      [play-sample self "laser2"]
      [>>add-overlay :viewport #'draw-beam])))

(define-method die xr7 ()
  [drop self (clone (if (= 0 (random 2))
			=energy= =crystal=))]
  [delete-from-world self])

(define-method seek xr7 ()
  (clon:with-field-values (row column) self
    (when (< [distance-to-player *active-world* row column] <chase-distance>)
      (let ((direction [direction-to-player *active-world* row column])
	    (world *active-world*))
	(if (< [distance-to-player self] 8)
	    (progn
	      [>>fire self direction]
	      (setf <clock> 6
		    <behavior> :fleeing))
	    (if [obstacle-in-direction-p world row column direction]
		(let ((target [target-in-direction-p world row column direction]))
		  (if (and target (not [in-category target :enemy]))
		      (progn nil)
;;			[>>attack self direction]
		      (progn (setf <direction> (random-direction))
			     [>>move self direction])))
		(progn (when (< 7 (random 10))
			 (setf <direction> (random-direction)))
		       [>>move self direction])))))))

;;; bresenham's rail gun

(defcell rail-trail 
  (tile :initform "ring")
  (categories :initform '(:ephemeral :actor))
  (clock :initform 6))

(define-method run rail-trail ()
  (decf <clock>)
  (when (minusp <clock>)
    [die self]))

(define-prototype rail-particle (:parent =muon-particle=)
  (path :initform nil)
  (clock :initform 60)
  (speed :initform (make-stat :base 6))
  (categories :initform '(:actor :target))
  (tile :initform "rail-particle"))

(define-method orient rail-particle ()
  (if <path>
      (destructuring-bind (row column) (pop <path>)
	(setf <direction> (rlx:direction-to <row> <column> row column)))
      [die self]))

(define-method impel rail-particle (row column)
  (let (path)
    (labels ((collect-point (x y)
	       (prog1 nil (push (list y x) path))))
      ;; (trace-line #'collect-point <row> <column> row column)
      ;; (when (equal (list row column) (first path))
      ;; 	(setf path (nreverse path)))
      ;; (setf <path> path)
      ;; [orient self]
      (setf <direction> (direction-to <row> <column> row column))
      [move self <direction>]
      [drop-trail self nil]
      [find-target self])))

(define-method run rail-particle ()
  [find-target self]
  (decf <clock>)
  (when (zerop <clock>)
    [die self])
  ;; (setf <direction> (direction-to <row> <column> [player-row *active-world*]
  ;; 				  [player-column *active-world*]))
  [drop-trail self nil]
  [move self <direction>])

(define-method drop-trail rail-particle (direction)
  (declare (ignore direction))
  [drop self (clone =rail-trail=)])

(defcell rail-cannon
  (name :initform "Rail gun cannon")
  (tile :initform "gun")
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:center-bay))
  (weight :initform 7000)
  (accuracy :initform (make-stat :base 100))
  (attack-power :initform (make-stat :base 18))
  (attack-cost :initform (make-stat :base 40))
  (energy-cost :initform (make-stat :base 1)))

(define-method fire rail-cannon (row column)
  [expend-action-points <equipper> [stat-value self :attack-cost]]
  (if [expend-energy <equipper> [stat-value self :energy-cost]]
      (let ((particle (clone =rail-particle=)))
	[play-sample <equipper> "bip"]
	[drop <equipper> particle]
	[impel particle row column])
      [say self "Not enough energy to fire!"]))

;;; the eyeboss

(defparameter *guardic-eye-open-time* 5)
(defparameter *guardic-eye-closed-time* 8)

(defcell guardic-eye
  (name :initform "Guardic eye")
  (tile :initform "guardic")
  (hit-points :initform (make-stat :base 4 :max 4 :min 0))
  (open :initform nil)
  (clock :initform (random *guardic-eye-closed-time*))
  (speed :initform (make-stat :base 6))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (energy :initform (make-stat :base 100 :min 0 :max 100))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 6))
  (equipment-slots :initform '(:center-bay))
  (firing-with :initform :center-bay)
  (max-items :initform (make-stat :base 2))
  (categories :initform '(:actor :obstacle :enemy :target))
  (description :initform "Invulnerable until red eye opens. Fires particle weapons."))

(define-method loadout guardic-eye ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =rail-cannon=)]])

(define-method run guardic-eye ()
  ;; open or close eye
  (decf <clock>)
  (if (zerop <clock>)
      (if <open>
	  (progn
	    (setf <open> nil)
	    (setf <tile> "guardic")
	    (setf <clock> *guardic-eye-closed-time*))
	  (progn
	    (setf <open> t)
	    (setf <tile> "guardic-open")
	    (setf <clock> *guardic-eye-open-time*))))
  ;; attack!
  (if (< [distance-to-player self] 20)
      (let ((cannon [equipment-slot self :center-bay]))
	[expend-default-action-points self]
	(when <open> [fire cannon [player-row *active-world*]
			   [player-column *active-world*]]))))
					  
(define-method damage guardic-eye (points)
  ;; only damage when open
  (if <open>
    [parent>>damage self points]
    [say self "Cannot damage closed eye."]))

(defcell guardic 
  (name :initform "Electric eye")
  (tile :initform "godseye"))
  
;;; the bases

(defcell vomac-base 
  (tile :initform "vomac-base")
  (description :initform "Platform for Guardic eye bases."))

(define-method explode vomac-base ()
  [drop self (clone =explosion=)]
  [die self])

(defcell vomac-wires 
  (tile :initform "vomac-wires")
  (description :initform "Deadly live defense wires."))

(define-method step vomac-wires (stepper)
  (when [is-player stepper]
    [play-sample self "spawn"]
    [damage stepper 5]
    [say self "You are shocked by the guard wires!"]))

;;; the vomac ship

(define-prototype vomac (:parent =olvac=)
  (tile :initform "vomac")
  (mode :initform :vehicle)
  (name :initform "Vomac XLUX Fighter")
  (last-direction :initform :here)
  (speed :initform (make-stat :base 9 :min 0 :max 25))
  (strength :initform (make-stat :base 12))
  (defense :initform (make-stat :base 15))
  (hearing-range :initform 15)
  (energy :initform (make-stat :base 70 :min 0 :max 70 :unit :gj))
  (pollen3a :initform (make-stat :base 0 :min 0 :max 30 :unit :kg))
  (endurium :initform (make-stat :base 70 :min 0 :max 140 :unit :kg))
  (technetium :initform (make-stat :base 0 :min 0 :unit :ug))
  (biosilicate :initform (make-stat :base 0 :min 0 :unit :g))
  (hit-points :initform (make-stat :base 70 :min 0 :max 70))
  (movement-cost :initform (make-stat :base 8))
  (max-items :initform (make-stat :base 2))
  (trail-length :initform (make-stat :base 12 :min 0))
  (bomb-ammo :initform (make-stat :base 10 :min 0 :max 10))
  (oxygen :initform (make-stat :base 200 :min 0 :max 200))
  (invincibility-clock :initform 0)
  (stepping :initform t)
  (attacking-with :initform nil)
  (firing-with :initform :center-bay)
  (categories :initform '(:actor :player :target :container :light-source :vehicle :repairable))
  (equipment-slots :initform '(:left-bay :right-bay :center-bay :extension))
  (boost-clock :initform 0)
  (description :initform 
"The Vomac XLUX Fighter is Arch Gamma's newest mid-range fighter model
with 8-way fire and heavy armor."))

(define-method damage vomac (points)
  [play-sample self "vomac-damage"]
  [parent>>damage self points])

(define-method update-tile vomac ()
  nil)

(define-method drop-trail vomac ()
  nil)

(define-prototype defleptor-trail (:parent =muon-trail=)
  (speed :initform (make-stat :base 20))
  (tile :initform "defleptor-trail")
  (clock :initform 3))

(define-method initialize defleptor-trail ()
  (setf <direction> :north))

(define-method run defleptor-trail ()
  [expend-default-action-points self]
  (decf <clock>)
  (when (minusp <clock>)
    [die self]))

(define-prototype defleptor-wave (:parent =muon-particle=)
  (name :initform "Defleptor wave")
  (speed :initform (make-stat :base 70))
  (tile :initform "defleptorwave")
  (clock :initform 20))

(define-method update-tile defleptor-wave ()
  (setf <tile> (case <direction> 
		 (:north "defleptorwave")
		 (:south "defleptorwave-south")
		 (:west "defleptorwave-west")
		 (:east "defleptorwave-east")
		 (:northeast "defleptorwave-northeast")
		 (:northwest "defleptorwave-northwest")
		 (:southeast "defleptorwave-southeast")
		 (:southwest "defleptorwave-southwest")
		 (otherwise ".gear"))))
  
(define-method drop-trail defleptor-wave (direction)
  (declare (ignore direction))
  [drop self (clone =defleptor-trail=)])

(define-prototype vomac-cannon (:parent =muon-cannon=)
  (name :initform "Vomac defleptor wave cannon")
  (energy-cost :initform (make-stat :base 1))
  (tile :initform "defleptorwave"))

(define-method fire vomac-cannon (direction)
  (if [expend-energy <equipper> [stat-value self :energy-cost]]
      (let (wave)
	(dolist (dir (delete :here rlx:*compass-directions*))
	  (setf wave (clone =defleptor-wave=))
	  [drop <equipper> wave]
	  [play-sample <equipper> "defleptor3"]
	  [impel wave dir])
	[expend-default-action-points self])
      [say <equipper> "Not enough energy to fire!"]))

(define-method loadout vomac ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =vomac-cannon=)]])

;;; The vaxodrones

(defcell vaxodrone 
  (name :initform "VAXodrone")
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
  (equipment-slots :initform '(:robotic-arm))
  (speed :initform (make-stat :base 7 :min 7))
  (max-items :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 3))
  (tile :initform "vaxodrone")
  (stepping :initform t)
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (direction :initform (rlx:random-direction))
  (strength :initform (make-stat :base 4 :min 0 :max 30))
  (dexterity :initform (make-stat :base 5 :min 0 :max 30))
  (intelligence :initform (make-stat :base 11 :min 0 :max 30))
  (hit-points :initform (make-stat :base 10 :min 0 :max 10))
  (description :initform "Swarms of vaxodrones will trap and kill you."))
 
(define-method run vaxodrone ()
  (when (< [distance-to-player self] 18)
    (let ((dir [direction-to-player self]))
      (if (= 0 (random 2))
	  [move self dir]
	  [move self (random-direction)])
      (when [adjacent-to-player self]
	[play-sample self "scree"]
	[attack self dir]))))

(define-method die vaxodrone ()
  [play-sample self "aagh2"]
  [parent>>die self])

(define-method loadout vaxodrone ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =shock-probe=)]])

;;; an arena for vomac combat

(defcell vomac-starfield 
  (name :initform "Star corridor")
  (tile :initform "vomac-starfield"))

(defcell vomac-starfield2
  (name :initform "Star corridor with turbulence")
  (tile :initform "vomac-starfield2"))

(define-prototype star-corridor (:parent rlx:=world=)
  (ambient-light :initform :total)
  (required-modes :initform '(:vomac :vehicle :spacesuit))
  (scale :initform '(100 m))
  (edge-condition :initform :block))

(define-method begin-ambient-loop star-corridor ()
  (play-music "corridor-music" :loop t))

(define-method drop-base star-corridor (row column &optional (size 5))
  (labels ((drop-panel (r c)
	     (prog1 nil [drop-cell self (clone =vomac-base=) r c])))
    (trace-rectangle #'drop-panel row column size size :fill)
    (dotimes (i 8)
      [drop-cell self (clone =guardic-eye=) 
		 (+ row (random size)) (+ column (random size)) :loadout t])
    (dotimes (i (* 2 size))
      [drop-cell self (clone =vomac-wires=)
		 (+ row (random size)) (+ column (random size))])))

(define-method generate star-corridor (&key (height 200)
					    (width 30)
					    sequence-number)
  (setf <height> height <width> width)
  [create-default-grid self]
  (dotimes (i height)
    (dotimes (j width)
      [drop-cell self (clone (if (zerop (random 7))
				 =vomac-starfield2= 
				 =vomac-starfield=))
		 i j]))
  (dotimes (i 20)
    [drop-cell self (clone =star=) (random height) (random width)]
    [drop-base self (random height) (random width) (+ 5 (random 10))])
  (dotimes (i 20)
    (let ((gond (clone =gond=))
	  (xr7 (clone =xr7=))
	  (row (random height))
	  (column (random width)))
      [drop-cell self xr7 row column :loadout t]
      [drop-cell self gond (+ 1 row) (+ 1 column) :loadout t]
      (dotimes (i 5)
	[drop-cell self (clone =vaxodrone=) row column])
      [defend gond xr7]))
  [drop-cell self (clone =vomac=) (- height 5) 2 :loadout t]
  [drop-cell self (clone =launchpad=) (- height 8) 5])


