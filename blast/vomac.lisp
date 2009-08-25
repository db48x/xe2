(in-package :blast)

;;; enemy ships a la tac scan

(define-prototype xr7 (:parent =rook=)
  (name :initform "XR-7 Phalanx Interdictor")
  (tile :initform "xr7"))

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

;;; the vomac ship

(define-prototype vomac (:parent =olvac=)
  (tile :initform "vomac")
  (mode :initform :vomac)
  (name :initform "Vomac XLUX Fighter")
  (last-direction :initform :here)
  (speed :initform (make-stat :base 9 :min 0 :max 25))
  (strength :initform (make-stat :base 12))
  (defense :initform (make-stat :base 15))
  (hearing-range :initform 18)
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
  (boost-clock :initform 0))

(define-method damage vomac (points)
  [stat-effect self :hit-points (- points)]
  (when (zerop [stat-value self :hit-points])
    [play-sample self "aagh"]
    [die self]))
    

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
		 (:southwest "defleptorwave-southwest")
		 (:southeast "defleptorwave-southeast")
		 (otherwise ".gear"))))
  
(define-method drop-trail defleptor-wave (direction)
  (declare (ignore direction))
  [drop self (clone =defleptor-trail=)])

(define-prototype vomac-cannon (:parent =muon-cannon=)
  (name :initform "Vomac defleptor wave cannon")
  (tile :initform "defleptorwave"))

(define-method fire vomac-cannon (direction)
  (if [expend-energy <equipper> [stat-value self :energy-cost]]
      (let ((wave1 (clone =defleptor-wave=))
	    (wave2 (clone =defleptor-wave=))
	    (wave3 (clone =defleptor-wave=)))
	[play-sample <equipper> "dtmf1"]
	[drop <equipper> wave1]
	[drop <equipper> wave2]
	[drop <equipper> wave3]
	[impel wave1 :north]
	[impel wave2 :southeast]
	[impel wave3 :southwest]
	[expend-default-action-points self])
      [say self "Not enough energy to fire!"]))

(define-method loadout vomac ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =vomac-cannon=)]])
 
;;; an arena for vomac combat

(defcell vomac-starfield 
  (name :initform "Star corridor")
  (tile :initform "vomac-starfield"))

(defcell vomac-starfield2
  (name :initform "Star corridor with turbulence")
  (tile :initform "vomac-starfield2"))

(define-prototype star-corridor (:parent rlx:=world=)
  (ambient-light :initform :total)
  (required-modes :initform '(:vomac :spacesuit))
  (scale :initform '(100 m))
  (edge-condition :initform :block))

(define-method begin-ambient-loop star-corridor ()
  (play-music "corridor-music" :loop t))

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
    [drop-cell self (clone =star=) (random height) (random width)])
  (dotimes (i 20)
    (let ((gond (clone =gond=))
	  (xr7 (clone =xr7=)))
      [drop-cell self xr7 (random height) (random width) :loadout t]
      [drop-cell self gond (random (truncate (/ height 2))) (random width) :loadout t]
      [defend gond xr7]))
  [drop-cell self (clone =vomac=) (- height 5) 2 :loadout t]
  [drop-cell self (clone =launchpad=) (- height 8) 5])


