(in-package :cons-game)


;;; Bombs!

(defcell bomb
  (tile :initform "bomb")
  (categories :initform '(:item :obstacle :target)))

(defun same-team (obj1 obj2)
  (eq (field-value :team obj1)
      (field-value :team obj2)))

;;; Particle gun

(defcell blue-particle 
  (tile :initform "blueparticle")
  (movement-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 5 :min 0 :max 10))
  (team :initform :player)
  (categories :initform '(:actor))
  (direction :initform :north))

(define-method initialize blue-particle (direction)
  (setf <direction> direction))

(define-method run blue-particle ()
  (multiple-value-bind (r c) (step-in-direction <row> <column> <direction>)
    (let ((thing (or [category-at-p *world* r c :obstacle]
		     [category-at-p *world* r c :target])))
      (if (null thing)
	  [move self <direction>]
	  (progn (when (and (clon:has-method :hit thing)
			    (not (same-team self thing)))
		   [hit thing])
		 [die self])))))

(defcell gun
  agent
  (tile :initform "gun")
  (categories :initform '(:item :target :function)))

(define-method call gun (caller)
  (clon:with-field-values (direction row column) caller
    (multiple-value-bind (r c) (step-in-direction row column direction)
      [play-sample caller "fire"]
      [drop-cell *world* (clone =blue-particle= direction) r c])))


;;; Phonic particles

(defcell particle 
  (tile :initform "particle")
  (direction :initform (car (one-of '(:north :south :east :west))))
  (categories :initform '(:actor))
  (clock :initform (random 20)))

(define-method run particle ()
  (decf <clock>)
  (setf <tile> (car (one-of '("particle" "particle2" "particle3"))))
  ;;[play-sample self "particle-sound-1"]
  (if (minusp <clock>) [die self]
      [move self <direction>]))

;;; Phi particles

(defcell phi
  (tile :initform "phi")
  (direction :initform (car (one-of '(:north :south :east :west))))
  (categories :initform '(:actor))
  (clock :initform (random 20)))

(define-method run phi ()
  (decf <clock>)
  (setf <tile> (car (one-of '("phi" "phi2" "phi3"))))
  ;;[play-sample self "particle-sound-1"]
  (if (minusp <clock>) 
      [die self]
      (progn (percent-of-time 3 [play-sample self (car (one-of '("dtmf1" "dtmf2" "dtmf3")))])
	     [move self <direction>])))

;;; Health powerup

(defcell health 
  (description :initform "Restores a few hit points worth of repair to your tank.")
  (tile :initform "health"))

(define-method step health (stepper)
  (when [is-player stepper]
    [stat-effect stepper :hit-points 7]
    [play-sample self "saddown"]
    [say stepper "Tank repaired 7 hit points."]
    [die self]))

;;; Shield

(defcell shield
  (tile :initform "shield")
  (description :initform "Wave shield blocks sound waves.")
  (team :initform :neutral)
  (default-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 20))
  (hit-points :initform (make-stat :base 5 :min 0))
  (categories :initform '(:actor :target)))

(define-method hit shield (&optional wave)
  (when [in-category wave :wave]
    [play-sample self "ice"]
    [damage self 1]))

(define-method run shield () nil)

;;; White noise

(defcell noise 
  (tile :initform (car (one-of '("white-noise" "white-noise2" "white-noise3" "white-noise4"))))
  (categories :initform '(:actor))
  (clock :initform (random 20)))

(define-method run noise ()
  (decf <clock>)
  [play-sample self "noise-white"]
  (if (minusp <clock>) [die self]
      [move self (random-direction)]))

