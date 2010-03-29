(in-package :cons-game)

(defun same-team (obj1 obj2)
  (eq (field-value :team obj1)
      (field-value :team obj2)))

;;; Glittering flash gives clues on locations of explosions/damage

(defcell flash 
  (clock :initform 2)
  (tile :initform "flash-1")
  (categories :initform '(:actor))
  (speed :initform (make-stat :base 1)))

(define-method run flash ()
  [expend-action-points self 10]
  (case <clock>
    (1 (setf <tile> "flash-2"))
    (0 [>>die self]))
  (decf <clock>))

;;; Sparkle is a bigger but faster flash.

(defcell sparkle 
  (clock :initform 1)
  (tile :initform "sparkle")
  (categories :initform '(:actor))
  (speed :initform (make-stat :base 1)))

(define-method run sparkle ()
  [expend-action-points self 20]
  (case <clock>
    (1 (setf <tile> "sparkle"))
    (0 [die self]))
  (decf <clock>))

;;; An explosion.

(defcell explosion 
  (name :initform "Explosion")
  (categories :initform '(:actor))
  (tile :initform "explosion")
  (speed :initform (make-stat :base 4))
  (damage-per-turn :initform 10)
  (clock :initform 6))

(define-method run explosion ()
  (if (zerop <clock>)
      [die self]
      (progn
	(setf <tile> (car (one-of '("explosion" "explosion2"))))
	(percent-of-time 30 [play-sample self "crunch"])
	(decf <clock>)
	(percent-of-time 80 [move self (random-direction)])
	[expend-action-points self 10]
	(xe2:do-cells (cell [cells-at *world* <row> <column>])
	  [damage cell <damage-per-turn>]))))

;;; Particle gun

(defcell buster-particle 
  (tile :initform "blueparticle")
  (movement-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 5 :min 0 :max 10))
  (team :initform :player)
  (categories :initform '(:actor))
  (direction :initform :north))

(define-method initialize buster-particle (direction)
  (setf <direction> direction))

(define-method run buster-particle ()
  (multiple-value-bind (r c) (step-in-direction <row> <column> <direction>)
    (let ((obs [obstacle-at-p *world* r c]))
      (if obs
	  (cond ((eq t obs)
		 ;; out of bounds.
		 [die self])
		((clon:object-p obs)
		 ;; hit it
		 (let ((thing (or [category-at-p *world* r c :target] obs)))
		   (if (null thing)
		       [move self <direction>]
		       (progn (when (and (clon:has-method :hit thing)
					 (not (same-team self thing)))
				[drop self (clone =flash=)]
				[hit thing])
			      [die self])))))
	  [move self <direction>]))))

(defcell buster-defun
  agent
  (tile :initform "buster")
  (categories :initform '(:item :target :defun)))

(define-method call buster-defun (caller)
  (clon:with-field-values (direction row column) caller
    [play-sample caller "fire"]
    [drop-cell *world* (clone =buster-particle= direction) row column]))

;;; A bomb with countdown display.

(defvar *bomb-tiles* '("bomb-1" "bomb-2" "bomb-3" "bomb-4"))

(defun bomb-tile (n)
  (nth (- n 1) *bomb-tiles*))

(defcell bomb 
  (categories :initform '(:actor))
  (clock :initform 4)
  (speed :initform (make-stat :base 1))
  (tile :initform (bomb-tile 4)))

(define-method run bomb () 
  (clon:with-fields (clock) self	       
    (if (zerop clock) 
	[explode self]
	(progn 
	  [expend-action-points self 30]		    
	  (setf <tile> (bomb-tile clock))
	  (decf clock)))))

(define-method explode bomb ()  
  (labels ((boom (r c &optional (probability 50))
	     (prog1 nil
	       (when (and (< (random 100) probability)
			  [in-bounds-p *world* r c])
		 [drop-cell *world* (clone =explosion=) r c :no-collisions nil])))
	   (damage (r c &optional (probability 100))
	     (prog1 nil
	       (when (and (< (random 100) probability)
			  [in-bounds-p *world* r c])
		 (do-cells (cell [cells-at *world* r c])
		   (if [is-player cell]
		       [damage cell 4]
		       [hit cell self]))))))
    (dolist (dir xe2:*compass-directions*)
      (multiple-value-bind (r c)
	  (step-in-direction <row> <column> dir)
	(boom r c 100)))
    ;; randomly sprinkle some fire around edges
    (trace-rectangle #'boom 
		     (- <row> 2) 
		     (- <column> 2) 
		     5 5)
    ;; definitely damage everything in radius
    (trace-rectangle #'damage
		     (- <row> 2) 
		     (- <column> 2) 
		     5 5 :fill)
    (dotimes (n (+ 10 (random 10)))
      [drop self (clone =plasma=)])
    (labels ((do-circle (image)
	       (prog1 t
		 (multiple-value-bind (x y) 
		     [screen-coordinates self]
		   (let ((x0 (+ x 8))
			 (y0 (+ y 8)))
		     (draw-circle x0 y0 40 :destination image)
		     (draw-circle x0 y0 35 :destination image))))))
      [>>add-overlay :viewport #'do-circle])
    [die self]))

(defcell bomb-defun
  (tile :initform "bomb-ammo")
  (categories :initform '(:item :target :defun)))

(define-method call bomb-defun (caller)
  (clon:with-field-values (direction row column) caller
    (multiple-value-bind (r c) (step-in-direction row column direction)
      [play-sample caller "fire"]
      [drop-cell *world* (clone =bomb=) r c]
      [expend-item caller])))

;;; Bomb cannon

(defcell bomb-cannon
  (categories :initform '(:item :weapon :equipment))
  (attack-cost :initform (make-stat :base 5))
  (weight :initform 3000)
  (equip-for :initform '(:right-bay :robotic-arm)))

(define-method activate bomb-cannon ()
  ;; leave bomb on top of ship
  (clon:with-field-values (row column) <equipper>
    [drop-cell *world* (clone =bomb=) row column]))

(define-method fire bomb-cannon (direction)
  (clon:with-field-values (last-direction row column) <equipper>
    (multiple-value-bind (r c) 
	(step-in-direction row column direction)
      [drop-cell *world* (clone =bomb=) r c])))

;;; The exploding mine

(defcell mine 
  (name :initform "Proximity mine")
  (categories :initform '(:item :target :actor :hidden))
  (tile :initform "mine")
  (description :initform "If you get near it, it will probably explode."))

(defvar *mine-warning-sensitivity* 5)
(defvar *mine-explosion-sensitivity* 3)

(define-method run mine ()
  (let ((distance [distance-to-player *world* <row> <column>]))
    (if (< distance *mine-warning-sensitivity*)
	(progn
	  (when (string= <tile> "mine")
	    [>>say :narrator "You see a mine nearby!"])
	  (setf <tile> "mine-warn")
	  (when (< distance *mine-explosion-sensitivity*)
	    (when (< (random 8) 1)
	      [explode self])))
	(setf <tile> "mine"))))

(define-method explode mine ()
  (labels ((boom (r c &optional (probability 50))
	     (prog1 nil
	       (when (and (< (random 100) probability)
			  [in-bounds-p *world* r c])
		 [drop-cell *world* (clone =explosion=) r c :no-collisions nil]))))
    (dolist (dir xe2:*compass-directions*)
      (multiple-value-bind (r c)
	  (step-in-direction <row> <column> dir)
	(boom r c 100)))
    ;; randomly sprinkle some fire around edges
    (trace-rectangle #'boom 
		     (- <row> 2) 
		     (- <column> 2) 
		     5 5)
    [die self]))

(define-method step mine (stepper)
  (when [is-player stepper]	      
    [explode self]))

(define-method damage mine (damage-points)
  (declare (ignore damage-points))
  [explode self])

;;; Muon particles, trails, and pistols

(defvar *muon-tiles* '(:north "muon-north"
		       :south "muon-south"
		       :east "muon-east"
		       :west "muon-west"
		       :northeast "muon-northeast"
		       :southeast "muon-southeast"
		       :southwest "muon-southwest"
		       :northwest "muon-northwest"))

(defvar *trail-middle-tiles* '(:north "bullet-trail-middle-north"
			       :south "bullet-trail-middle-south"
			       :east "bullet-trail-middle-east"
			       :west "bullet-trail-middle-west"
			       :northeast "bullet-trail-middle-northeast"
			       :southeast "bullet-trail-middle-southeast"
			       :southwest "bullet-trail-middle-southwest"
			       :northwest "bullet-trail-middle-northwest"))

(defvar *trail-end-tiles* '(:north "bullet-trail-end-north"
			       :south "bullet-trail-end-south"
			       :east "bullet-trail-end-east"
			       :west "bullet-trail-end-west"
			       :northeast "bullet-trail-end-northeast"
			       :southeast "bullet-trail-end-southeast"
			       :southwest "bullet-trail-end-southwest"
			       :northwest "bullet-trail-end-northwest"))

(defvar *trail-tile-map* (list *trail-end-tiles* *trail-middle-tiles* *trail-middle-tiles*))

(defcell muon-trail
  (categories :initform '(:actor))
  (clock :initform 2)
  (speed :initform (make-stat :base 10))
  (default-cost :initform (make-stat :base 10))
  (tile :initform ".gear")
  (direction :initform :north))

(define-method orient muon-trail (direction)
  (setf <direction> direction)
  (setf <tile> (getf *trail-middle-tiles* direction)))

(define-method run muon-trail ()
  (setf <tile> (getf (nth <clock> *trail-tile-map*)
		     <direction>))
  [expend-default-action-points self]
  (decf <clock>)
  (when (minusp <clock>)
    [die self]))

;;; Basic muon particle

(defcell muon-particle 
  (categories :initform '(:actor :muon :target))
  (speed :initform (make-stat :base 22))
  (default-cost :initform (make-stat :base 3))
  (attack-power :initform 5)
  (tile :initform "muon")
  (firing-sound :initform "dtmf2")
  (direction :initform :here)
  (clock :initform 12))

(define-method initialize muon-particle (&key attack-power)
  (when attack-power
    (setf <attack-power> attack-power)))

(define-method drop-trail muon-particle (direction)
  (let ((trail (clone =muon-trail=)))
    [orient trail direction]
    [drop self trail]))

(define-method find-target muon-particle ()
  (let ((target [category-in-direction-p *world* 
					 <row> <column> <direction>
					 '(:obstacle :target)]))
    (if target
	(progn
	  [>>move self <direction>]
	  [>>expend-default-action-points self]
	  [>>drop target (clone =flash=)]
	  [>>push target <direction>]
	  [>>damage target <attack-power>]
	  [>>die self])
	(multiple-value-bind (r c) 
	    (step-in-direction <row> <column> <direction>)
	  (if (not (array-in-bounds-p (field-value :grid *world*) r c))
	      [die self]
	      (progn [drop-trail self <direction>]
		     [>>move self <direction>]))))))

(define-method step muon-particle (stepper)
  [damage stepper <attack-power>]
  [die self])
  
(define-method update-tile muon-particle ()
  (setf <tile> (getf *muon-tiles* <direction>)))

(define-method run muon-particle ()
  [update-tile self]
  [find-target self]
  (decf <clock>)
  (when (zerop <clock>)
    [>>die self]))

(define-method impel muon-particle (direction)
  (assert (member direction *compass-directions*))
  (setf <direction> direction)
  ;; don't hit the player
  ;;  [move self direction]
  [play-sample self <firing-sound>]
  [find-target self])

;;; Beta-muons

(define-prototype beta-muon (:parent =muon-particle=)
  (speed :initform (make-stat :base 24))
  (attack-power :initform 8)
  (firing-sound :initform "dtmf3")
  (tile :initform "beta-muon")
  (clock :initform 15))
  
(defvar *beta-muon-tiles* '(:north "beta-muon-north"
			    :south "beta-muon-south"
			    :east "beta-muon-east"
			    :west "beta-muon-west"
			    :northeast "beta-muon-northeast"
			    :southeast "beta-muon-southeast"
			    :southwest "beta-muon-southwest"
			    :northwest "beta-muon-northwest"))

(define-method update-tile beta-muon ()
  (setf <tile> (getf *beta-muon-tiles* <direction>)))

;;; Muon cannon

(defcell muon-cannon
  (name :initform "Muon energy cannon")
  (tile :initform "gun")
  (ammo :initform =muon-particle=)
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:center-bay))
  (weight :initform 7000)
  (accuracy :initform (make-stat :base 100))
  (attack-power :initform (make-stat :base 12))
  (attack-cost :initform (make-stat :base 10))
  (energy-cost :initform (make-stat :base 1)))

(define-method change-ammo muon-cannon (ammo)
  (assert (clon:object-p ammo))
  (setf <ammo> ammo))

(define-method fire muon-cannon (direction)
  (if [expend-energy <equipper> [stat-value self :energy-cost]]
      (let ((bullet (clone <ammo>)))
	[>>drop <equipper> bullet]
	[>>impel bullet direction])
      [say <equipper> "Not enough energy to fire!"]))

(define-method step muon-cannon (stepper)
  (when [is-player stepper]
    [>>take stepper :direction :here :category :item]))

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
  (direction :initform (car (one-of '(:north :northeast :northwest :southeast :southwest :south :east :west))))
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
  (description :initform "Restores a few hit points when used.")
  (tile :initform "health")
  (categories :initform '(:item)))

(define-method call health (caller)
  (when [is-player caller]
    [stat-effect stepper :hit-points 6]
    [play-sample self "buzzfan"]
    [say stepper "Recovered 6 hit points."]
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

;;; Radioactive gas

(defcell gas
  (tile :initform "rad")
  (name :initform "Radioactive Gas")
  (clock :initform 8)
  (categories :initform '(:actor))
  (description :initform "Spreading toxic radioactive gas. Avoid at all costs!"))

(define-method step gas (stepper)
  (when [is-player stepper]
    [damage stepper 5]
    [>>say :narrator "RADIOACTIVE HAZARD!"]))

(define-method run gas ()
  [play-sample self "gas-poof"]
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      (progn 
	(do-cells (cell [cells-at *world* <row> <column>])
	  (when [is-player cell]
	    [damage cell 5]
	    [>>say :narrator "RADIOACTIVE HAZARD!"]))
	[move self (random-direction)])))

;;; A melee weapon: the Shock Probe

(defcell shock-probe 
  (name :initform "Shock probe")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "shock-probe")
  (attack-power :initform (make-stat :base 5))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (stepping :initform t)
  (weight :initform 3000)
  (equip-for :initform '(:robotic-arm :left-hand :right-hand)))

(define-prototype shock-prod (:parent =shock-probe=)
  (name :initform "Shock prod")
  (attack-power :initform (make-stat :base 7))
  (attack-cost :initform (make-stat :base 12))
  (accuracy :initform (make-stat :base 80)))
  
;;; Lepton Seeker Cannon

(defvar *lepton-tiles* '(:north "lepton-north"
		       :south "lepton-south"
		       :east "lepton-east"
		       :west "lepton-west"
		       :northeast "lepton-northeast"
		       :southeast "lepton-southeast"
		       :southwest "lepton-southwest"
		       :northwest "lepton-northwest"))

(defvar *lepton-trail-middle-tiles* '(:north "bullet-trail-middle-thin-north"
			       :south "bullet-trail-middle-thin-south"
			       :east "bullet-trail-middle-thin-east"
			       :west "bullet-trail-middle-thin-west"
			       :northeast "bullet-trail-middle-thin-northeast"
			       :southeast "bullet-trail-middle-thin-southeast"
			       :southwest "bullet-trail-middle-thin-southwest"
			       :northwest "bullet-trail-middle-thin-northwest"))

(defvar *lepton-trail-end-tiles* '(:north "bullet-trail-end-thin-north"
			       :south "bullet-trail-end-thin-south"
			       :east "bullet-trail-end-thin-east"
			       :west "bullet-trail-end-thin-west"
			       :northeast "bullet-trail-end-thin-northeast"
			       :southeast "bullet-trail-end-thin-southeast"
			       :southwest "bullet-trail-end-thin-southwest"
			       :northwest "bullet-trail-end-thin-northwest"))

(defvar *lepton-trail-tile-map* (list *lepton-trail-end-tiles* *lepton-trail-middle-tiles* *lepton-trail-middle-tiles*))

(define-prototype lepton-trail (:parent xe2:=cell=)
  (categories :initform '(:actor))
  (clock :initform 2)
  (speed :initform (make-stat :base 10))
  (default-cost :initform (make-stat :base 10))
  (tile :initform ".gear")
  (direction :initform :north))

(define-method initialize lepton-trail (direction)
  (setf <direction> direction)
  (setf <tile> (getf *lepton-trail-middle-tiles* direction)))

(define-method run lepton-trail ()
  (setf <tile> (getf (nth <clock> *lepton-trail-tile-map*)
		     <direction>))
  [expend-default-action-points self]
  (decf <clock>)
  (when (minusp <clock>)
    [die self]))

(define-prototype lepton-particle (:parent xe2:=cell=)
  (categories :initform '(:actor :target))
  (speed :initform (make-stat :base 8))
  (stepping :initform t)
  (hit-damage :initform (make-stat :base 7))
  (default-cost :initform (make-stat :base 2))
  (hit-points :initform (make-stat :base 5))
  (movement-cost :initform (make-stat :base 4))
  (tile :initform "lepton")
  (direction :initform :here)
  (clock :initform 10))

(define-method find-target lepton-particle ()
  (let ((target [category-in-direction-p *world* 
					 <row> <column> <direction>
					 '(:obstacle :target)]))
    (if target
	(progn	
	  [drop target (clone =sparkle=)]
	  [damage target [stat-value self :hit-damage]]
	  [play-sample target "serve"]
	  (labels ((do-circle (image)
		     (prog1 t
		       (multiple-value-bind (x y) 
			   [screen-coordinates self]
			 (let ((x0 (+ x 8))
			       (y0 (+ y 8)))
			   (draw-circle x0 y0 40 :destination image)
			   (draw-circle x0 y0 35 :destination image))))))
	    [>>add-overlay :viewport #'do-circle])
	  [die self])
	(progn 
	  [drop self (clone =lepton-trail= <direction>)]
	  [move self <direction>]))))

(define-method update-tile lepton-particle ()
  (setf <tile> (getf *lepton-tiles* <direction>)))
  
(define-method run lepton-particle ()
  [update-tile self]
  (clon:with-field-values (row column) self
    (let* ((world *world*)
	   (direction [direction-to-player *world* row column]))
      (setf <direction> direction)
      [find-target self])
    (decf <clock>)
    (when (and (zerop <clock>) 
	       (not [in-category self :dead]))
      [>>die self])))

(define-method damage lepton-particle (points)
  (declare (ignore points))
  [drop self (clone =sparkle=)]
  [die self])
      
(define-method impel lepton-particle (direction)
  (assert (member direction *compass-directions*))
  (setf <direction> direction)
  ;; don't hit the player
  [find-target self])

(define-prototype lepton-cannon (:parent xe2:=cell=)
  (name :initform "Xiong Les Fleurs Lepton(TM) energy cannon")
  (tile :initform "lepton-cannon")
  (categories :initform '(:item :weapon :equipment))
  (equip-for :initform '(:robotic-arm))
  (weight :initform 14000)
  (accuracy :initform (make-stat :base 60))
  (attack-power :initform (make-stat :base 16))
  (attack-cost :initform (make-stat :base 25))
  (energy-cost :initform (make-stat :base 32)))

(define-method fire lepton-cannon (direction)
  (if [expend-energy <equipper> [stat-value self :energy-cost]]
      (let ((lepton (clone =lepton-particle=)))
	[play-sample <equipper> "bloup"]
	[>>drop <equipper> lepton]
	[>>impel lepton direction]
	[expend-action-points <equipper> [stat-value self :attack-cost]]
      (message "Not enough energy to fire."))))

;;; An exploding missile.

(defvar *missile-trail-tile-map* (list *lepton-trail-end-tiles* *lepton-trail-middle-tiles* *lepton-trail-middle-tiles*))

(defvar *missile-tiles* '(:north "missile-north"
		       :south "missile-south"
		       :east "missile-east"
		       :west "missile-west"
		       :northeast "missile-northeast"
		       :southeast "missile-southeast"
		       :southwest "missile-southwest"
		       :northwest "missile-northwest"))

(define-prototype missile (:parent =lepton-particle=)
  (speed :initform (make-stat :base 25))
  (hit-damage :initform (make-stat :base 10))
  (hit-points :initform (make-stat :base 10))
  (tile :initform "missile-north")
  (clock :initform 20))

(define-method update-tile missile ()
  (setf <tile> (or (getf *missile-tiles* <direction>)
		   "missile-north")))

(define-method die missile ()
  [drop self (clone =explosion=)]
  [parent>>die self])

;;; Multi-warhead missile

(defvar *multi-missile-tiles* '(:north "multi-missile-north"
		       :south "multi-missile-south"
		       :east "multi-missile-east"
		       :west "multi-missile-west"
		       :northeast "multi-missile-northeast"
		       :southeast "multi-missile-southeast"
		       :southwest "multi-missile-southwest"
		       :northwest "multi-missile-northwest"))

(define-prototype multi-missile (:parent =missile=)
  (tile :initform "multi-missile-north")
  (clock :initform 12)
  (hit-damage :initform (make-stat :base 18))
  (hit-points :initform (make-stat :base 20)))

(define-method update-tile multi-missile ()
  (setf <tile> (or (getf *multi-missile-tiles* <direction>)
		   "multi-missile-north")))

(define-method run multi-missile ()
  [update-tile self]
  (if (or (= 0 <clock>)
	  (> 7 [distance-to-player self]))
      ;; release warheads
      (progn 
	(dolist (dir (list :northeast :southeast :northwest :southwest))
	  (multiple-value-bind (r c) 
	      (step-in-direction <row> <column> dir)
	    [drop-cell *world* (clone =missile=) r c]))
	[die self])
      ;; move toward player
      (progn (decf <clock>)
	     [parent>>run self])))

(define-method die multi-missile ()
  [drop self (clone =flash=)]
  [parent>>die self])
  
;;; Missile launchers

(define-prototype missile-launcher (:parent =lepton-cannon=)
  (ammo :initform =missile=)
  (attack-cost :initform (make-stat :base 20)))

(define-method fire missile-launcher (direction)
  (let ((missile (clone <ammo>)))
    [play-sample <equipper> "bloup"]
    [>>drop <equipper> missile]
    [>>impel missile direction]
    [expend-action-points <equipper> [stat-value self :attack-cost]]))

(define-prototype multi-missile-launcher (:parent =missile-launcher=)
  (ammo :initform =multi-missile=)
  (attack-cost :initform (make-stat :base 80)))

