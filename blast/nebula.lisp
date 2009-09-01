
(in-package :blast)

(defun random-nebula-powerup ()
  (clone (ecase (random 6)
	   (0 =crystal=)
	   (1 =pulse-ammo=)
	   (2 =extender=)
	   (3 =bomb-ammo=)
	   (4 =repair-module=)
	   (5 =mystery-box=))))

;;; The evil boss stations must be destroyed.

(defcell station-arm-horz 
  (tile :initform "station-arm-horz")
  (categories :initform '(:obstacle :opaque :destructible :target))
  (hit-points :initform (make-stat :base 10 :min 0)))

(defcell station-arm-vert 
  (tile :initform "station-arm-vert")
  (categories :initform '(:obstacle :opaque :destructible :target))
  (hit-points :initform (make-stat :base 10 :min 0)))

(defcell station-base 
  (tile :initform "station-base")
  (categories :initform '(:obstacle :actor :equipper :opaque))
  (speed :initform (make-stat :base 4))
  (hit-points :initform (make-stat :base 40 :min 0))
  (equipment-slots :initform '(:robotic-arm))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (attacking-with :initform :robotic-arm)
  (energy :initform (make-stat :base 800 :min 0 :max 1000))
  (firing-with :initform :robotic-arm)
  (strength :initform (make-stat :base 13))
  (dexterity :initform (make-stat :base 9)))

(defvar *station-base-count* 0)

(define-method loadout station-base ()
  (let ((cannon (clone =lepton-cannon=)))
    [equip self [add-item self cannon]])
  (incf *station-base-count*))

(define-method initialize station-base ()
  [make-inventory self]
  [make-equipment self])

(define-method run station-base ()
  (clon:with-field-values (row column) self
    (let ((world *active-world*))
      (if (< [distance-to-player world row column] 10)
	  (let ((player-dir [direction-to-player world row column]))
	    [expend-default-action-points self]
	    [queue>>fire self player-dir])))))

(define-method die station-base ()
  (decf *station-base-count*)
  (when (= 0 *station-base-count*)
    [>>narrateln :narrator "YOU WIN!"])
  [parent>>die self])

(defun paint-station-piece (world row column maxsize)
  (when (not [obstacle-at-p world row column])
    (let ((guardian (clone =guardian=)))
      (labels ((drop-horz (r c)
		 [drop-cell world (clone =station-arm-horz=) r c])
	       (drop-vert (r c)
		 [drop-cell world (clone =station-arm-vert=) r c])
	       (drop-base (r c)
		 [drop-cell world (clone =station-base=) r c :loadout t]))
	(trace-row #'drop-horz row column (max 0 (- column (random maxsize))))
	(trace-row #'drop-horz row column (+ column (random maxsize)))
	(trace-column #'drop-vert column row (max 0 (- row (random maxsize))))
	(trace-column #'drop-vert column row (+ row (random maxsize)))
	[drop-cell world guardian
		   (+ (- row 5) (random 10))
		   (+ (- column 5) (random 10))
		   :loadout t]
	[defend guardian (drop-base row column)]))))

(defcell nebula-space 
  (tile :initform "nebula4")
  (name :initform "Diffuse gas"))

(defcell red-plasma
  (tile :initform "red-plasma")
  (name :initform "Red plasma"))

(defcell charged-nebula-space
  (tile :initform "nebula3")
  (name :initform "Charged plasma"))

(defcell protostar
  (tile :initform "protostar")
  (description :initform "These protostars are forming out of the energy of the nebula."))

(define-method step protostar (stepper)
  (when [is-player stepper]
    [say self "You make a near miss of a protostar, and suffer heat damage."]
    [damage stepper 12])) 

(defcell protogas
  (tile :initform "protogas")
  (description :initform 
"This highly dangerous superheated plasma is in the process of
condensing into protostars."))

(define-method step protogas (stepper)
  (when [is-player stepper]
    [say self "You fly through hot protostar gases, and suffer heat damage."]
    [say self "The hot gases reduce your speed."]
    [expend-action-points stepper 5]
    [damage stepper 2]))

(define-prototype nebula-m (:parent rlx:=world=)
  (name :initform "Restricted Nebula MX type")
  (scale :initform '(50 m))
  (ambient-light :initform :total)
  (required-modes :initform '(:vehicle :spacesuit))
  (description :initform
"This nebula swirls with light and heat as massive clouds of
superheated plasma condense into young stars. Droid activity level
seems high."))
  
(define-method drop-plasma nebula-m (&optional &key (object
					       =red-plasma=)
					       distance 
					       (row 0) (column 0)
					       (graininess 0.3)
					       (density 100)
					       (cutoff 0))
  (clon:with-field-values (height width) self
    (let* ((h0 (or distance height))
	   (w0 (or distance width))
	   (r0 (- row (truncate (/ h0 2))))
	   (c0 (- column (truncate (/ w0 2))))
	   (plasma (rlx:render-plasma h0 w0 :graininess graininess))
	   (value nil))
      (dotimes (i h0)
	(dotimes (j w0)
	  (setf value (aref plasma i j))
	  (when (< cutoff value)
	    (when (or (null distance)
		      (< (distance (+ j r0) (+ c0 i) row column) distance))
	      (percent-of-time density
		[drop-cell self (clone object) (+ r0 i) (+ c0 j) :no-collisions t]))))))))

(define-method generate nebula-m (&key (height 100)
				       (width 100)
				       (protostars 30)
				       (asteroids 230)
				       (mysteries 4)
				       (vaxodrones 16)
				       (polaris 60)
				       (chunks 12)
				       (rooks 5)
				       (canaz 13)
				       (sequence-number (genseq)))
  (setf <name> (format nil "Nebula MX-~A" sequence-number))
  (setf <height> height <width> width)
  [create-default-grid self]
  (dotimes (i width)
    (dotimes (j height)
      [drop-cell self (clone (if (zerop (random 7))
				 =charged-nebula-space= 
				 =nebula-space=))
		 i j]))
  [drop-plasma self]
  (dotimes (i rooks)
    [drop-cell self (clone =rook=) (random height) (random width) :loadout t])
  (dotimes (i chunks)
    [drop-cell self (clone =big-crystal=) (random height) (random width)])
  (dotimes (i protostars)
    (let ((r (random height))
	  (c (random width)))
      [drop-plasma self :object =protogas= :distance 12 :row r :column c :graininess 0.3]
      [drop-plasma self :object =crystal= :density 7 :distance 16 :row r :column c :graininess 0.3]
      [drop-cell self (clone =protostar=) r c]))
  (dotimes (i mysteries)
    [drop-cell self (clone =mystery-box=)
	       (random height) (random width)])
  (dotimes (i canaz)
    [drop-cell self (clone =canaz=) (random height) (random width) :loadout t])
  (dotimes (i vaxodrones)
    [drop-cell self (clone (symbol-value '=vaxodrone=)) (random height) (random width) :loadout t])
  (dotimes (i polaris)
    [drop-cell self (clone =polaris=) (random height) (random width) :loadout t])
  ;;
  [drop-random-asteroids self asteroids]
  [drop-cell self (clone =launchpad=) (random height) (random width)])

(define-method drop-random-asteroids nebula-m (count)
  (clon:with-field-values (height width) self
    (dotimes (i count)
      [drop-cell self (clone =asteroid= :speed (+ 2 (random 3))
			     :direction (rlx:random-direction)
			     :color (nth (random 4)
					 '(:red :blue :brown :orange)))
		 (random height) (random width)])))

(define-method begin-ambient-loop nebula-m ()  
  (play-music "xiomacs2" :loop t))
