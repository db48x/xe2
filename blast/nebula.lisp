
(in-package :blast)

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
      (drop-base row column))))

(defcell nebula-space 
  (tile :initform "nebula"))

(defcell red-plasma
  (tile :initform "red-plasma"))

(defcell charged-nebula-space
  (tile :initform "nebula2"))

(defcell protostar
  (tile :initform "protostar"))

(define-method step protostar (stepper)
  (when [is-player stepper]
    [>>say :narrator "You ram into the white-hot protostar and vaporize."]
    [>>die stepper]))

(define-prototype nebula-m (:parent rlx:=world=)
  (name :initform "Restricted Nebula M")
  (ambient-light :initform :total))
  
(define-method drop-plasma nebula-m ()
  (clon:with-field-values (height width) self
    (let ((plasma (rlx:render-plasma height width :graininess 0.7))
	  (value nil))
      (dotimes (i height)
	(dotimes (j width)
	  (setf value (aref plasma i j))
	  (when (< 0 value)
	    (let ((object =red-plasma=))
	      [drop-cell self (clone object) i j :no-collisions t])))))))

(define-method generate nebula-m (&key (height 100)
				       (width 100)
				       (protostars 30)
				       (asteroids 100)
				       (polaris 20)
				       (rooks 10)
				       (canaz 30))
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
  (dotimes (i protostars)
    [drop-cell self (clone =protostar=) (random height) (random width)])
  (dotimes (i canaz)
    [drop-cell self (clone =canaz=) (random height) (random width) :loadout t])
  (dotimes (i polaris)
    [drop-cell self (clone =polaris=) (random height) (random width) :loadout t])
  ;;
  [drop-random-asteroids self asteroids]
  ;;
  (setf *station-base-count* 0)
  (loop do (paint-station-piece self (random height) (random width) 20)
	while (< *station-base-count* 20)))

(define-method drop-random-asteroids nebula-m (count)
  (clon:with-field-values (height width) self
    (dotimes (i count)
      [drop-cell self (clone =asteroid= :speed (+ 3 (random 7))
			     :direction (rlx:random-direction)
			     :color (nth (random 4)
					 '(:red :blue :brown :orange)))
		 (random height) (random width)])))
  
(define-method start nebula-m ()
  (play-music "xiomacs2" :loop t)
  [parent>>start self])
