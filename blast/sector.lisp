(in-package :blast)

;;; Stars

(define-prototype star (:parent =gateway=)
  (tile :initform "star"))

(defcell void
  (tile :initform "void"))

(define-method step void (stepper)
  (when (has-field :endurium stepper)
    [stat-effect stepper :endurium -1]
    [>>say :narrator (format nil "Burned 1 unit of endurium moving to ~D:~D"
			     (field-value :row stepper)
			     (field-value :column stepper))]
    ;; are you doomed? 
    (when (<= [stat-value stepper :endurium] 0)
      [>>say :narrator "You run out of endurium in the deeps of interstellar space."]
      [>>say :narrator "Your oxygen runs out, suffocating you."]
      [die stepper])))

(define-prototype starfield (:parent =void=)
  (tile :initform "starfield"))

;;; Zeta base

(define-prototype zeta-base-gateway (:parent =gateway=)
  (tile :initform "zeta-base")
  (address :initform '(=zeta-base= 
		       :width 50
		       :height 200
		       :asteroid-count 0
		       :biclops-count 10
		       :berserker-count 10
		       :polaris-count 0
		       :probe-count 50
		       :energy-gas-cluster-count 8
		       :room-size 8
		       :box-cluster-count 40
		       :room-count 65
		       :rook-count 0
		       :scanner-count 25
		       :energy-count 40)))

(define-method step zeta-base-gateway (stepper)
  [>>narrateln :narrator "This is the old Zeta Base. Press RETURN to enter."])

;;; The mysterious Nebula M.

(define-prototype nebula-m-gateway (:parent =gateway=)
  (tile :initform "nebula-m-gateway")
  (address :initform '(=nebula-m=)))

(define-method step nebula-m-gateway (stepper)
  [>>narrateln :narrator "The mysterious Nebula M. Press RETURN to enter."])

;;; A mars-like planet with fractal terrain.

(define-prototype mars-gateway (:parent =gateway=)
  (tile :initform "mars-gateway")
  (address :initform '(=mars= :technetium 8)))

(define-method step mars-gateway (stepper)
  [>>narrateln :narrator "The planet Mars. Press RETURN to enter."])

;;; Infested derelict freighters. 

(defvar *freighter-sequence-number* 0)

(define-prototype freighter-gateway (:parent =gateway=)
  (tile :initform "freighter-gateway")
  (address :initform (list '=freighter= 
			   ;; ensure all freighters are distinct
			   :rooms (+ 5 (random 10))
			   :stations (+ 3 (random 10))
			   :sequence-number 
			   (incf *freighter-sequence-number*))))

(define-method step freighter-gateway (stepper)
  [>>narrateln :narrator "An infested derelict freighter. Press RETURN to enter."])

;;; The local cluster

(define-prototype star-sector (:parent rlx:=world=)
  (ambient-light :initform :total)
  (scale :initform '(1 ly))
  (edge-condition :initform :block))
  
(define-method generate star-sector (&key (height 80)
					  (width 80)
					  (freighters 12)
					  (stars 80))
  (setf <height> height <width> width)
  [create-default-grid self]
  (dotimes (i width)
    (dotimes (j height)
      [drop-cell self (clone (if (zerop (random 7))
				 =starfield= 
				 =void=))
		 i j]))
  (dotimes (i stars)
    [drop-cell self (clone =star=) (random height) (random width)])
  (dotimes (i freighters)
    [drop-cell self (clone =freighter-gateway=) (random height) (random width)])
  [drop-cell self (clone =zeta-base-gateway=) (random 20) (random 20)]
  [drop-cell self (clone =mars-gateway=) (random 20) (random 20)]
  [drop-cell self (clone =nebula-m-gateway=) (random 20) (random 20)])

