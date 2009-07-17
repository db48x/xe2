(in-package :blast)

;;; Gateways 

(defcell gateway
  (tile :initform "gateway")
  (categories :initform '(:gateway))
  (address :initform nil))

(define-method activate gateway ()
  [play *active-universe* :address <address>])

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
    (when (zerop [stat-value stepper :endurium])
      [>>say :narrator "You run out of endurium in the deeps of interstellar space."]
      [>>say :narrator "Your oxygen runs out, suffocating you."]
      [die stepper])))

(define-prototype starfield (:parent =void=)
  (tile :initform "starfield"))

;; TODO star systems with planets

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

;;; The local cluster

(define-prototype star-sector (:parent rlx:=world=)
  (ambient-light :initform :total))
  
(define-method generate star-sector (&key (height 80)
					  (width 80)
					  (star-count 80))
  (setf <height> height <width> width)
  [create-default-grid self]
  (dotimes (i width)
    (dotimes (j height)
      [drop-cell self (clone (if (zerop (random 7))
				 =starfield= 
				 =void=))
		 i j]))
  (dotimes (i star-count)
    [drop-cell self (clone =star=) (random height) (random width)])
  [drop-cell self (clone =zeta-base-gateway=) (random 20) (random 20)]
  [drop-cell self (clone =nebula-m-gateway=) (random 20) (random 20)])

