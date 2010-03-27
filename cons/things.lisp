(in-package :cons-game)

;;; Bombs!

(defcell bomb
  (tile :initform "bomb")
  (categories :initform '(:item :obstacle :target)))

(defun same-team (obj1 obj2)
  (eq (field-value :team obj1)
      (field-value :team obj2)))

;;; Particle gun

(defcell particle 
  (tile :initform "blueparticle")
  (movement-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 5 :min 0 :max 10))
  (team :initform :player)
  (categories :initform '(:actor))
  (direction :initform :north))

(define-method initialize particle (direction)
  (setf <direction> direction))

(define-method run particle ()
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
      [drop-cell *world* (clone =particle= direction) r c])))

