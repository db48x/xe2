(in-package :blast)

;;; Glittering flash gives clues on locations of explosions/damage

(defcell flash 
  (clock :initform 2)
  (tile :initform "flash-1")
  (categories :initform '(:actor))
  (speed :initform (make-stat :base 10)))

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
  (speed :initform (make-stat :base 10)))

(define-method run sparkle ()
  [expend-action-points self 10]
  (case <clock>
    (1 (setf <tile> "sparkle"))
    (0 [>>die self]))
  (decf <clock>))

;;; An explosion.

(defcell explosion 
  (name :initform "Explosion")
  (categories :initform '(:actor))
  (tile :initform "explosion")
  (speed :initform (make-stat :base 10))
  (damage-per-turn :initform 5)
  (clock :initform 2))

(define-method run explosion ()
  (if (zerop <clock>)
      [die self]
      (progn
	[play-sample self "crunch"]
	(decf <clock>)
	[expend-action-points self 10]
	(rlx:do-cells (cell [cells-at *active-world* <row> <column>])
	  [damage cell <damage-per-turn>]))))

