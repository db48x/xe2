(in-package :cons-game)

;;; Inert blocks

(defcell block 
  (tile :initform "block")
  (team :initform :neutral)
  (categories :initform '(:item :obstacle :target)))

;;; Reactor world

(defcell orange-barrier
  (description :initform "Impenetrable barrier.")
  (tile :initform "orangeworld")
  (categories :initform '(:obstacle)))

(defcell blue-brick
  (description :initform "Breakable brick.")
  (hit-points :initform (make-stat :base 20 :min 0))
  (tile :initform "darkorangeworld2")
  (categories :initform '(:obstacle)))

(define-method hit blue-brick ()
  [play-sample self "break"]
  [damage self 1])

(define-method die blue-brick ()
  [play-sample self "break2"]
  [parent>>die self])

(defcell purple-brick
  (description :initform "Impenetrable barrier.")
  (hit-points :initform (make-stat :base 10 :min 0))
  ;; (tile :initform "darkorangeworld3")
  (tile :initform "darkorangeworld3")
  (categories :initform '(:obstacle)))

(define-method hit purple-brick ()
  [play-sample self "break"]
  [damage self 1])

(define-method die purple-brick ()
  [play-sample self "break2"]
  [parent>>die self])

(defcell orange-barrier4
  (description :initform "Impenetrable barrier.")
  (tile :initform "darkorangeworld4")
  (categories :initform '(:obstacle)))

(defcell orange-road
  (description :initform "Core maintenance vehicle transit area.")
  (tile :initform "darkorangeworld"))

(define-prototype reactor (:parent xe2:=world=)
  (description :initform "Power core station.")
  (height :initform 120)
  (width :initform 120)
  (level :initform 1)
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (scale :initform '(1 xm))
  (edge-condition :initform :block)
  (grammar :initform 
	   '((world >> (=launchpad= :color :drop
			90 :right
			20 :jump 
			=gun= :color :drop
			90 :left
		        30 :jump
			90 :left
			:pushloc security-structure :poploc
			90 :right 90 :right 40 :jump
			security-structure))
	     (side-chamber >> (:pushloc
			       room3 90 random-turn
			       room3 90 :left
			       1 :jump
			       gun-maybe
			       :poploc))
	     (gun-maybe >> :noop :noop (=shocker= :color :drop))
	     (security-structure >> (room 90 :left 
				     room 90 :left 
				     room 90 :left
				     room 90 :right
				     6 :jump
				     :pushloc room2 90 random-turn room2 :poploc
				     side-chamber))
             (random-turn >> :right :left)
	     (random-brick >> =purple-brick= =blue-brick=)
	     (room >> (=orange-barrier= :color 
		       10 :draw 
		       90 :right 
		       4 :draw
		       2 :jump
		       4 :draw
		       90 :right 
		       10 :draw))
	     (room2 >> (random-brick :color 
			5 :draw 
			90 :right 
			5 :draw 
			90 :right 
			2 :draw
			1 :jump
			2 :draw
			90 :right
		        10 :draw))
	     (room3 >> (=blue-brick= :color 
			3 :draw 
			90 :right 
			4 :draw 
			90 :right 
			4 :draw 
			90 :right
		        4 :draw)))))

(define-method generate reactor (&rest params)
  [create-default-grid self]
  (dotimes (row <height>)
    (dotimes (column <width>)
      [drop-cell self (clone =orange-road=) row column]))
  [parent>>generate self])

(define-method begin-ambient-loop reactor ()
  (play-music "beatup" :loop t))

;;; Basic level

(defcell road
  (description :initform "Security vehicle transit area.")
  (tile :initform "darkcyanworld"))

(defcell barrier
  (description :initform "Impenetrable barrier.")
  (tile :initform "cyanworld")
  (categories :initform '(:obstacle)))

(define-prototype highway (:parent xe2:=world=)
  gen-row gen-column 
  ;;
  (description :initform "You enter a long corridor.")
  (level :initform 1)
  ;;
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (scale :initform '(3 m))
  (edge-condition :initform :block))

(define-method generate highway (&key (height 80)
					    (width 50))
  (setf *notes* nil)
  (setf <height> height <width> width)
  [create-default-grid self]
  (labels ((drop-barrier (r c)
	     (prog1 nil
	       [drop-cell self (clone =barrier=) r c])))
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =road=)
		 i j]))
    (dotimes (i 20)
      [drop-cell self (clone =block=) (random height) (random width)])
    (dotimes (i 20)
      [drop-cell self (clone =gun=) (random height) (random width)])
    (dotimes (i 20)
      [drop-cell self (clone =bomb=) (random height) (random width)])
    (dotimes (i 25)
      (let ((draw-function (if (= 0 (random 3))
			       #'trace-row #'trace-column)))
	(funcall draw-function #'drop-barrier
		 (+ 10 (random 50))
		 (+ 10 (random 50))
		 (+ 10 (random 50)))))
    [drop-cell self (clone =launchpad=) 10 10]))

(define-method begin-ambient-loop highway ()
  (play-music "beatup" :loop t))

