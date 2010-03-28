(in-package :cons-game)

(defparameter *reactor-tiles* 
  '("reactor1" "reactor2" "reactor3" 
    "reactor4" "reactor5" "reactor6" 
    "reactor7" "reactor8" "reactor9"))

(defparameter *reactor-animation-delay* 5)

(defsprite reactor-core
  (image :initform "reactor1")
  (clock :initform 0)
  (frame :initform 0)
  (categories :initform '(:obstacle :actor))
  (hit-points :initform (make-stat :base 10 :min 0 :max 15))
  (speed :initform (make-stat :base 10 :min 0 :max 15)))

(define-method run reactor-core ()
  (if (zerop <clock>)
      (progn 
	(setf <frame> (mod (+ 1 <frame>) (length *reactor-tiles*)))
	(setf <image> (nth <frame> *reactor-tiles*))
	(setf <clock> *reactor-animation-delay*))
      (progn 
	(decf <clock>))))

;;; Reactor core sector

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

(define-prototype reactor (:parent =sector=)
  (description :initform "Power core station.")
  (floor :initform "reactor-background")
  (barrier :initform "reactor-foreground")
  (accent :initform "reactor-accent")
  (height :initform 120)
  (width :initform 120)
  (level :initform 1)
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (scale :initform '(1 xm))
  (edge-condition :initform :block)
  (grammar :initform 
	   '((world >> (:pushloc
			45 :right 
			5 :jump
			=exit= :color :drop
			:poploc
			90 :right
			20 :jump 
			90 :left
			20 :jump 
			90 :left
			:pushloc security-structure :poploc
			:drop-drones
			90 :right 90 :right 40 :jump
			security-structure))
	     (side-chamber >> (:pushloc
			       room3 90 random-turn
			       room3 90 :left
			       1 :jump
			       gun-maybe
			       :pushloc
			       90 :right
			       3 :jump
			       :drop-reactor
			       :poploc
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
	     (room >> (=barrier= :color 
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
			5 :draw 
			90 :right 
			6 :draw 
			90 :right 
			5 :draw
			90 :right
		        6 :draw)))))

(define-method generate reactor (&rest params)
  [create-default-grid self]
  (dotimes (row <height>)
    (dotimes (column <width>)
      [drop-cell self (clone =orange-road=) row column]))
  [parent>>generate self])

(define-method drop-reactor reactor ()
  (clon:with-field-values (row column tile-size) self
    (let ((x (* column tile-size))
	  (y (* row tile-size)))
      [drop-sprite self (clone =reactor-core=) x y])))

(define-method drop-drones reactor ()
  (dotimes (n 2)
    [drop-sprite self (clone =drone=) (+ 40 (random (* 13 <height>))) (+ 40 (* 13 (random <width>)))]))

(define-method begin-ambient-loop reactor ()
  (play-music "beatup" :loop t))
