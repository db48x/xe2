(in-package :cons-game)

;;; Keys and locks

(defcell blue-key
  (name :initform "Cube box")
  (color :initform :blue)
  (stepping :initform t)
  (tile :initform "blue-key")
  (categories :initform '(:obstacle :pushable :key))
  (description :initform
"Strange boxes appear to have almost no weight, and bullets move them
around."))

(define-method push blue-key (dir)
  (when (or (not [obstacle-in-direction-p *world* <row> <column> dir])
	    [category-in-direction-p *world*
				     <row> <column>
				     dir :key-receptacle])
	    [move self dir :ignore-obstacles]))

(define-prototype turquoise-key (:parent =blue-key=)
  (color :initform :turquoise)
  (tile :initform "turquoise-key"))

(define-prototype red-key (:parent =blue-key=)
  (color :initform :red)
  (tile :initform "red-key"))

(defparameter *receptacle-colors* 
  '(:red :turquoise :blue))

(defun random-receptacle-color ()
  (car (one-of *receptacle-colors*)))

(defparameter *receptacle-tiles*
  '(:red "red-key-receptacle" 
    :turquoise "turquoise-key-receptacle" 
    :blue "blue-key-receptacle" ))

(defcell lock 
  (categories :initform '(:obstacle :opaque :lock))
  color)

(define-method initialize lock (&optional (color (random-receptacle-color)))
  (setf <color> color)
  (setf <tile> (getf *receptacle-tiles* color)))

(define-method step lock (stepper)
  (when (and [in-category stepper :key]
	     (eq <color> (field-value :color stepper)))
    [play-sample self "worp"]
    [say self "The box and lock both disappear."]
    [die self]
    [die stepper]))

;;; The storage crate

(defcell crate
  (tile :initform "crate")
  (name :initform "Storage crate")
  (categories :initform '(:obstacle :opaque :pushable :destructible :target))
  (hit-points :initform (make-stat :base 10 :min 0)))

(defcell crate-debris
  (tile :initform "crate-debris")
  (name :initform "Storage crate debris"))

(define-method die crate ()
  [>>drop self (clone =crate-debris=)]
  [parent>>die self])

(defcell crate-special 
  (name :initform "Interesting storage crate")
  (categories :initform '(:obstacle :opaque :pushable :destructible :target))
  (hit-points :initform (make-stat :base 10 :min 0))
  (tile :initform "crate-special"))
  
(define-method die crate-special ()
  [>>drop self (clone =health=)]
  [parent>>die self])

;;; Storage world generation

(define-prototype storage (:parent =sector=)
  (description :initform "Equipment and supply storage.")
  (floor :initform "storage-background")
  (barrier :initform "storage-foreground")
  (accent :initform "storage-accent")
  (height :initform 45)
  (width :initform 60)
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (scale :initform '(1 xm))
  (edge-condition :initform :block)
  (grammar :initform 
	   '((world >> (=launchpad= :color :drop
			90 :right
			5 :jump 
			=gun= :color :drop
			90 :left
			:pushloc room-row :poploc
			90 :right 12 :jump 90 :left
			:pushloc room-row :poploc
			90 :right 12 :jump 90 :left
			:pushloc room-row :poploc
			90 :right 12 :jump 90 :left
			:drop-shockers :drop-drones))
	     (room-row >> (10 :jump
			   :pushloc room :poploc 
			   10 :jump 
			   :pushloc room :poploc 
			   10 :jump 
			   :pushloc room :poploc 
			   10 :jump 
			   :pushloc room :poploc ))
	     (shocker-maybe >> :noop :noop (=shocker= :color :drop))
             (random-turn >> :right :left)
	     (room >> (=barrier= :color 
		       8 :draw 
		       90 :right 
		       4 :draw
		       2 :jump
		       2 :draw
		       90 :right 
		       8 :draw
		       90 :right
		       4 :draw)
	      (=barrier= :color 
		       5 :draw 
		       90 :right 
		       7 :draw
		       90 :right
	               5 :draw
		       90 :right 
		       3 :draw
	               2 :jump
		       90 :right
		       2 :draw)
	      (=barrier= :color
	       2 :draw
	       2 :jump
	       4 :draw 
	       90 :right
	       8 :draw
	       90 :right
	       4 :draw 
	       :pushloc
	       90 :right
	       6 :draw
	       :poploc
	       3 :draw
	       90 :right
	       8 :draw)))))

(define-method drop-shockers storage ()
  (dotimes (n 5)
    [drop-cell self (clone =shocker=) (random <height>) (random <width>)]))

(define-method drop-drones storage ()
  (dotimes (n 2)
    [drop-sprite self (clone =drone=) (+ 300 (random 200)) (+ 200 (random 200)) :loadout t]))
