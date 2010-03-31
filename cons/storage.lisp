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
  (hit-points :initform (make-stat :base 4 :min 0)))

(defcell crate-debris
  (tile :initform "crate-debris")
  (name :initform "Storage crate debris"))

(define-method die crate ()
  [drop self (clone =crate-debris=)]
  [parent>>die self])

(define-method hit crate (&optional hitter)
  [play-sample self "bip"]
  [damage self 1])

(defcell crate-special 
  (name :initform "Interesting storage crate")
  (categories :initform '(:obstacle :opaque :pushable :destructible :target))
  (hit-points :initform (make-stat :base 10 :min 0))
  (tile :initform "crate-special"))
  
(define-method die crate-special ()
  [drop self (ecase (random 4)
	       (0 (clone =health=))
	       (1 (clone =bomb-defun=))
	       (2 (clone =shocker=))
	       (3 (clone =blue-key=)))]
  [parent>>die self])

(define-method hit crate-special (&optional hitter)
  [play-sample self "bip"]
  [damage self 1])

;;; Storage world generation

(define-prototype storage (:parent =sector=)
  (description :initform "Equipment and supply storage.")
  (name :initform "Storage cube")
  (description :initform 
"You never know what you'll find in a storage chamber. All manner of
supplies, ammunition, and sometimes special items may be found here.")
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
	   '((world >> (:pushloc
			5 :jump
			90 :right
			4 :jump
			=exit= :color :drop
			:poploc
			90 :right
			5 :jump 
			90 :left
			1 :jump
			=bomb-defun= :color :drop
			:pushloc room-row :poploc
			90 :right 12 :jump 90 :left
			:pushloc room-row :poploc
			90 :right 12 :jump 90 :left
			:pushloc room-row :poploc
			90 :right 12 :jump 90 :left
			:drop-shockers :drop-biclops))
	     (room-row >> (10 :jump
			   :pushloc room :poploc 
			   10 :jump 
			   :pushloc room :poploc 
			   10 :jump 
			   :pushloc room :poploc 
			   10 :jump 
			   :pushloc room :poploc ))
	     (random-crate >> =crate= =crate= =crate= =crate= =crate-special=)
	     (crate-to-right >> (:push-color :pushloc
				random-crate :color
				90 :right
				1 :jump 
				1 :draw
				:poploc :color))
	     (shocker-maybe >> :noop :noop (=shocker= :color :drop))
             (random-turn >> :right :left)
	     (room >> (=barrier= :color 
		       8 :draw 
		       90 :right 
		       4 :draw
		       2 :jump
		       2 :draw
		       90 :right 
		       2 :draw
		       crate-to-right
		       1 :draw
		       crate-to-right
		       1 :draw
		       crate-to-right
		       4 :draw
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
		       2 :draw
		       crate-to-right
		       1 :draw
		       crate-to-right
		       1 :draw
		       crate-to-right
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
	       4 :draw
	       crate-to-right
	       1 :draw
	       crate-to-right
	       1 :draw
	       crate-to-right
	       :poploc
	       3 :draw
	       90 :right
	       8 :draw)))))

(define-method drop-shockers storage ()
  (dotimes (n 17)
    [drop-cell self (clone =shocker=) (random <height>) (random <width>)])
  (dotimes (n 4)
    [drop-cell self (clone =scanner=) (random <height>) (random <width>)]))

(define-method drop-scanners storage ()
  (dotimes (n 4)
    [drop-cell self (clone =scanner=) (random <height>) (random <width>)]))

(define-method begin-ambient-loop storage ()
  (play-music "beatup" :loop t))
    
