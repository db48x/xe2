(in-package :cons-game)

(defcell security-crate 
  (name :initform "Security storage crate")
  (categories :initform '(:obstacle :opaque :pushable :destructible :target))
  (hit-points :initform (make-stat :base 10 :min 0))
  (tile :initform "crate"))
  
(define-method die security-crate ()
  (percent-of-time 50
      [drop self (ecase (random 3)
		   (0 (clone =health=))
		   (1 (clone =health=))
		   (2 (clone =red-key=)))])
  [drop self (clone =crate-debris=)]
  [parent>>die self])

(define-method hit security-crate (&optional hitter)
  [play-sample self "bip"]
  [damage self 1])

;;; Security world generation

(define-prototype security (:parent =sector=)
  (description :initform "Data security.")
  (floor :initform "security-background")
  (barrier :initform "security-foreground")
  (accent :initform "security-accent")
  (height :initform 50)
  (width :initform 50)
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (scale :initform '(1 xm))
  (edge-condition :initform :block)
  (grammar :initform 
  	   '((world >> (:pushloc
  			25 :jump
  			90 :right
  			35 :jump
  			=exit= :color :drop
  			:poploc
  			90 :right
  			5 :jump 
  			90 :left
  			6 :jump
  			:pushloc room-row :poploc
  			90 :right 8 :jump 90 :left
			90 :right 90 :right 8 :jump 90 :left 90 :left
  			:pushloc room-row :poploc
  			90 :right 8 :jump 90 :left
			90 :left 90 :left 8 :jump 90 :right 90 :right
  			:pushloc room-row :poploc
  			90 :right 8 :jump 90 :left))
	     (drop-rook >> (:push-color :pushloc 
			   =rook= :color
			   90 :right 1 :jump
			   1 :draw :poploc :color))
  	     (room-row >> (10 :jump
  			   :pushloc room :poploc 
  			   10 :jump 
  			   :pushloc room :poploc))
  	     (random-crate >> =crate= =crate= =security-crate=)
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
  		       4 :draw drop-rook)))))

(define-method begin-ambient-loop security ()
  (play-music "purity" :loop t))
    
