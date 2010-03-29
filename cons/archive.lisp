(in-package :cons-game)

(defcell archive-crate 
  (name :initform "Data storage crate")
  (categories :initform '(:obstacle :opaque :pushable :destructible :target))
  (hit-points :initform (make-stat :base 10 :min 0))
  (tile :initform "archive-crate"))
  
(define-method die archive-crate () 
  [drop self (clone =crate-debris=)]
  (percent-of-time 50
      [drop self (ecase (random 3)
		   (0 (clone =health=))
		   (1 (clone =health=))
		   (2 (clone =red-key=)))])
  [parent>>die self])

(define-method hit archive-crate (&optional hitter)
  [play-sample self "bip"]
  [damage self 1])

;;; Archive world generation

(define-prototype archive (:parent =sector=)
  (description :initform "Data archive.")
  (floor :initform "archive-background")
  (barrier :initform "archive-foreground")
  (accent :initform "archive-accent")
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
	     (drop-eye >> (:push-color :pushloc 
			   =guardic-eye= :color
			   90 :right 1 :jump
			   1 :draw :poploc :color))
  	     (room-row >> (10 :jump
  			   :pushloc room :poploc 
  			   10 :jump 
  			   :pushloc room :poploc))
  	     (random-crate >> =crate= =crate= =archive-crate=)
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
  		       4 :draw drop-eye)))))

(define-method begin-ambient-loop archive ()
  (play-music "xiomacs" :loop t))
    
