(in-package :cons-game)

;;; Being alive

(defvar *alive* t)

(defvar *hero* nil)

;;; Scoring points

(defvar *score* 0)

(defun score (points)
  (incf *score* points))

;;; Counting bricks

(defvar *bricks* 0)

;;; Which way the player is going

(defvar *english* :left)

;;; Colors

(defparameter *colors* '(:purple :red :blue :orange :green :yellow :white))

(defparameter *color-schemes* '((:yellow :orange :blue)
				(:yellow :purple :blue)
				(:purple :red :orange)
				(:green :yellow :purple)
				(:red :yellow :orange)))

;;; The underlying floor

(defcell floor 
  (tile :initform "floor"))

;;; The dancefloor tiles

(defparameter *light-tiles* '("floor"
			      "rezlight5"
			      "rezlight4"
			      "rezlight3"
			      "rezlight2"
			      "rezlight1"))

(defparameter *light-clock* 12)

(defcell dancefloor 
  (tile :initform nil)
  (clock :initform *light-clock*)
  (categories :initform '(:actor :dancefloor)))

(define-method update-tile dancefloor ()
  (when [is-located self]
    (unless [category-at-p *world* <row> <column> '(:brick :wall)]
      (setf <tile> (nth (truncate (/ <clock> 2)) *light-tiles*)))))

(define-method light dancefloor (&optional (time *light-clock*))
  (setf <clock> (max 0 time))
  [update-tile self])

(define-method light-toward dancefloor (direction)
  (multiple-value-bind (r c) (step-in-direction <row> <column> direction)
    (unless [category-at-p *world* r c :dancefloor]
      (let ((dancefloor (clone =dancefloor=)))
	[drop-cell *world* dancefloor r c]
	[light dancefloor (max 0 (- <clock> 1))]))))
      
(define-method light-plus dancefloor ()
  (dolist (dir '(:north :south :east :west))
    [light-toward self dir]))
		
(define-method run dancefloor ()
  (setf <clock> (max 0 (- <clock> 1)))
  (if (plusp <clock>)
      (progn
	(when (< 2 <clock>)
	  [light-plus self])
	[update-tile self])
      [die self]))
  
;;; The borders around the gameworld

(defcell wall-horizontal
  (tile :initform "wall")
  (orientation :initform :horizontal)
  (categories :initform '(:obstacle :oriented :wall)))

(defcell wall-vertical
  (tile :initform "wall")
  (orientation :initform :vertical)
  (categories :initform '(:obstacle :oriented :wall)))

(defcell pit
  (tile :initform "floor")
  (orientation :initform :horizontal)
  (categories :initform '(:obstacle :oriented :pit)))

;;; The bouncing ball

(defparameter *ball-bounce-time* 20)

(defvar *balls* 0)

(defsprite ball 
  (image :initform "ball")
  (speed :initform (make-stat :base 20))
  (bounce-clock :initform 0)
  (dead :initform nil)
  (movement-distance :initform (make-stat :base 7 :min 0 :max 14))
  (movement-cost :initform (make-stat :base 10))
  (categories :initform '(:actor :ball :obstacle))
  (direction :initform :north))

(define-method serve ball (direction)
  [play-sample self "serve"]
  (incf *balls*)
  (setf <direction> direction))

(defparameter *vertical-collision*
  '(:northeast :northwest
    :northwest :northeast
    :southeast :southwest
    :southwest :southeast))

(defparameter *horizontal-collision*
  '(:southeast :northeast
    :northeast :southeast
    :southwest :northwest
    :northwest :southwest))

(define-method do-collision ball (object)
  "Choose a new direction based on colliding with the object at DIRECTION."
  [expend-action-points self 1]
  [play-sample self "bip"]
  (block colliding
    (when object
      (unless (eq object self)
	(if [in-category object :ball]
	    (progn 
	      (setf <direction> (car (one-of '(:northeast :northwest :southeast :southwest))))
	      (multiple-value-bind (y x) (xe2:step-in-direction <y> <x> <direction> 
								[stat-value self :movement-distance])
		[update-position self x y]))
	    (progn
	      (when [in-category object :brick]
		[hit object self])
	      (when (eq :sprite (field-value :type object))
		[do-collision object self])
	      (setf <direction>
		    (if [in-category object :pit]
			(progn [die self] (return-from colliding))
			(if [in-category object :paddle]
			    (ecase *english* 
			      (:west :northwest)
			      (:east :northeast))
			    (if (has-field :orientation object)
				(let ((rule (ecase (field-value :orientation object)
					      (:horizontal *horizontal-collision*)
					      (:vertical *vertical-collision*))))
				  (getf rule <direction>))
				(random-direction)))))
	      (when (null <direction>)
		(setf <direction> (car (one-of '(:northeast :northwest :southeast :southwest)))))
	      (multiple-value-bind (y x) (xe2:step-in-direction <y> <x> <direction> 
								[stat-value self :movement-distance])
		[update-position self x y])))))))

(define-method run ball ()
  (if (zerop <bounce-clock>)
      (progn [expend-action-points self 2]
	     (multiple-value-bind (y x) (xe2:step-in-direction <y> <x> <direction> 
							       [stat-value self :movement-distance])
	       [update-position self x y]))
      (progn 
	(setf <bounce-clock> (max 0 (1- <bounce-clock>)))
	(multiple-value-bind (y x) (xe2:step-in-direction <y> <x> <direction> 
							  [stat-value self :movement-distance])
	  [update-position self x y]))))

(define-method die ball ()
  (unless <dead>
    (setf <dead> t)
    (decf *balls*)
    (when (and (zerop [stat-value [get-player *world*] :balls])
	       (zerop *balls*))
      [play-sample self "doom"]
      (setf *alive* nil))
    [remove-sprite *world* self]))

;;; Plasma

(defparameter *plasma-tiles* '("rezblur5"
			       "rezblur4"
			       "rezblur3"
			       "rezblur2"
			       "rezblur1"))

(defparameter *plasma-samples* '("zap1" "zap2" "zap3"))

(defparameter *plasma-alt-samples* '("zap4" "zap5" "zap6" "zap7"))

(defparameter *plasma-sample-schemes* (list *plasma-samples* *plasma-alt-samples*))

(defcell plasma
  (tile :initform "rezblur1")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (clock :initform 5)
  (samples :initform *plasma-samples*)
  (categories :initform '(:actor :paint-source :plasma))
  (description :initform "Spreading toxic paint gas. Avoid at all costs!"))

(define-method set-clock plasma (clock)
  (setf <clock> clock))

(define-method run plasma ()
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      (let ((dir (random-direction)))
	(setf <tile> (nth <clock> *plasma-tiles*))
	[play-sample self (car (one-of <samples>))]
	[move self dir])))

;;; Fuzz

(defparameter *fuzz-tiles* '("psifuzz2"
			     "psifuzz1"))

(defcell fuzz
  (tile :initform "psifuzz1")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (clock :initform 2)
  (categories :initform '(:actor :paint-source :fuzz)))

(define-method set-clock fuzz (clock)
  (setf <clock> clock))

(define-method run fuzz ()
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      (let ((dir (random-direction)))
	(setf <tile> (nth <clock> *fuzz-tiles*))
	[move self dir])))

;;; Psi energy

(defparameter *psi-tiles* '("psiblur5"
			       "psiblur4"
			       "psiblur3"
			       "psiblur2"
			       "psiblur1"))

(defparameter *psi-fuzz-tiles* '("psifuzz1" "psifuzz2"))

(defparameter *psi-samples* '("nextpiano" "nextpiano2"))

(defparameter *psi-alt-samples* '("nextpiano" "nextpiano2"))

(defparameter *psi-sample-schemes* (list *psi-samples* *psi-alt-samples*))

(defcell psi
  (tile :initform "psiblur1")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform (random-direction))
  (clock :initform 10)
  (samples :initform *psi-samples*)
  (categories :initform '(:actor :paint-source :psi)))

(define-method set-clock psi (clock)
  (setf <direction> (random-direction))
  (setf <clock> clock))

(define-method run psi ()
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      (progn (setf <tile> (nth (truncate (/ <clock> 2)) *psi-tiles*))
	     [play-sample self (car (one-of <samples>))]
	     [move self <direction>])))

(define-method die psi ()
  [drop self (clone =fuzz=)]
  [delete-from-world self])

;;; Chi energy

(defparameter *chi-tiles* '("chiblur5"
			       "chiblur4"
			       "chiblur3"
			       "chiblur2"
			       "chiblur1"))

(defparameter *chi-samples* '("bass1" "bass3" "bass3" "bass3" "bass3" "bass3" "bass3" "ohoh"))

(defparameter *chi-alt-samples* '("snare1" "snare2"))

(defvar *beat* 0)

(defparameter *beat-max* 80)

(defparameter *snare-beats* '(19 59))

(defparameter *bd-beats* '(0 19 39 59))

(defparameter *bass-beats* '(0 59))

(defun click-beat ()
  (incf *beat*)
  (when (>= *beat-max* *beat*)
    (setf *beat* 0)))

(defun do-beat (cell)
  (when (member *beat* *snare-beats*)
    [play-sample cell (car (one-of *chi-alt-samples*))])
  ;; (when (member *beat* *bd-beats*)
  ;;   [play-sample cell "bd"])
  (when (member *beat* *bass-beats*)
    [play-sample cell (car (one-of *chi-samples*))]))

(defparameter *chi-sample-schemes* (list *chi-samples* *chi-alt-samples*))

(defcell chi
  (tile :initform "chiblur1")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform (car (one-of '(:north :south :east :west))))
  (clock :initform 10)
  (samples :initform *chi-samples*)
  (categories :initform '(:actor :paint-source :chi)))

(define-method set-clock chi (clock)
  (setf <direction> (random-direction))
  (setf <clock> clock))

(define-method run chi ()
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      (progn (setf <tile> (nth (truncate (/ <clock> 2)) *chi-tiles*))
	     (do-beat self)
	     [move self <direction>])))

(define-method die chi ()
;;  [drop self (clone =fuzz=)]
  [delete-from-world self])

;;; Level themes

(defparameter *psi-theme* (list :stuff =psi=
				:name "PSI"
				:sample-schemes *psi-sample-schemes*
				:music "next"))

(defparameter *chi-theme* (list :stuff =chi=
				:name "CHI"
				:sample-schemes *chi-sample-schemes*
				:music "kodama"))

(defparameter *plasma-theme* (list :stuff =plasma=
				   :name "PLASMA"
				:sample-schemes *plasma-sample-schemes*
				:music "rappy"))

(defparameter *themes* (list *plasma-theme* *psi-theme* *chi-theme*))

(defvar *theme* *plasma-theme*)

;;; Bust these bricks

(defvar *brick-tiles* '(:purple "brick-purple"
			:black "brick-black"
			:red "brick-red"
			:blue "brick-blue"
			:orange "brick-orange"
			:green "brick-green"
			:white "brick-white"
			:yellow "brick-yellow"))

(defcell brick 
  (name :initform "Brick")
  (tile :initform "brick-purple")
  (orientation :initform :horizontal)
  (categories :initform '(:exclusive :actor :obstacle :brick :horizontal :oriented))
  (color :initform :purple))

(define-method run brick ()
  (clon:with-field-values (row column) self
    (multiple-value-bind (r0 c0) (step-in-direction row column :east)
      (multiple-value-bind (r1 c1) (step-in-direction row column :west)
	(unless (and [category-at-p *world* r0 c0 :brick]
		     [category-at-p *world* r1 c1 :brick])
	  (setf <orientation> :vertical))))))

(define-method paint brick (c)
  (setf <color> c)
  (let ((res (getf *brick-tiles* c)))
    (assert (stringp res))
    (setf <tile> res)))

(define-method loadout brick ()
  (incf *bricks*))

(define-method hit brick (&optional ball)
  (let ((floor (clone =dancefloor=)))
    [drop self floor]
    [light floor]
    [die self]))

(define-method splash brick (&optional (add 0))
  (let ((stuff-type (getf *theme* :stuff))
	(schemes (getf *theme* :sample-schemes)))
    (dotimes (n (+ add 5 (random 10)))
      [drop self (let ((stuff (clone stuff-type)))
		   (prog1 stuff
		     (setf (field-value :samples stuff)
			   (car (one-of schemes)))))])))

(define-method die brick ()
  (score 100)
  (decf *bricks*)
  [splash self]
  [parent>>die self])

(define-method cancel brick ()
  (decf *bricks*))

;;; Makes the ball grow

(define-prototype grow-brick (:parent =brick=)
  (name :initform "Grow brick")
  (tile :initform "grow")
  (orientation :initform :horizontal))

(define-method hit grow-brick (&optional ball)
  (when ball
    [update-image ball "big-ball"]
    [die self]))

(define-method die grow-brick ()
  (score 1000)
  [splash self]
  [play-sample self "speedup"]
  [delete-from-world self])

;;; Extra ball

(define-prototype extra-brick (:parent =brick=)
  (name :initform "Extra brick")
  (tile :initform "extra")
  (orientation :initform :horizontal))

(define-method hit extra-brick (&optional ball)
  [die self])

(define-method die extra-brick ()
  (score 1000)
  [splash self]
  [stat-effect [get-player *world*] :balls 1]
  [play-sample self "speedup"]
  [delete-from-world self])

;;; Explode to score points

(define-prototype bomb-brick (:parent =brick=)
  (name :initform "Bomb")
  (tile :initform "bomb")
  (orientation :initform :horizontal))

(define-method explode bomb-brick ()
  [splash self]
    (labels ((do-circle (image)
	     (prog1 t
	       (multiple-value-bind (x y) 
		   [viewport-coordinates self]
		 (let ((x0 (+ x 8))
		       (y0 (+ y 8)))
		 (draw-circle x0 y0 40 :destination image)
		 (draw-circle x0 y0 35 :destination image))))))
    [>>add-overlay :viewport #'do-circle])
  (dolist (dir '(:north :south :east :west :northeast :southeast :northwest :southwest))
    (multiple-value-bind (r c) (step-in-direction <row> <column> dir)
      (let ((brick [category-at-p *world* r c :brick]))
	(when brick [hit brick])))))

(define-method hit bomb-brick (&optional ball)
  (when ball
    [explode self]
    [stat-effect ball :movement-distance 3]
    [die self]))

(define-method die bomb-brick ()
  (score 1000)
  [splash self]
  [play-sample self "explode"]
  [delete-from-world self])

(define-method cancel bomb-brick ()
  (decf *bricks*))

;;; Sparks

(defvar *spark-tiles* '("sparkblur" "sparkblur2" "sparkblur3"))

(defcell spark
  (tile :initform "sparkblur")
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (direction :initform (random-direction))
  (clock :initform 4)
  (categories :initform '(:actor :paint-source :spark)))

(define-method run spark ()
  (decf <clock>)
  (if (> 0 <clock>)
      [die self]
      (progn [play-sample self "spark"]
	     (setf <tile> (car (one-of *spark-tiles*)))
	     [move self (random-direction) :ignore-obstacles])))

(define-method die spark ()
  (let ((brick [category-at-p *world* <row> <column> :brick]))
    (when brick [hit brick]))
  [delete-from-world self])

;;; Unbreakable bricks

(define-prototype hard-brick (:parent =brick=)
  (tile :initform "hard-brick")
  (orientation :initform :horizontal))

(define-method loadout hard-brick ()
  nil)

(define-method hit hard-brick (&optional ball)
  (when ball
    [spark self]))

(define-method spark hard-brick ()
  (dotimes (n 10)
    [play-sample self "spark-pop"]
    [drop self (clone =spark=)]))

(define-method die hard-brick ()
  [delete-from-world self])

(define-method cancel hard-brick ()
  nil)

;;; The hero

(defsprite hero 
  (name :initform "Hero")
  (image :initform "hero")
  (score :initform (make-stat :base 0 :min 0))
  (x :initform 0)
  (y :initform 0)
  (delta-y :initform 0)
  (delta-x :initform 0)
  (last-direction :initform nil)
  (balls :initform (make-stat :base 5 :min 0))
  (dead :initform nil)
  (speed :initform (make-stat :base 10 :min 0 :max 15))
  (hearing-range :initform 100000)
  (movement-cost :initform (make-stat :base 10))
  (movement-distance :initform (make-stat :base 5))
  (jumping :initform nil)
  (jump-time :initform (make-stat :base 15))
  (jump-clock :initform 0)
  (bounce-clock :initform 0)
  (bounce-time :initform 5)
  (direction :initform nil)
  (grabbing :initform nil)
  (walking :initform nil)
  (gravity :initform :south)
  (categories :initform '(:actor :player :massive)))

(define-method quit hero ()
  (xe2:quit :shutdown))

(define-method walk hero (direction)
  (assert (member direction '(:east :west)))
  (setf <walking> direction)
  [move self direction])

(define-method move hero (&optional direction distance)
  [expend-action-points self [stat-value self :movement-cost]]
  (let ((dir (or direction <jumping> <gravity>))
	(dist (or distance [stat-value self :movement-distance])))
    (multiple-value-bind (y x) (step-in-direction <y> <x> dir dist)
      [update-position self x y]
      (setf <last-direction> dir))))

(define-method jump hero (direction)
  (unless <jumping> 
    (setf <walking> nil)
    (setf <jumping> direction)
    (setf <jump-clock> [stat-value self :jump-time])))

(define-method do-collision hero (&optional object)
  (when object
    (message "COLLIDING HERO WITH ~S" (object-name (object-parent object))) 
    [undo-excursion self]))
  	
(define-method run hero ()
  (let ((dir (or <jumping> <gravity>)))
    (when <jumping>
      (decf <jump-clock>)
      (when (zerop <jump-clock>)
	(setf <jumping> (ecase <jumping>
			  (:north :south)
			  (:northeast :southeast)
			  (:northwest :southwest)))))
    (when (< [stat-value self :jump-time] (abs <jump-clock>))
      (setf <jump-clock> 0)
      (setf <jumping> nil))
    [save-excursion self]
    [move self dir]))

;; (define-method run hero ()
;;   (let ((dir (or <jumping> <gravity>)))
;;     (multiple-value-bind (r c) (step-in-direction <y> <x> dir [stat-value self :movement-distance])
;;       (if [would-collide self c r]
;; 	  (progn (message "WOULD COLLIDE T")
;; 		 (when <jumping> 
;; 		   (setf <jumping> nil)
;; 		   (multiple-value-bind (r1 c1) 
;; 		       (step-in-direction <y> <x> (opposite-direction dir) (1+ [stat-value self :movement-distance]))
;; 		     [update-position self c1 r1])))
;; 	  [update-position self c r]))
;;     (when <jumping>
;;       (decf <jump-clock>)
;;       (when (zerop <jump-clock>)
;; 	(setf <jumping> (ecase <jumping>
;; 			  (:northeast :southeast)
;; 			  (:northwest :southwest)))))
;;     (when (< [stat-value self :jump-time] (abs <jump-clock>))
;;       (setf <jump-clock> 0)
;;       (setf <jumping> ninnl))))

;;; Platforms to jump on 

(defparameter *platform-delay-time* 10)

(defsprite platform 
  (open :initform nil)
  (image :initform "plat1-closed")
  (clock :initform 0)
  (orientation :initform :horizontal)
  (categories :initform '(:obstacle :oriented)))

(define-method do-collision platform (&optional ball)
  (message "PLATFORM COLLIDED")
  (unless (or (null ball) (eq self ball))
    (if (zerop <clock>)
      (progn 
	(setf <clock> *platform-delay-time*)
	(when [in-category ball :ball]
	  (setf <open> (if <open> nil t))
	  [update-image self (if <open> "plat1-open" "plat1-closed")]))
	(progn
	  (setf <clock> (max 0 (- <clock> 1)))))))
  
(define-method run platform ()
  (setf <clock> (max 0 (- <clock> 1))))

;;; The paddle

(defparameter *serve-key-delay* 18)

(defparameter *paddle-size* 5)

(defcell paddle 
  (tile :initform "player")
  (direction :initform nil)
  (next-piece :initform nil)
  (previous-piece :initform nil)
  (hearing-range :initform 1000)
  (orientation :initform :horizontal)
  (initialized :initform nil)
  (name :initform "paddle")
  (serve-key-clock :initform 0)
  (balls :initform (make-stat :base 5))
  (score :initform (make-stat :base 0 :min 0))
  (speed :initform (make-stat :base 10 :min 0 :max 20))
  (movement-cost :initform (make-stat :base 10))
  (categories :initform '(:actor :proxy :player :obstacle :paddle :oriented)))

(define-method obstructed paddle (direction)
  (or (when <next-piece> [obstructed <next-piece> direction])
      [category-in-direction-p *world* <row> <column> direction :obstacle]))

(define-method restart paddle ()
  (let ((player (clone =paddle=))
	(hero (clone =hero=)))
    (setf *alive* t)
    (setf *balls* 0)
    (setf *bricks* 0)
    [exit *universe*] ;; don't crash
    [destroy *universe*]
    (setf *theme* (car (one-of *themes*)))
    [set-player *universe* player]
    [set-character *status* player]
    [play *universe*
	  :address '(=room=)]
    [proxy player hero]
    [loadout player]
    [loadout hero]
    (xe2:reset-joystick)))

(define-method quit paddle ()
  (xe2:quit :shutdown))

(defparameter *joystick-dead-zone* 2000)

(define-method run paddle ()
  (setf <tile> (if *alive* "player" "floor"))
  (when (null <previous-piece>)
    (setf <initialized> t)
    [update *status*]
    (when (plusp <serve-key-clock>)
      (decf <serve-key-clock>))
    (let ((value (xe2:poll-joystick-axis 0)))
      (if (< (abs value) *joystick-dead-zone*)
	  (setf <direction> nil)
	  (if (plusp value)
	      (setf <direction> :east)
	      (setf <direction> :west))))
    (when (and <next-piece> <direction>)
      [move self <direction>])))

(define-method attach paddle (piece)
  (setf <next-piece> piece)
  (setf (clon:field-value :previous-piece piece) self))

(define-method initialize paddle ()
  (message "created paddle"))

(define-method serve-ball paddle (direction)
  (if (zerop <serve-key-clock>)
      (if (plusp [stat-value self :balls])
	  (let ((ball (clone =ball=)))
	    [stat-effect self :balls -1]
	    (setf <serve-key-clock> *serve-key-delay*)
	    (multiple-value-bind (x y) [viewport-coordinates self]
	      [drop-sprite self ball (+ x 30) (- y 20)]
	      [serve ball direction]))
	  [play-sample self "error"])
      (setf <serve-key-clock> (max 0 (- <serve-key-clock> 1)))))

(define-method walk paddle (direction)
  ;; for hero keybinding compatibility
  [move self direction])
 
(define-method move paddle (direction &optional slave)
  (if slave
      [parent>>move self direction :ignore-obstacles]
      (clon:with-field-values (width player) *world*
	;; don't allow paddle off screen
	(if (ecase direction
	      (:west (< 1 <column>))
	      (:east (< <column> (- width *paddle-size* 2))))
	    (progn (setf *english* direction)
		   [parent>>move self direction :ignore-obstacles]
		   (let ((piece <next-piece>))
		     (loop do (when piece [>>move piece direction t])
			      (setf piece (clon:field-value :next-piece piece))
			   while piece)))
	    [play-sample self "bip"]))))

(define-method loadout paddle ()
  (let ((last-piece self))
    (dotimes (n *paddle-size*)
      (let ((piece (clone =paddle=)))
	[drop-cell *world* piece <row> (+ n 1 <column>)]
	[attach last-piece piece]
	(setf last-piece piece)))))
	
(define-method disembark paddle ()
  [unproxy self :dy -50 :dx 20])

;;; The xiobreak room

(defcell drop-point 
  (categories :initform '(:player-entry-point))
  (tile :initform "floor"))

(defparameter *room-height* (truncate (/ (- *xiobreak-window-height* 20) *tile-size*)))
(defparameter *room-width* (truncate (/ *xiobreak-window-width* *tile-size*)))

(define-prototype room (:parent xe2:=world=)
  (height :initform *room-height*)
  (width :initform *room-width*)
  (edge-condition :initform :block))

(define-method drop-border room ()
  (clon:with-field-values (height width) self
    (labels ((drop-horz-wall (r c)
	       (prog1 nil [drop-cell self (clone =wall-horizontal=) r c]))
	     (drop-vert-wall (r c)
	       (prog1 nil [drop-cell self (clone =wall-vertical=) r c]))
	     (drop-pit (r c)
	       (prog1 nil [drop-cell self (clone =pit=) r c])))
      (drop-horz-wall 0 0)
      (trace-row #'drop-horz-wall 0 0 width)
      (trace-row #'drop-pit (- height 1) 0 width)
      (trace-column #'drop-vert-wall 0 0 (- height 1))
      (trace-column #'drop-vert-wall (- width 1) 0 (- height 1)))))
	          
(define-method drop-floor room ()
  (clon:with-field-values (height width) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))))

(define-method drop-brick-row room (row x0 x1 color)
  (labels ((drop-brick (r c)
	     (let ((brick (clone =brick=)))
	       [paint brick color]
	       [drop-cell self brick r c :loadout t])))
    (xe2:trace-row #'drop-brick row x0 x1)))

(defparameter *classic-layout-horz-margin* 0)

(defparameter *classic-layout-top-margin* 4)

(defparameter *classic-layout-layers* 2)

(define-method drop-classic-layout room (&optional (row-delta 0))
  (let ((left *classic-layout-horz-margin*)
	(right (- <width> 2 *classic-layout-horz-margin*))
	(row (+ 1 row-delta *classic-layout-top-margin*))
	(scheme (car (one-of *color-schemes*))))
    (dotimes (n *classic-layout-layers*)
      (dolist (color scheme)
	[drop-brick-row self row left right color]
	(incf row)))))

(define-method drop-unbreakable-mass room (row column height width)
  (labels ((drop-wall (r c)
	     (prog1 nil
	       [delete-category-at self r c :brick]
	       [drop-cell self (clone =hard-brick=) r c])))
    (trace-rectangle #'drop-wall row column height width t))) 

(define-method drop-masses room ()
  (dotimes (i 3)
    [drop-unbreakable-mass self (+ 5 (random 4)) (+ 3 (random (- <width> 15)))
			   (+ 3 (random 3)) (+ 5 (random 7))]))

(define-method generate room (&key (height *room-height*)
				   (width *room-width*)
				   (grow-bricks 2)
				   (bomb-bricks 22)
				   (extra-bricks 2)
				   (platforms 3))
  (setf <height> height)
  (setf <width> width)
  [create-default-grid self]
  [drop-floor self]
  [drop-border self]
  [drop-classic-layout self]
  (dotimes (n grow-bricks)
    (let ((row (1+ (random 5)))
	  (column (1+ (random (- width 1)))))
      [delete-category-at self row column :brick]
      [drop-cell self (clone =grow-brick=) row column :loadout t]))
  ;; (dotimes (n platforms)
  ;;   (let ((platform (clone =platform=)))
  ;;     [add-sprite self platform]
  ;;     [update-position platform (+ 100 (random 400)) (+ 400 (random 120))]))
  (dotimes (n extra-bricks)
    (let ((row (1+ (random 5)))
	  (column (1+ (random (- width 1)))))
      [delete-category-at self row column :brick]
      [drop-cell self (clone =extra-brick=) row column :loadout t]))
  (dotimes (n bomb-bricks)
    (let ((row (+ 6 (random 5)))
	  (column (1+ (random (- width 3)))))
      [delete-category-at self row column :brick]
      [drop-cell self (clone =bomb-brick=) row column :loadout t]))
  [drop-masses self]
  [drop-cell self (clone =drop-point=) 32 5])

(define-method begin-ambient-loop room ()
  (play-music (getf *theme* :music) :loop t))

;;; Controlling the game

(define-prototype room-prompt (:parent xe2:=prompt=))

(defparameter *numpad-keybindings* 
  '(("KP4" nil "walk :west .")
    ("KP6" nil "walk :east .")
    ;;
    ("KP7" (:control) "serve-ball :northwest .")
    ("KP9" (:control) "serve-ball :northeast .")
    ("JOYSTICK" (:right :button-down) "joywaslk :east .")
    ("JOYSTICK" (:left :button-down) "joywaslk :west .") 
    ("JOYSTICK" (:right :button-up) "joystop .")
    ("JOYSTICK" (:left :button-up) "joystop .")
    ("JOYSTICK" (:square :button-down) "serve-ball :northwest .")
    ("JOYSTICK" (:cross :button-down) "serve-ball :northeast .")
    ("LEFT" nil "walk :west .")
    ("RIGHT" nil "walk :east .")
    ;;
    ("C" nil "jump :northwest .")
    ("F" nil "jump :north .")
    ("V" nil "jump :northeast .")
    ;;
    ("N" nil "embark .")
    ("M" nil "disembark .")
    ("Z" nil "serve-ball :northwest .")
    ("X" nil "serve-ball :northeast .")))

(defparameter *qwerty-keybindings*
  (append *numpad-keybindings*
	  '(("H" nil "walk :west .")
	    ("L" nil "walk :east .")
	    ;;
	    ("Y" (:control) "serve-ball :northwest .")
	    ("U" (:control) "serve-ball :northeast .")
	    ;;
	    ("ESCAPE" nil "restart .")
	    ("Q" (:control) "quit ."))))

(define-method install-keybindings room-prompt ()
  (dolist (k (append *numpad-keybindings* *qwerty-keybindings*))
    (apply #'bind-key-to-prompt-insertion self k))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   (click-beat)
				   (message "FPS: ~S" (truncate (sdl:average-fps)))
				   [run-cpu-phase *world* :timer])])

;;; A status widget for score display

(defvar *status*)

(define-prototype status (:parent xe2:=formatter=)
  (character :documentation "The character cell."))

(define-method set-character status (character)
  (setf <character> character))

(define-method print-stat status (stat-name &key warn-below show-max)
  (let* ((stat (field-value stat-name <character>))
	 (value [stat-value <character> stat-name]))
    (destructuring-bind (&key min max base delta unit) stat
      (let ((color (if (and (numberp warn-below)
			    (< value warn-below))
		       ".red"
		       ".gray40")))
	[print self (symbol-name stat-name)
	       :foreground ".white"]
	[print self ":["]
	[print self (format nil "~S" value) 
	       :foreground ".yellow"
	       :background color]
	(when show-max
	  [print self (format nil "/~S" max)
		 :foreground ".yellow"
		 :background color])
	(when unit 
	  [print self " "]
	  [print self (symbol-name unit)])
	[print self "]"]
	))))

(defparameter *status-bar-character* " ")

(define-method print-stat-bar status (stat &key 
					   (color ".yellow")
					   (background-color ".gray40"))
  (let ((value (truncate [stat-value <character> stat]))
	(max (truncate [stat-value <character> stat :max])))
    (dotimes (i max)
      [print self *status-bar-character*
	     :foreground ".yellow"
	     :background (if (< i value)
			     color
			   background-color)])))

(define-method update status ()
  [delete-all-lines self]
  (let* ((char <character>))
    (when char
      ;; :font "fat-bits"
      [print self (format nil "THEME: ~6A   " (getf *theme* :name))]
	[print self (format nil "SCORE: ~S   " *score*)  :foreground ".white" :background ".black"]
	[print self (format nil "BALLS: ~S   " [stat-value char :balls])]
	[print self (format nil "BRICKS: ~S   " *bricks*)]
	[print self " ARROWS: MOVE / Z+X: FIRE / CONTROL-Q: QUIT / ESC: RESET"]
	[newline self])))

;;; Main program. 

(defun init-xiobreak ()
  (xe2:message "Initializing Xiobreak...")
  (clon:initialize)
  (xe2:set-screen-height *xiobreak-window-height*)
  (xe2:set-screen-width *xiobreak-window-width*)
  (let* ((prompt (clone =room-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (status (clone =status=))
	 (hero (clone =hero=))
	 (paddle (clone =paddle=))
	 (viewport (clone =viewport=)))
    (setf *hero* hero)
    (setf *alive* t)
    (setf *balls* 0)
    (setf *bricks* 0)
    (setf *score* 0)
    (setf *theme* (car (one-of (list *psi-theme* *chi-theme* *plasma-theme*))))
    ;;
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    ;;
    (setf *status* status)
    [resize status :height 20 :width *xiobreak-window-width*]
    [move status :x 0 :y (- *xiobreak-window-height* 20)]
    [set-character status paddle]
    ;;
    [resize narrator :height 80 :width *xiobreak-window-width*]
    [move narrator :x 0 :y (- *xiobreak-window-height* 80)]
    [set-verbosity narrator 0]
    ;;
    [play universe
	  :address '(=room=)
	  :player paddle
	  :narrator narrator
	  :prompt prompt
	  :viewport viewport]
    [loadout paddle]
    [loadout hero]
    [proxy paddle hero]
    [set-tile-size viewport *tile-size*]
    [resize viewport :height 470 :width *xiobreak-window-width*]
    [move viewport :x 0 :y 0]
    [set-origin viewport :x 0 :y 0 
		:height (truncate (/ *xiobreak-window-height* *tile-size*))
		:width (truncate (/ *xiobreak-window-width* *tile-size*))] 
    [adjust viewport] 
    (xe2:install-widgets prompt viewport status)))

(init-xiobreak)
