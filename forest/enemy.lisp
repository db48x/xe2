(in-package :forest)

;;; Skeletons haunt the woods, coming from gravestones

(defcell gravestone 
  (tile :initform "gravestone")
  (description :initform "The epitaph is no longer legible.")
  (contains-body :initform (percent-of-time 25 t))
  (categories :initform '(:obstacle :actor))
  (generated :initform nil))

(define-method run gravestone ()
  (when (and <contains-body>
	     (< [distance-to-player self] 10)
	     [line-of-sight *world* <row> <column> 
			    [player-row *world*]
			    [player-column *world*]])
    (percent-of-time 40
      (when (not <generated>)
	(setf <generated> t)
	(let ((skeleton (clone =skeleton=)))
	  [drop self skeleton]
	  [loadout skeleton])))))
	  
(defcell dagger 
  (name :initform "dagger")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "dagger")
  (attack-power :initform (make-stat :base 15))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (stepping :initform t)
  (weight :initform 3000)
  (equip-for :initform '(:left-hand :right-hand)))

(defcell skeleton 
  (name :initform "Skeleton")
  (description :initform "A foul spirit animates this dagger-wielding skeleton.")
  (strength :initform (make-stat :base 20 :min 0 :max 50))
  (dexterity :initform (make-stat :base 20 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
  (equipment-slots :initform '(:left-hand))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (speed :initform (make-stat :base 1))
  (movement-cost :initform (make-stat :base 5))
  (attacking-with :initform :left-hand)
  (equipment-slots :initform '(:left-hand :right-hand :belt :extension :feet))
  (max-weight :initform (make-stat :base 25))
  (max-items :initform (make-stat :base 20))
  (hit-points :initform (make-stat :base 5 :min 0 :max 5))
  (tile :initform "skeleton"))

(define-method loadout skeleton ()
  [make-inventory self]
  [make-equipment self]
  (let ((dagger (clone =dagger=)))
    [equip self [add-item self dagger]]))

(define-method run skeleton ()
  (clon:with-field-values (row column) self
    (let* ((world *world*)
	   (direction [direction-to-player *world* row column]))
      (when (< [distance-to-player self] 8)
	(percent-of-time 40 [play-sample self "grak"]))
      (if [adjacent-to-player world row column]
	  (progn [say self "The skeleton stabs at you with its dagger."]
		 [play-sample self "groar"]
		 [expend-action-points self 10]
		 (percent-of-time 80
		   [say self "You are hit!"]
		   [damage [get-player *world*] 7]))
	  (progn	
	    [expend-action-points self 10]
	    (if [obstacle-in-direction-p world row column direction]
		(let ((target [target-in-direction-p world row column direction]))
		  (if (and target (not [in-category target :enemy]))
		      [move self (random-direction)]
		      (progn (setf <direction> (random-direction))
			     [>>move self direction])))
		(progn (when (< 7 (random 10))
			 (setf <direction> (random-direction)))
		       [>>move self direction])))))))

(define-method die skeleton ()
  [play-sample self "dead"]
  [parent>>die self])
	   
;;; Wolves are the most difficult enemies. 

(defcell wolf 
  (categories :initform '(:actor :target :obstacle :opaque :enemy))
  (dexterity :initform (make-stat :base 20))
  (max-items :initform (make-stat :base 1))
  (speed :initform (make-stat :base 2))
  (chase-distance :initform 14)
  (stepping :initform t)
  (behavior :initform :seeking)
  (clock :initform 0)
  (last-direction :initform :north)
  (strength :initform (make-stat :base 50))
  (movement-cost :initform (make-stat :base 10))
  (tile :initform "wolf")
  (target :initform nil)
  (hit-points :initform (make-stat :base 6 :min 0 :max 40))
  (description :initform 
"These undead wolves will devour your flesh if they get the chance."))

(define-method run wolf ()
  (ecase <behavior>
    (:seeking [seek self])
    (:fleeing [flee self])))

(define-method seek wolf ()
  (clon:with-field-values (row column) self
    (when (< [distance-to-player *world* row column] <chase-distance>)
      (let ((direction [direction-to-player *world* row column])
	    (world *world*))
	(percent-of-time 5 [play-sample self (car (one-of '("growl-1" "growl-2")))])
	(if [adjacent-to-player world row column]
	    (progn
	      (percent-of-time 80 
		[say self "The undead wolf bites you."]
		[damage [get-player *world*] 4])
	      (setf <clock> 6
		    <behavior> :fleeing))
	    (if [obstacle-in-direction-p world row column direction]
		(let ((target [target-in-direction-p world row column direction]))
		  (if (and target (not [in-category target :enemy]))
		      (progn nil)
		      (progn (setf <direction> (random-direction))
			     [>>move self direction])))
		(progn (when (< 7 (random 10))
			 (setf <direction> (random-direction)))
		       [>>move self direction])))))))

(define-method damage wolf (points)
  [play-sample self "bark"]
  [parent>>damage self points])

(define-method die wolf ()
  [play-sample self "yelp"]
  [parent>>die self])

(define-method flee wolf ()
  (decf <clock>)
  ;; are we done fleeing? then begin seeking. 
  (if (<= <clock> 0)
      (setf <behavior> :seeking)
      ;; otherwise, flee
      (clon:with-field-values (row column) self
	(let ((player-row [player-row *world*])
	      (player-column [player-column *world*]))
	  (labels ((neighbor (r c direction)
		     (multiple-value-bind (r0 c0)
			 (step-in-direction r c direction)
		       (list r0 c0)))
		   (all-neighbors (r c)
		     (let (ns)
		       (dolist (dir *compass-directions*)
			 (push (neighbor r c dir) ns))
		       ns))
		   (score (r c)
		     (distance player-column player-row c r)))
	    (let* ((neighbors (all-neighbors row column))
		   (scores (mapcar #'(lambda (pair)
				       (apply #'score pair))
				   neighbors))
		   (farthest (apply #'max scores))
		   (square (nth (position farthest scores)
				neighbors)))
	      (destructuring-bind (r c) square
		  [move self (xe2:direction-to row column r c)])))))))

(define-method move wolf (direction)
  (setf <last-direction> direction)
  [parent>>move self direction])
