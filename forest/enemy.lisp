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
  (bow-reload-clock :initform 0)
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (speed :initform (make-stat :base 2))
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
	   
;;; The deadly archer skeleton

(defcell archer-skeleton
  (tile :initform "archer-skeleton")
  (name :initform "Archer-Skeleton")
  (categories :initform '(:obstacle :actor :equipper :opaque 
			  :exclusive :enemy :target :archer-skeleton))
  (direction :initform nil)
  (speed :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 6))
  (hit-points :initform (make-stat :base 12 :min 0))
  (equipment-slots :initform '(:left-hand :right-hand))
  (arrows :initform (make-stat :base 15 :min 0))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (dead :initform nil)
  (firing-with :initform :left-hand)
  (energy :initform (make-stat :base 800 :min 0 :max 1000))
  (strength :initform (make-stat :base 24))
  (dexterity :initform (make-stat :base 12))
  (description :initform 
"An undead soul inhabits these blackened bones."))
    
(define-method choose-new-direction archer-skeleton ()
  [expend-action-points self 2]
  (setf <direction>
	(if (= 0 (random 20))
	    ;; occasionally choose a random dir
	    (nth (random 3)
		 '(:north :south :east :west))
	    ;; otherwise turn left
	    (getf '(:north :west :west :south :south :east :east :north)
		  (or <direction> :north)))))
  
(define-method loadout archer-skeleton ()
  [choose-new-direction self]
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =wooden-bow=)]])
  
(define-method cancel archer-skeleton ()
  (decf *enemies*))

;; (define-method initialize archer-skeleton ()
;;   [make-inventory self]
;;   [make-equipment self])

(define-method kick archer-skeleton (direction)
  (setf <direction> direction))

(define-method run archer-skeleton ()
  (clon:with-field-values (row column) self
    (let ((world *world*))
      (if (and (< [distance-to-player world row column] 10)
	       [line-of-sight world row column 
			      [player-row world]
			      [player-column world]])
	  (let ((player-dir [direction-to-player world row column]))
	    [move self player-dir]
	    [fire self player-dir]
	    [expend-action-points self 10])
	  (multiple-value-bind (r c)
	      (step-in-direction <row> <column> <direction>)
	    [expend-action-points self 12]
	    (when [obstacle-at-p world r c]
	      [choose-new-direction self])
	    [move self <direction>])))))
  
(define-method die archer-skeleton ()
  (unless <dead>
    (setf <dead> t)
    (when (plusp [stat-value self :arrows])
      [drop self (clone =arrows= :count [stat-value self :arrows])])
    [play-sample self "death-alien"]
    [delete-from-world self]))

;;; The horrifying Lich

(defvar *lich-alive* nil)

(defcell lichblade 
  (name :initform "lichblade")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "dagger")
  (attack-power :initform (make-stat :base 30))
  (attack-cost :initform (make-stat :base 30))
  (accuracy :initform (make-stat :base 90))
  (stepping :initform t)
  (weight :initform 3000)
  (equip-for :initform '(:left-hand :right-hand)))

(define-prototype lich (:parent xe2:=cell=)
  (name :initform "Lich")
  (strength :initform (make-stat :base 29 :min 0 :max 40))
  (dexterity :initform (make-stat :base 15 :min 0 :max 30))
  (intelligence :initform (make-stat :base 19 :min 0 :max 30))
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
  (equipment-slots :initform '(:left-hand :right-hand))
  (max-items :initform (make-stat :base 3))
  (stepping :initform t)
  (speed :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 8))
  (attacking-with :initform :left-hand)
  (screamed :initform nil)
  (max-weight :initform (make-stat :base 25))
  (hit-points :initform (make-stat :base 80 :min 0 :max 10))
  (tile :initform "lich")
  (description :initform "The Lich is a rotting skeletal horror in red velvet robes."))

(define-method initialize lich ()
  [make-inventory self]
  [make-equipment self])

(define-method loadout lich ()
  (setf *lich-alive* t)
  (let ((blade (clone =lichblade=)))
    [equip self [add-item self blade]]))

(define-method attack lich (target)
  [damage [get-player *world*] 10]
  [expend-action-points self 40]
  [play-sample self "lichdie"]
  [say self "The Lich screams 'DIE!' as it stabs at you."])

(define-method run lich ()
  [expend-action-points self 10]
  (when (and (null <screamed>)
	     (< [distance-to-player self] 15))
    (setf <screamed> t)
    [play-sample self "lichscream"])
  (clon:with-field-values (row column) self
    (let* ((world *world*)
	   (direction [direction-to-player *world* row column]))
      (if [adjacent-to-player world row column]
	  [>>attack self direction]
	  (if [obstacle-in-direction-p world row column direction]
	      (let ((target [target-in-direction-p world row column direction]))
		(if (and target (not [in-category target :enemy]))
		    [>>attack self direction]
		    (progn (setf <direction> (random-direction))
			   [>>move self direction])))
	      (progn (when (< 7 (random 10))
		       (setf <direction> (random-direction)))
		     [>>move self direction]))))))

(define-method die lich ()
  [say self "The lich dies!"]
  (setf *lich-alive* nil)
  [play-sample self "lichdeath"]
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
