(in-package :cons-game)

;;; List body segments

(defcell segment 
  (tile :initform "segment")
  (item-tile :initform nil :documentation "When non-nil, superimpose this tile.")
  (description :initform "List snake body segment.")
  (direction :initform nil :documentation "When non-nil, move once in this direction.")
  (last-direction :initform :north)
  (movement-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (categories :initform '(:actor :opaque :target :segment :drawn))
  (team :initform :player))

(define-method run segment ()
  (when <direction>
    [move self <direction>]
    (setf <direction> nil)))

(define-method hit segment (&optional other)
  [hit [get-player *world*]])

(define-method move segment (direction)
  (setf <last-direction> direction)
  [parent>>move self direction :ignore-obstacles])

(define-method queue-move segment (direction)
  (setf <direction> direction))

(define-method show-item segment (item-tile)
  (setf <item-tile> item-tile))

(define-method draw segment (x y image)
  (draw-resource-image <tile> x y :destination image)
  (when <item-tile>
    (draw-resource-image <item-tile> x y :destination image)))

;;; Agent: the player

(defcell agent 
  (tile :initform "agent-north")
  (description :initform "You are a sentient warrior cons cell.")
  (tail-length :initform 3)
  (segments :initform nil)
  (items :initform nil)
  (direction :initform :north)
  (last-direction :initform :north :documentation "Last direction actually moved.")
  (dead :initform nil)
  (last-turn-moved :initform 0)
  (team :initform :player)
  (hit-points :initform (make-stat :base 20 :min 0 :max 20))
  (movement-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 10 :min 0 :max 25))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hearing-range :initform 25)
  (movement-cost :initform (make-stat :base 10))
  (stepping :initform t)
  (light-radius :initform 7)
  (categories :initform '(:actor :obstacle :player :target :container :light-source)))

(define-method loadout agent ()
  (push (clone =buster-defun=) <items>))

(define-method start agent ()
  (clon:with-fields (segments) self
    (setf segments nil)
    (setf <direction> :north)
    (setf <last-direction> :north)
    (if (field-value :overworld *world*)
	(setf <tile> "player32")
	(clon:with-field-values (row column) self
	  (setf <tile> "agent-north")
	    (dotimes (n <tail-length>)
	      [add-segment self (- (+ row <tail-length>) n) column])))))

(define-method hit agent (&optional object)
  [play-sample self "ouch"]
  [damage self 1])

(define-method damage agent (points)
  (message "DAMAGE OF ~S" points)
  [parent>>damage self points])
  
(define-method pause agent ()
  [pause *world*])

(defparameter *agent-tiles* '(:north "agent-north"
			     :south "agent-south"
			     :east "agent-east"
			     :west "agent-west"))

(define-method aim agent (direction)
  (setf <direction> direction)
  (setf <tile> (getf *agent-tiles* direction)))

(define-method move agent (&optional direction)
  (unless <dead>
    (let ((phase (field-value :phase-number *world*))
	  (dir (or direction <direction>)))
      (unless (= <last-turn-moved> phase)
	(setf <last-turn-moved> phase)
	[aim self dir]
	(when [parent>>move self dir]
	  [move-segments self]
	  (setf <last-direction> dir))))))

(define-method move-segments agent ()
  (clon:with-field-values (items last-direction segments) self
    (let ((next-dir last-direction))
      (dolist (segment segments)
	[queue-move segment next-dir]
	(setf next-dir (field-value :last-direction segment))))))

(define-method in-overworld agent ()
  (field-value :overworld *world*))

(define-method update-tiles agent ()
  (if [in-overworld self]
      (setf <tile> "player32")
      (clon:with-field-values (items segments) self
	(let ((n 0)
	      (num-items (length items)))
	  (dolist (segment segments)
	    [show-item segment (when (< n num-items)
				 (field-value :tile (nth n items)))]
	    (incf n))))))

(define-method add-segment agent (&optional force-row force-column)
  (clon:with-fields (segments) self
    (multiple-value-bind (row column)
	(if (or (null segments) (or force-row force-column))
	    (step-in-direction <row> <column> (opposite-direction <last-direction>))
	    (when (and (consp segments)
		       (consp (last segments))
		       (clon:object-p (car (last segments))))
	      (clon:with-field-values (row column last-direction) (car (last segments))
		(step-in-direction row column (opposite-direction last-direction)))))
      (let ((segment (clone =segment=)))
	[drop-cell *world* segment (or force-row) (or force-column column)]
	(push segment segments)))))

(define-method space-at-head agent ()
  (step-in-direction <row> <column> <direction>))

(define-method category-at-head agent (category)
  (multiple-value-bind (row column) 
      [space-at-head self]
    [category-at-p *world* row column category]))

(define-method item-at-head agent ()
  [category-at-head self :item])

(define-method obstacle-at-head agent ()
  [category-at-head self :obstacle])
  
(define-method push agent () 
  ;; TODO verify enough segments
  (let ((item [item-at-head self]))
    (if item
	(progn (push item <items>)
	       [play-sample self "doorbell"]
	       [delete-from-world item])
	[say self "Nothing to push."])))
	
(define-method pop agent ()
  (clon:with-fields (items) self
    (multiple-value-bind (row column)
	[space-at-head self]
      (if [category-at-head self :obstacle]
	  [say self "Cannot drop item."]
	  (progn
	    (let ((item (car items)))
	      (if (clon:object-p item)
		  (progn (setf items (delete item items))
			 [play-sample self "doorbell2"]
			 [drop-cell *world* item row column])
		  [say self "Nothing to drop."])))))))

(define-method do-action agent ()
  (if [in-overworld self]
      (let ((gateway [category-at-p *world* <row> <column> :gateway]))
	(if (clon:object-p gateway)
	    [activate gateway]
	    (error "No gateway.")))
      (cond ([category-at-head self :action]
	     [do-action [category-at-head self :action]])
	    ([category-at-head self :item]
	     [push self])
	    (t 
	     (if (car <items>)
		 [pop self]
		 (progn 
		   [play-sample self "error"]
		   [say self "Nothing to do here."]))))))
	   
(define-method expend-item agent ()
  (pop <items>))

(define-method rotate agent () 
  (clon:with-fields (items) self
    (when items
      (let ((tail (pop items)))
	(setf items (append items (list tail)))))))

(define-method call agent (&optional direction)
  (when direction
    [aim self direction])
  (let ((item (car <items>)))
    (if (and item [in-category item :item]
	     (clon:has-method :call item))
	[call item self]
	[say self "Cannot call."])))

(define-method run agent () 
  [update-tiles self])
  
(define-method quit agent ()
  (xe2:quit :shutdown))

(define-method exit agent ()
  (dolist (segment <segments>)
    [die segment])
  (setf <segments> nil))

(define-method die agent ()
  (unless <dead>
    (setf <tile> "agent-disabled")
    [play-sample self "gameover"]
    [say self "You died. Press escape to reset."]
    (setf <dead> t)))

(define-method restart agent ()
  (let ((agent (clone =agent=)))
    [say self "Restarting CONS..."]
    (halt-sample t)
    [destroy *universe*]
    [set-player *universe* agent]
;;    [set-prompt *form* agent]
    [set-character *status* agent]
    [play *universe*
	  :address (list '=alien-base= :sequence-number (genseq))]
    [loadout agent]))
