(in-package :forest)

;;; Dandelion seeds

(defsprite dandelion
  (image :initform "dandelion")
  (speed :initform (make-stat :base 10))
  (categories :initform '(:actor))
  (speed :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 10))
  (movement-distance :initform 2)
  (clock :initform 10))

(define-method run dandelion ()
  (clon:with-fields (clock) self
    (if (plusp clock) 
	(progn 
	  (decf clock)
	  [move self (car (one-of '(:southeast :east :east))) 2])
	[die self])))

;;; Ancient road

(defparameter *road-width* 10)

(defparameter *road-tiles* '("road-1" "road-2" "road-3" "road-2" "road-2"))

(defcell road
  (description :initform "This ancient road has been patched, but never fully rebuilt.")
  (tile :initform (car (one-of *road-tiles*))))

;;; Brother Lothaine

(defparameter *greeting-text* '((("Greetings, Brother."))))

(defparameter *beckon-text* '((("We've been waiting for you."))
			      (("Come with me, I'll show you to"))
			      (("your room."))))

(defparameter *success-text* '((("Let's move along now."))))

(defparameter *thisway-text* '((("This way to your quarters."))))

(defcell lothaine 
  (tile :initform "lothaine")
  (state :initform 0)
  (categories :initform '(:obstacle :actor :npc))
  (running :initform t)
  (timeout :initform 40))
      
(define-method emote lothaine (text &optional (timeout 3.0))
  (let ((balloon (clone =balloon= :text text :timeout timeout)))
    [play-sample self "talk"]
    [drop self balloon]))

(define-method run lothaine ()
  (when <running>
    (clon:with-fields (state timeout) self
      [expend-default-action-points self]
      (labels ((act ()
		 (case state 
		   (0 (when (< [distance-to-player self] 15)
			[emote self *greeting-text* 1.0]
			(incf state)
			(setf timeout 20)))
		   (1 [emote self *beckon-text*]
		      (setf timeout 100))
		   (2 [emote self *success-text*]
		      (incf state)
		      (setf timeout 29))
		   (3 [emote self *thisway-text*]
		      [drop self (clone =quarters-gateway=)]
		      [move self :north]
		      (incf state)
		      (setf <running> nil)))))
	(if (and (not (= 3 state))
		 (< [distance-to-player self] 5))
	    (progn (setf state 2) (act))
	    (if (null timeout)
		(act)
		(when (minusp (decf timeout))
		  (setf timeout nil)
		  (act))))))))

;;; Monastery approach world

(defcell hill-1
  (description :initform "This gentle slope heads down toward the Monastery.")
  (tile :initform "hill-1"))

(defcell hill-2
  (description :initform "This gentle slope heads down toward the Monastery.")
  (tile :initform "hill-2"))

(defcell hill-3 
  (description :initform "This gentle slope heads down toward the Monastery.")
  (tile :initform "hill-3"))

(defcell flowers-1
  (description :initform "Gorgeous wildflowers.")
  (tile :initform "flowers-1"))

(define-prototype flowers-2 (:parent =flowers-1=)
  (description :initform "Wildflowers of every description.")
  (tile :initform "flowers-2")
  (categories :initform '(:actor)))

(define-method run flowers-2 ()
  (percent-of-time 1
    (multiple-value-bind (x y) [xy-coordinates self]
      [drop-sprite self (clone =dandelion=) x y])))

(define-prototype monastery-gateway (:parent =gateway=)
  (tile :initform "monastery-gateway")
  (description :initform "The mountain pass opens here to the foothills by the Monastery.")
  (sequence-number :initform (genseq))
  (address :initform (generate-level-address 5)))

(define-method step monastery-gateway (stepper)
  [say self "The mountain pass opens to the foothills by the Monastery here."]
  [say self "Press ENTER to continue on."])

(define-prototype monastery (:parent xe2:=world=)
  (height :initform *forest-height*)
  (width :initform *forest-width*)
  (ambient-light :initform :total)
  (description :initform 
"Morning has broken, and Valisade Monastery has come into view to the
south. You can hear the monks singing in the distance.")
  (edge-condition :initform :block))

(define-method drop-road monastery ()
  (labels ((drop (r c)
	     (prog1 nil 
	       (percent-of-time 95 
		 [drop-cell self (clone =road=) r c]))))
    (dotimes (row <height>)
      (trace-row #'drop row (+ 15 (random 3)) (+ 15 *road-width* (random 3))))))

(define-method drop-hill monastery (&key (graininess 0.3)
				       (density 60)
				       distance
				       (cutoff 0.3))
  (clon:with-field-values (height width) self
    (let ((plasma (xe2:render-plasma height width :graininess graininess))
	  (plasma2 (xe2:render-plasma height width :graininess graininess))
	  (value nil))
      (dotimes (i height)
	(dotimes (j width)
	  [drop-cell self (clone =hill-3=) i j]))
      (dotimes (i height)
	(dotimes (j width)
	  (setf value (aref plasma i j))
	  (when (or (null distance)
		    (< (distance j i row column) distance))
	    (percent-of-time density
	      [drop-cell self (if (< cutoff value)
				  (clone =hill-1=)
				  (clone =hill-2=))
			 i j :no-collisions t]))))
      (dotimes (i height)
	(dotimes (j width)
	  (setf value (aref plasma i j))
	  (when (or (null distance)
		    (< (distance (+ j r0) (+ c0 i) row column) distance))
	    (percent-of-time density
	      [drop-cell self (if (< cutoff value)
				  (clone =flowers-1=)
				  (clone =flowers-2=))
			 i j :no-collisions t])))))))

(defcell sheep 
  (tile :initform "sheep")
  (speed :initform (make-stat :base 2))
  (categories :initform '(:obstacle :actor))
  (hit-points :initform (make-stat :base 10 :min 0))
  (direction :initform :south))

(define-method run sheep ()
  (percent-of-time 15
    (setf <direction> (random-direction)))
  (percent-of-time 80 [move self <direction>]))
    
(define-method drop-sheep monastery (&optional (sheep 10))
  (dotimes (i sheep)
    (multiple-value-bind (r c) [random-place self]
      [drop-cell self (clone =sheep=) r c :loadout t])))

(define-method generate monastery (&key (height 100)
				      (width *forest-width*)
				      sequence-number)
  (setf <height> height)
  (setf <width> width)
  (setf <sequence-number> sequence-number)
  [create-default-grid self]
  [drop-hill self]
  [drop-road self]
  [drop-sheep self]
  (let ((row (1+ (random 10)) )
	  (column (+ 15 (random 6))))
      [drop-cell self (clone =drop-point=) row column
		 :exclusive t :probe t])
  [drop-cell self (clone =lothaine=) 85 10])

(define-method begin-ambient-loop monastery ()
  (play-music "rain" :loop t)
  (play-sample "monks"))

;;; Your quarters

(define-prototype quarters-gateway (:parent =gateway=)
  (tile :initform "quarters-gateway")
  (address :initform (generate-level-address 6)))

(define-method step quarters-gateway (stepper)
  (when [is-player stepper]
    [say self "The way to your quarters. Press ENTER to continue."]))

(defcell quarters-floor
  (tile :initform (car (one-of '("quarters-floor-1" "quarters-floor-1" "quarters-floor-2")))))

(defcell quarters-wall
  (tile :initform "quarters-wall")
  (categories :initform '(:obstacle)))

(defcell desk
  (tile :initform "desk")
  (categories :initform '(:obstacle)))

(defcell bed
  (tile :initform "bed")
  (categories :initform '(:obstacle)))

(defcell letter
  (tile :initform "letter")
  (categories :initform '(:item)))

(define-method step letter (stepper)
  (when [is-player stepper]
    [emote stepper '((("It's a letter for me.")))]
    [say stepper "You pick up the letter."]
    [take stepper :direction :here :category :item]))
    
(define-method use letter (user)
  (let ((box (clone =textbox=)))
    [resize-to-scroll box :height 540 :width 800] 
    [move box :x 0 :y 0]
    (let ((text	(find-resource-object "letter-text")))
      [set-buffer box text])
    (install-widgets box)))
 
(define-prototype quarters (:parent xe2:=world=)
  (height :initform 9)
  (width :initform 7)
  (ambient-light :initform :total)
  (description :initform 
"The monks have given you a spare but comfortable room.")
  (edge-condition :initform :block))

(define-method generate quarters (&rest params)
  (clon:with-field-values (height width) self
    [create-default-grid self]
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =quarters-floor=) i j]))
    [drop-cell self (clone =desk=) 2 3]
    [drop-cell self (clone =bed=) 1 5]
  [drop-cell self (clone =letter=) 2 2]
  (labels ((drop-wall (r c)
	     (prog1 nil
	       [drop-cell self (clone =quarters-wall=) r c])))
    (xe2:trace-rectangle #'drop-wall 0 0 height width))
  (let ((row 3)
	(column 3))
    [drop-cell self (clone =drop-point=) row column
	       :exclusive t :probe t])))

(define-method begin-ambient-loop quarters ()
  (play-music "solace"))


