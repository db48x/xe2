(in-package :xong)

;;; Text overlay balloons

(defcell balloon 
  (categories :initform '(:drawn :actor))
  text stroke-color background-color timeout)

(define-method initialize balloon (&key text (stroke-color ".white") (background-color ".blue")
					(style :balloon) (timeout nil))
  (setf <text> text)
  (setf <stroke-color> stroke-color)
  (setf <background-color> background-color)
  (setf <style> style)
  (setf <timeout> (if (floatp timeout)
		      ;; specify in (roughly) seconds if floating
		      (truncate (* 15 timeout))
		      ;; leave as frames if integer
		      timeout)))
  
(define-method draw balloon (x y image)
  (clon:with-field-values (text style) self
    (let* ((offset (ecase style
		     (:balloon 16)
		     (:flat 0)))
	   (x0 (+ x offset))
	   (y0 (+ y offset))
	   (x1 (+ x0 offset))
	   (y1 (+ y0 offset))
	   (margin 4)
	   (height (+ (* 2 margin) (apply #'+ (mapcar #'formatted-line-height text))))
	   (width (+ (* 2 margin) (apply #'max (mapcar #'formatted-line-width text)))))
      (draw-box x1 y1 width height 
		:stroke-color <stroke-color>
		:color <background-color>
		:destination image)
      (when (eq style :balloon)
	(draw-line x0 y0 x1 y1 :destination image))
      (let ((x2 (+ margin x1))
	    (y2 (+ margin y1)))
	(dolist (line text)
	  (render-formatted-line line x2 y2 :destination image)
	  (incf y2 (formatted-line-height line)))))))

(define-method run balloon ()
  [expend-default-action-points self]
  (when (integerp <timeout>)
    (when (minusp (decf <timeout>))
      [die self])))
		  
;;; Tutorial NPC: Beckoner

(defparameter *greeting-text* '((("Hello there!"))))

(defparameter *beckon-text* '((("Use the arrow keys")) (("to approach me."))))

(defparameter *success-text* '((("Good job. You've mastered the use")) (("of the arrow keys."))))

(defparameter *bye-text* '((("You can exit through one")) (("of the portals."))
			   (("Portals look like this: ") (nil :image "portal"))))

(defcell beckoner 
  (tile :initform "npc")
  (state :initform 0)
  (categories :initform '(:obstacle :actor :npc))
  (timeout :initform 40))
      
(define-method emote beckoner (text &optional (timeout 3.0))
  (let ((balloon (clone =balloon= :text text :timeout timeout)))
    [play-sample self "talk"]
    [drop self balloon]))

(define-method run beckoner ()
  (clon:with-fields (state timeout) self
    [expend-default-action-points self]
    (labels ((act ()
	       (ecase state 
		 (0 [emote self *greeting-text* 1.0]
		    (incf state)
		    (setf timeout 20))
		 (1 [emote self *beckon-text*]
		    (setf timeout 100))
		 (2 [emote self *success-text*]
		    (incf state)
		    (setf timeout 50))
		 (3 [emote self *bye-text*]
		    (setf timeout 200)))))
      (if (and (not (= 3 state))
	       (< [distance-to-player self] 5))
	  (progn (setf state 2) (act))
	  (if (null timeout)
	      (act)
	      (when (minusp (decf timeout))
		(setf timeout nil)
		(act)))))))

;;; Tutorial level 1: puckman

(defvar *bricks* 0)

(defcell brick
  (tile :initform "wall-red")
  (categories :initform '(:obstacle :paint-source :wall :brick))
  (color :initform :red))

(define-method initialize brick ()
  (incf *bricks*))

(define-method die brick ()
  (decf *bricks*)
  [parent>>die self])
    
(defparameter *puck-welcome-text* '((("Hello and welcome to the tutorial!"))))

(defparameter *puck-instruction-text* '((("Use CONTROL with an arrow-key to"))
					(("fire your puck at the bricks."))
					(("See if you can break them all."))))

(defparameter *puck-success-text* '((("Good job. You've mastered the use")) (("of the puck."))))

(defparameter *puck-bye-text* '((("You can exit through the")) (("portal to the southeast."))))

(defcell puckman 
  (tile :initform "npc")
  (state :initform 0)
  (categories :initform '(:obstacle :actor :npc))
  (timeout :initform 10))
      
(define-method emote puckman (text &optional (timeout 3.0))
  (let ((balloon (clone =balloon= :text text :timeout timeout)))
    [play-sample self "talk"]
    [drop self balloon]))

(define-method run puckman ()
  (message "BRICKS ~S" *bricks*)
  (clon:with-fields (state timeout) self
    [expend-default-action-points self]
    (labels ((act ()
	       (ecase state 
		 (0 [emote self *puck-welcome-text* 1.0]
		    (incf state)
		    (setf timeout 20))
		 (1 [emote self *puck-instruction-text*]
		    (setf timeout 200))
		 (2 [emote self *puck-success-text*]
		    (incf state)
		    (setf timeout 60))
		 (3 [emote self *puck-bye-text*]
		    (setf timeout 200)))))
      (if (and (not (= 3 state))
	       (zerop *bricks*))
	  (progn (setf state 2) (act))
	  (if (null timeout)
	      (act)
	      (when (minusp (decf timeout))
		(setf timeout nil)
		(act)))))))

(define-prototype puckman-world (:parent xe2:=world=)
  (height :initform *xong-level-height*)
  (width :initform *xong-level-width*)
  (edge-condition :initform :block)
  (ambient-light :initform :total))

(define-method generate puckman-world (&rest args)
  [create-default-grid self]
  (setf <level> 0 *bricks* 0)
  (clon:with-fields (height width grid player) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))
    (dotimes (i 3)
      (dotimes (j 3)
	[drop-cell self (clone =brick=) (+ 10 i) (+ 10 j)]))
    (let ((puckman (clone =puckman=)))
      [drop-cell self puckman 3 5]
      [drop-cell self (clone =portal=
			     :address '(=chevronman-world=)
			     :text '((("To part 2"))))
		 18 30 :loadout t])))

(define-method begin-ambient-loop puckman-world ()
  (play-music "sparqq" :loop t))

;;; Tutorial level 2: Chevrons

(defvar *chevron-enemies* 0)

(defcell enemy
  (tile :initform "enemy")
  (categories :initform '(:actor :target :obstacle :enemy :puck))
  (direction :initform :east)
  (stepping :initform t))

(define-method kick enemy (direction)
  (setf <direction> direction))

(define-method run enemy ()
  (clon:with-field-values (row column) self
    (when [obstacle-in-direction-p *world* row column <direction>]
      (setf <direction> (random-direction)))
      [move self <direction>])
  [expend-action-points self 25])

(define-method loadout enemy ()
  (incf *chevron-enemies*))

(define-method die enemy ()
  (decf *chevron-enemies*)
  [parent>>die self])

(defparameter *chevron-welcome-text* '((("Now you'll learn about chevrons."))))

(defparameter *chevron-instruction-text* '((("Use ALT (or OPTION on the Mac)"))
					   (("with an arrow-key to drop a chevron."))
					   (("The chevron directs enemies toward the"))
					   (("direction it points in. See if you can"))
					   (("defeat the enemy ") (nil :image "enemy") (" by directing it"))
					   (("into the Black hole, i.e. ") (nil :image "hole") (" ."))))

(defparameter *chevron-success-text* '((("Good job. You've mastered the use")) (("of the chevron."))))

(defparameter *chevron-bye-text* '((("You can exit through the")) (("portal to the northeast."))))

(defcell chevronman 
  (tile :initform "npc")
  (state :initform 0)
  (categories :initform '(:obstacle :actor :npc))
  (timeout :initform 10))
      
(define-method emote chevronman (text &optional (timeout 3.0))
  (let ((balloon (clone =balloon= :text text :timeout timeout)))
    [play-sample self "talk"]
    [drop self balloon]))

(define-method run chevronman ()
  (clon:with-fields (state timeout) self
    [expend-default-action-points self]
    (labels ((act ()
	       (ecase state 
		 (0 [emote self *chevron-welcome-text* 1.0]
		    (incf state)
		    (setf timeout 20))
		 (1 [emote self *chevron-instruction-text* 4.0]
		    (setf timeout 300))
		 (2 [emote self *chevron-success-text*]
		    (incf state)
		    (setf timeout 50))
		 (3 [emote self *chevron-bye-text*]
		    (setf timeout 200)))))
      (if (and (not (= 3 state))
	       (zerop *chevron-enemies*))
	  (progn (setf state 2) (act))
	  (if (null timeout)
	      (act)
	      (when (minusp (decf timeout))
		(setf timeout nil)
		(act)))))))

(define-prototype chevronman-world (:parent xe2:=world=)
  (height :initform *xong-level-height*)
  (width :initform *xong-level-width*)
  (edge-condition :initform :block)
  (ambient-light :initform :total))

(define-method generate chevronman-world (&rest args)
  [create-default-grid self]
  (setf <level> 0 *chevron-enemies* 0)
  (clon:with-fields (height width grid player) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =floor=) i j]))
    (let ((chevron-1 (clone =chevron=))
	  (chevron-2 (clone =chevron=))
	  (chevronman (clone =chevronman=))
	  (enemy (clone =enemy=)))
      [orient chevron-1 :east]
      [orient chevron-2 :west]
      [drop-cell self chevron-1 10 10]
      [drop-cell self chevron-2 10 20]
      (dotimes (n 9)
	[drop-cell self (clone =hole= :nospew t) 20 (+ n 11)])
      (dotimes (n 9)
	[drop-cell self (clone =hole= :nospew t) 5 (+ n 11)])
      (dotimes (n 5)
	(let ((diamond (clone =diamond=)))
	  [drop-cell self diamond (+ 20 (random 5)) (+ 30 (random 10))]
	  (when (= n 0)
	    [drop diamond (clone =balloon= :text '((("Extra chevrons."))))])))
      [drop-cell self enemy 10 15]
      [loadout enemy]
      [drop-cell self chevronman 20 5]
      [drop-cell self (clone =portal=
			     :address '(=multiman-world=)
			     :text '((("To part 3"))))
		 6 30 :loadout t])))

(define-method begin-ambient-loop chevronman-world ()
  (play-music "phong" :loop t))

;;; MULTIMAN: use multiple skills

(defparameter *multi-welcome-text* '((("Now combine your skills")) (("to defeat multiple enemies."))))

(defparameter *multi-instruction-text* '((("Use multiple chevrons to direct"))
					 (("enemies around the obstacles and"))
					 (("into the black holes. Use the puck"))
					 (("to break any bricks blocking your path."))))

(defparameter *multi-success-text* '((("Great! You've combined"))
				     (("your skills and learned"))
				     (("the basics of XONG."))))

(defparameter *multi-bye-text* '((("Exit through the portal to")) (("the northeast to begin"))
				 (("playing Level 1 of XONG."))))

(defcell multiman 
  (tile :initform "npc")
  (state :initform 0)
  (categories :initform '(:obstacle :actor :npc))
  (timeout :initform 10))
      
(define-method emote multiman (text &optional (timeout 3.0))
  (let ((balloon (clone =balloon= :text text :timeout timeout)))
    [play-sample self "talk"]
    [drop self balloon]))

(define-method run multiman ()
  (clon:with-fields (state timeout) self
    [expend-default-action-points self]
    (labels ((act ()
	       (ecase state 
		 (0 [emote self *multi-welcome-text* 1.0]
		    (incf state)
		    (setf timeout 20))
		 (1 [emote self *multi-instruction-text* 4.0]
		    (setf timeout 300))
		 (2 [emote self *multi-success-text*]
		    (incf state)
		    (setf timeout 35))
		 (3 [emote self *multi-bye-text*]
		    (setf timeout 200)))))
      (if (and (not (= 3 state))
	       (zerop *chevron-enemies*))
	  (progn (setf state 2) (act))
	  (if (null timeout)
	      (act)
	      (when (minusp (decf timeout))
		(setf timeout nil)
		(act)))))))

(define-prototype multiman-world (:parent xe2:=world=)
  (height :initform *xong-level-height*)
  (width :initform *xong-level-width*)
  (edge-condition :initform :block)
  (ambient-light :initform :total))

(define-method generate multiman-world (&rest args)
  [create-default-grid self]
  (setf <level> 0 *chevron-enemies* 0)
  (labels ((drop-bulkhead (r c)
	     (prog1 nil
	       [drop-cell self (clone =bulkhead=) r c]))
	   (drop-brick (r c)
	     (prog1 nil
	       [drop-cell self (clone =brick=) r c])))
    (clon:with-fields (height width grid player) self
      (dotimes (i height)
	(dotimes (j width)
	  [drop-cell self (clone =floor=) i j]))
      (let ((chevron-1 (clone =chevron=))
	    (chevron-2 (clone =chevron=))
	    (chevron-3 (clone =chevron=))
	    (chevron-4 (clone =chevron=))
	    (multiman (clone =multiman=))
	    (enemy-1 (clone =enemy=))
	    (enemy-2 (clone =enemy=)))
	[orient chevron-1 :east]
	[orient chevron-2 :west]
	[drop-cell self chevron-1 15 10]
	[drop-cell self chevron-2 15 20]
	[orient chevron-3 :east]
	[orient chevron-4 :west]
	[drop-cell self chevron-3 8 10]
	[drop-cell self chevron-4 8 20]
	(trace-row #'drop-bulkhead 5 8 22)
	(trace-row #'drop-bulkhead 17 8 22)
	(dotimes (n 8)
	  [drop-cell self (clone =hole= :nospew t) 2 (+ n 11)])
	(dotimes (n 8)
	  [drop-cell self (clone =hole= :nospew t) 20 (+ n 11)])
	(trace-rectangle #'drop-brick 1 10 3 10)
	(trace-rectangle #'drop-brick 19 10 3 10)
	(dotimes (n 5)
	  (let ((diamond (clone =diamond=)))
	    [drop-cell self diamond (+ 20 (random 5)) (+ 30 (random 10))]
	    (when (= n 0)
	      [drop diamond (clone =balloon= :text '((("Extra chevrons."))))])))
	(dotimes (n 2)
	  (let ((puckup (clone =puckup=)))
	    [drop-cell self puckup (+ 20 (random 5)) (+ 3 (random 5))]
	    (when (= n 0)
	      [drop puckup (clone =balloon= :text '((("Extra pucks."))))])))
	[drop-cell self enemy-1 15 18]
	[drop-cell self enemy-2 8 15]
	[loadout enemy-1]
	[loadout enemy-2]
	[drop-cell self multiman 15 25]
	[drop-cell self (clone =portal=
			       :address (generate-level-address 1)
			       :text '((("Exit to Level 1"))))
		   6 30 :loadout t]))))
  
(define-method begin-ambient-loop multiman-world ()
    (play-music "neon" :loop t))
		       
