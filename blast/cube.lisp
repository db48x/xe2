(in-package :blast)

(defcell cube-wall
  (name :initform "Cube wall")
  (tile :initform "cube-wall")
  (categories :initform '(:opaque :obstacle))
  (descriptions :initform "Ultra-hard yellow surface inscribed with angular marks."))

(defcell cube-floor
  (name :initform "Cube floor")
  (tile :initform "cube-floor")
  (description :initform 
"You will use 1 unit of oxygen for each square moved,
or each turn waited. Melee combat uses 2 units per hit."))

(defcell blue-arrowbox
  (name :initform "Cube box")
  (color :initform :blue)
  (stepping :initform t)
  (tile :initform "blue-arrowbox")
  (categories :initform '(:obstacle :pushable :arrowbox))
  (description :initform
"Strange boxes appear to have almost no weight, and bullets move them
around."))

(define-method push blue-arrowbox (dir)
  [move self dir :ignore-obstacles])

(define-prototype turquoise-arrowbox (:parent =blue-arrowbox=)
  (color :initform :turquoise)
  (tile :initform "turquoise-arrowbox"))

(define-prototype red-arrowbox (:parent =blue-arrowbox=)
  (color :initform :red)
  (tile :initform "red-arrowbox"))

(defparameter *receptacle-colors* 
  '(:red :turquoise :blue))

(defun random-receptacle-color ()
  (car (one-of *receptacle-colors*)))

(defparameter *receptacle-tiles*
  '(:red "red-arrowbox-receptacle" 
    :turquoise "turquoise-arrowbox-receptacle" 
    :blue "blue-arrowbox-receptacle" ))

(defcell arrowbox-receptacle 
  (categories :initform '(:obstacle :opaque))
  color)

(define-method initialize arrowbox-receptacle (&optional (color (random-receptacle-color)))
  (setf <color> color)
  (setf <tile> (getf *receptacle-tiles* color)))

(define-method step arrowbox-receptacle (stepper)
  (when (and [in-category stepper :arrowbox]
	     (eq <color> (field-value :color stepper)))
    [play-sample self "worp"]
    [say self "The box and lock both disappear."]
    [drop self (clone =energy=)]
    [die self]
    [die stepper]))

;;; The cube world

(define-prototype cube (:parent =world=)
  (scale :initform '(10 m))
  (room-size :initform 10)
  (required-modes :initform '(:spacesuit :olvac :vomac :vehicle))
  (width :initform 100)
  (height :initform 100)
  (name :initform "Ancient cube")
  (ambient-light :initform 10))

(define-method begin-ambient-loop cube ()
  (play-music "ancients" :loop t))

(define-method generate cube (&key sequence-number)
  [create-default-grid self]
  (clon:with-field-values (height width) self
    ;; drop floors
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =cube-floor=) i j]))
    ;; create walls
    (labels ((drop-wall (x y)
	       (prog1 nil
		 [drop-cell self (clone =cube-wall=) y x]))
	     (drop-box (x y)
	       (prog1 nil 
		   [drop-cell self (clone (car (one-of (list =blue-arrowbox= =turquoise-arrowbox= =red-arrowbox=))))
			      y x :no-collisions t])))
      ;; create border around world
      (trace-rectangle #'drop-wall
		       0 0 width height)
      ;;
      [drop-maze self]
      [drop-specials self]
      (dotimes (i 200)
	(drop-box (random height) (random width))))
    [drop-cell self (clone =launchpad=) 10 10]))

(define-method drop-maze cube ()
  (clon:with-field-values (height width room-size) self
    (labels ((drop-wall (r c)
	       (prog1 nil 
		 [drop-cell self (clone =cube-wall=) r c]))
	     (drop-room (r c)
	       (trace-rectangle #'drop-wall r c (1+ <room-size>) (1+ <room-size>)))
	     (maybe-remove-obstacles (r c probability)
	       (percent-of-time probability
		 (let (obstacle)
		   (loop while (setf obstacle [category-at-p *active-world* r c :obstacle])
			 do [delete-from-world obstacle]))))
	     (maybe-drop-lock (r c probability)
	       (percent-of-time probability
		 [drop-cell self (clone =arrowbox-receptacle=) r c]))
	     (open-room (r c side)
	       (dotimes (i 4)
		 (multiple-value-bind (row column)
		     (ecase side
		       (:top (values r (+ c (random room-size))))
		       (:bottom (values (+ r room-size) (+ c (random room-size))))
		       (:left (values (+ r (random room-size)) c))
		       (:right (values (+ r (random room-size)) (+ c room-size))))
		   (maybe-remove-obstacles row column 100)
		   (maybe-drop-lock row column 95)))))
      (dotimes (i (truncate (/ width room-size)))
	(dotimes (j (truncate (/ height room-size)))
	  (let ((r0 (1- (* i room-size)))
		(c0 (1- (* j room-size))))
	    (drop-room r0 c0)
	    (dotimes (i 3)
	      (open-room r0 c0 (car (one-of '(:top :bottom :left :right)))))))))))

	   


	     
    



(define-method drop-specials cube  ()
  nil)


      

  
