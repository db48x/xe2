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

(defcell cube-box 
  (name :initform "Cube box")
  (tile :initform "blue-arrowbox")
  (categories :initform '(:obstacle :opaque :pushable))
  (description :initform
"Strange boxes appear to have almost no weight, and bullets move them
around."))

(define-method push cube-box (dir)
  [move self dir])

(define-prototype cube (:parent =world=)
  (scale :initform '(10 m))
  (required-modes :initform '(:spacesuit :olvac :vomac :vehicle))
  (width :initform 100)
  (height :initform 100)
  (name :initform "Ancient cube")
  (ambient-light :initform 10))

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
		   [drop-cell self (clone =cube-box=) y x :no-collisions t])))
      ;; create border around world
      (trace-rectangle #'drop-wall
		       0 0 width height)
      ;; drop maze
      [drop-maze self]
      [drop-specials self]
      (dotimes (i 100)
	(drop-box (random height) (random width))))
    [drop-cell self (clone =launchpad=) 10 10]))




(define-method drop-maze cube ()
  nil)

(define-method drop-specials cube  ()
  nil)


      

  
