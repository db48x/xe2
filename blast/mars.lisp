(in-package :blast)

;;; Martian terrain

(defcell mars-flat
  (tile :initform "mars-terrain-flat"))

(define-method step mars-flat (stepper)
  (when (has-field :endurium stepper)
    [>>stat-effect stepper :endurium -0.01]))

(define-prototype mars-icy (:parent =mars-flat=)
  (tile :initform "mars-terrain-icy"))

(define-prototype mars-icy2 (:parent =mars-flat=)
  (tile :initform "mars-terrain-icy2"))

(define-prototype mars-tundra (:parent =mars-flat=)
  (tile :initform "mars-tundra"))

(define-prototype mars-dark (:parent =mars-flat=)
  (tile :initform "mars-terrain-dark"))

(define-prototype mars-cracks (:parent =mars-flat=)
  (tile :initform "mars-terrain-cracks"))

;;; Mining sites

(define-prototype mining-site (:parent rlx:=world=)
  (scale :initform '(1 km))
  (height :initform 40)
  (width :initform 40))

(define-method generate mining-site (&key technetium
					  endurium
					  biosilicate 
					  scanners
					  snowy-p
					  &allow-other-keys)
  [create-default-grid self]
  (clon:with-field-values (height width) self
    (let ((dust-plasma (rlx:render-plasma height width :graininess 0.7)) 
	  (ice-plasma (rlx:render-plasma height width :graininess 0.3)) 
	  (value nil))
      (dotimes (i height)
	(dotimes (j width)
	  (setf value (aref dust-plasma i j))
	  [drop-cell self (clone (if (< 0 value)
				     (if (< 0.2 value) 
					 =mars-tundra=
					 =mars-cracks=)
				     =mars-dark=))
		     i j :no-collisions t]
	  (when snowy-p
	    (setf value (aref ice-plasma i j))
	    (when (< 0 value)
	    [drop-cell self (clone =mars-icy2=) i j]))))
      ;; deposit minerals and stuff
      (dotimes (n technetium)
	[drop-cell self (clone =technetium=) (random height) (random width)])
      (dotimes (n endurium)
	[drop-cell self (clone =crystal=) (random height) (random width)])
      (dotimes (n biosilicate)
	[drop-cell self (clone =biosilicate=) (random height) (random width)])
      (dotimes (n scanners)
	[drop-cell self (clone =scanner=) (random height) (random width) :loadout t])
      [drop-cell self (clone =launchpad=) (random height) (random width) :loadout t])))

(define-method begin-ambient-loop mining-site ()
  (rlx:play-music "crisis" :loop t))

(define-prototype mining-site-gateway (:parent =gateway=)
  (tile :initform "pickaxe")
  (address :initform (list '=mining-site= 
			   :sequence (genseq)
			   :technetium (random 15)
			   :endurium (random 5)
			   :biosilicate (random 20)
			   :scanners (random 8))))

(define-method initialize mining-site-gateway (&key snowy-p)
  (setf <address> 
	(append <address>
		(list :snowy-p snowy-p))))

(define-method step mining-site-gateway (stepper)
  [>>narrateln :narrator "This area looks promising. Press RETURN to land."])
 
;;; Planet map.

(define-prototype mars (:parent rlx:=world=)
  (height :initform 20)
  (width :initform 45)
  (technetium :initform 0)
  (scale :initform '(2000 km)))

(define-method draw-terrain mars ()
  (clon:with-field-values (height width) self
    (let ((dust-plasma (rlx:render-plasma height width :graininess 0.2)) 
	  (ice-plasma (rlx:render-plasma height width :graininess 0.7)) 
	  (value nil))
      (dotimes (i height)
	(dotimes (j width)
	  (setf value (aref dust-plasma i j))
	  [drop-cell self (clone (if (< 0 value)
				     (if (< 0.3 value) 
					 =mars-flat=
					 =mars-cracks=)
				     =mars-dark=))
		     i j :no-collisions t]
	  (setf value (aref ice-plasma i j))
	  (when (< 0 value)
	    [drop-cell self (clone =mars-icy2=) i j])))
      ;; draw polar ice caps
      (dotimes (j width)
	(dotimes (n (1+ (random 3)))
	  [drop-cell self (clone =mars-tundra=) n j])
	(dotimes (n (1+ (random 3)))
	  [drop-cell self (clone =mars-tundra=) (- height n 1) j]))
      ;; deposit mining sites
      (dotimes (n (+ 3 (random 8)))
	(let* ((r (random height))
	       (c (random width))
	       (snowy-p (< 0 (aref ice-plasma r c))))
	  [drop-cell self (clone =mining-site-gateway=
				 :snowy-p snowy-p) 
		     (random height) (random width)])))))
	
(define-method generate mars (&rest parameters)
  [create-default-grid self]
  [draw-terrain self])

(define-method begin-ambient-loop mars ()
  (play-music "black-thunder" :loop t))

