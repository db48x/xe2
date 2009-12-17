(in-package :forest)

;;; Icy tundra

(defparameter *tundra-tiles* '("tundra-1" 
			      "tundra-2"
			      "tundra-3"
			      "tundra-4"
			      "tundra-5"
			      "tundra-6"
			      "floor"))

(defparameter *tundra-light-radius* 14)


(defcell tundra 
  (tile :initform "floor")
  (description :initform "This frozen surface is treacherous.")
  (categories :initform '(:actor :reflective)))

(define-method blow tundra (dark)
  (let ((snow [category-at-p *world* <row> <column> :snow]))
    (when snow
      [update-tile snow dark])
    (if (minusp <snow-clock>)
	(progn (setf <snow-clock> *snow-clock*)
	       (if (null snow)
		   (percent-of-time 3
		     (setf snow (clone =snow=))
		     [drop self snow])
		   (percent-of-time 10 
		     [collect snow 1 dark])))
	(decf <snow-clock>))))
    
(define-method run tundra ()
  (let* ((dist [distance-to-player self]))
    (setf <tile> (if (< dist *tundra-light-radius*)
		     (nth (truncate (/ dist 2)) *tundra-tiles*)
		     "floor"))))

;;; Water

(defcell foam 
  (tile :initform "foam")
  (clock :initform 4)
  (description :initform "The shallow water sparkles here where moonlight reflects off of it.")
  (categories :initform '(:actor :ephemeral)))

(define-method run foam ()
  (let ((dir (car (one-of '(:southwest :south :southeast)))))
    (when [category-in-direction-p *world* <row> <column> dir :water]
      [move self dir])
    (decf <clock>)
    (when (minusp <clock>)
      [die self])))

(defparameter *water-tiles* '("water-1"
			     "water-2"
			     "water-3"))

(defcell water 
  (tile :initform "floor")
  (description :initform "These waters of the wasteland are not always fit to drink.")
  (categories :initform '(:actor :reflective :water :exclusive)))

(define-method run water ()
  (let ((dist [distance-to-player self]))
    (setf <tile> (if (< dist *earth-light-radius*)
		     (prog1 (nth (truncate (/ dist 6)) *water-tiles*)
		       (percent-of-time 1 [drop self (clone =foam=)]))
		     "floor"))))

(define-method step water (stepper)
  (when [is-player stepper]
    [wet stepper]))

;;; The storm is an invisible cell that sits in the corner and plays thunder

(defcell storm 
  (tile :initform nil)
  (categories :initform '(:actor))
  (clock :initform 10))

(define-method run storm ()
  [expend-default-action-points self]
  (decf <clock>)
  (if (minusp <clock>)
      (progn 
	(setf <clock> (random (+ 400 (random 200))))
	(message "THUNDER")
	[play-sample self (car (one-of '("thunder-med" "thunder-med" "thunder-big")))]
      (decf <clock>))))
  
;;; The tree

(defcell tree 
  (tile :initform (car (one-of '("tree-2" "tree-2" "tree-2" "tree-3")))) 
  (description :initform "These trees are still green. Perhaps the land is coming back?")
  (categories :initform '(:obstacle :opaque :nosnow :exclusive :target)))

;;; The snow

(defparameter *snow-tiles* '("snow-1"
			     "snow-2"
			     "snow-3"
			     "snow-4"
			     "snow-5"))

(defparameter *snow-dark-tiles* '("snow-dark-1"
				  "snow-dark-2"
				  "snow-dark-3"
				  "snow-dark-4"
				  "snow-dark-5"))

(defcell snow 
  (amount :initform 0)
  (tile :initform "snow-1")
  (categories :initform '(:snow)))

(define-method collect snow (&optional (amount 1) dark)
  (setf <amount> (min (+ amount <amount>) 
		      (length *snow-tiles*)))
    (setf <tile> (nth <amount> (if dark *snow-dark-tiles* *snow-tiles*))))

(define-method update-tile snow (dark)
  (setf <tile> (nth <amount> (if dark *snow-dark-tiles* *snow-tiles*))))


;;; Reflects light 

(defparameter *earth-tiles* '("earth-1" 
			      "earth-2"
			      "earth-3"
			      "earth-4"
			      "earth-5"
			      "earth-6"
			      "floor"))

(defparameter *earth-light-radius* 14)

(defparameter *earth-rain-clock* 10)

(defparameter *snow-clock* 8)

(defcell earth 
  (tile :initform "floor")
  (categories :initform '(:actor :reflective))
  (snow-clock :initform *snow-clock*)
  (description :initform "The solid earth beneath your feet.")
  (clock :initform (random *earth-rain-clock*)))

(define-method snow earth (dark)
  (let ((snow [category-at-p *world* <row> <column> :snow]))
    (when snow
      [update-tile snow dark])
    (if (minusp <snow-clock>)
	(progn (setf <snow-clock> *snow-clock*)
	       (if (null snow)
		   (percent-of-time 3
		     (setf snow (clone =snow=))
		     [drop self snow])
		   (percent-of-time 10 
		     [collect snow 1 dark])))
	(decf <snow-clock>))))
    
(define-method run earth ()
  [expend-action-points self 10]
  (let* ((dist [distance-to-player self])
	 (dark (not (< dist 8))))
    (when [is-snowing *world*]
      [snow self dark])
    (setf <tile> (if (< dist *earth-light-radius*)
		     (prog1 (nth (truncate (/ dist 2)) *earth-tiles*)
		       (if (minusp <clock>)
			   (progn (percent-of-time 5
				    (when [is-snowing *world*]
				      (multiple-value-bind (x y) [viewport-coordinates self]
					[drop-sprite self (clone =snowflake=) x y])))
				  (percent-of-time 5
				    (when [is-raining *world*]
				      (multiple-value-bind (x y) [viewport-coordinates self]
					[drop-sprite self (clone =raindrop=) x y])))
				  (setf <clock> *earth-rain-clock*))
			   (decf <clock>)))
		     "floor"))))
    
;;; The stone wall

(defcell wall
  (tile :initform "wall")
  (description :initform "These crumbling walls are all that remain of some old town.")
  (categories :initform '(:obstacle :opaque :exclusive)))

(defcell debris
  (tile :initform "debris"))

(defcell ruin-floor
  (name :initform "Ruined floor")
  (description :initform "I wonder whose house this was.")
  (tile :initform "ruin-floor"))

;;; Rain and snow

(defsprite raindrop 
  (image :initform "raindrop")
  (categories :initform '(:actor))
  (movement-distance :initform 1)
  (clock :initform 4))

(define-method run raindrop ()
  [expend-default-action-points self]
  (clon:with-fields (clock) self
    (if (plusp clock) 
	(progn 
	  (decf clock)
	  [move self :southeast])
	[die self])))

(defsprite snowflake
  (image :initform "snowflake")
  (categories :initform '(:actor))
  (movement-distance :initform 2)
  (clock :initform 8))

(define-method run snowflake ()
  (clon:with-fields (clock) self
    (if (plusp clock) 
	(progn 
	  (decf clock)
	  [move self :southeast])
	[die self])))

;;; Ambient Fireflies

(defsprite firefly 
  (image :initform "firefly-1")
  (categories :initform '(:actor))
  (movement-distance :initform 1)
  (clock :initform 0))

(define-method run firefly ()
  [expend-default-action-points self]
  (clon:with-fields (clock image) self
    (if (plusp clock) 
	(progn 
	  (setf image (car (one-of '("firefly-1" "firefly-2"))))
	  (decf clock))
	(progn
	  (setf image nil)
	  (when (< [distance-to-player self] 10)
	    (percent-of-time 3
	      (setf clock (+ 5 (random 5)))))))
    [move self (random-direction)]))

;;; Bodies of other adventurers

(defcell body 
  (tile :initform "body")
  (description :initform "This long-dead body may yet hold usable supplies.")
  (categories :initform '(:exclusive)))

(define-method step body (stepper)
  (when [is-player stepper]
    [say self "You search the body."]
    (unless (percent-of-time 40
	      (prog1 t
		[drop self (clone (car (one-of (list =herb= =arrows=))))]))
      [say self "Nothing was found."])
    [die self]))

;;; The forest

(defcell drop-point 
  (categories :initform '(:player-entry-point))
  (tile :initform nil))

(defparameter *forest-width* 49)
(defparameter *forest-height* 100)

(define-prototype forest (:parent xe2:=world=)
  (height :initform *forest-height*)
  (width :initform *forest-width*)
  (gateway-row :initform nil)
  (gateway-column :initform nil)
  (snowing :initform nil)
  (raining :initform nil)
  (ambient-light :initform *earth-light-radius*)
  (description :initform "It is cold and snowing.")
  (edge-condition :initform :block))

(define-method is-snowing forest ()
  <snowing>)

(define-method is-raining forest ()
  <raining>)

(define-method drop-earth forest ()
  (dotimes (i <height>)
    (dotimes (j <width>)
      [drop-cell self (clone =earth=) i j])))

(define-method drop-tundra forest ()
  (dotimes (i <height>)
    (dotimes (j <width>)
      [drop-cell self (clone =tundra=) i j])))

(define-method drop-trees forest (&optional &key (object =tree=)
					    distance 
					    (row 0) (column 0)
					    (graininess 0.3)
					    (density 100)
					    (cutoff 0))
  (clon:with-field-values (height width) self
    (let* ((h0 (or distance height))
	   (w0 (or distance width))
	   (r0 (- row (truncate (/ h0 2))))
	   (c0 (- column (truncate (/ w0 2))))
	   (plasma (xe2:render-plasma h0 w0 :graininess graininess))
	   (value nil))
      (dotimes (i h0)
	(dotimes (j w0)
	  (setf value (aref plasma i j))
	  (when (< cutoff value)
	    (when (or (null distance)
		      (< (distance (+ j r0) (+ c0 i) row column) distance))
	      (percent-of-time density
		[drop-cell self (clone object) i j :no-collisions t]))))))))

(define-method drop-graves forest (row column height width)
  (setf height (max 5 height))
  (setf width (max 5 width))
  (dotimes (i height)
    (dotimes (j width)
      (percent-of-time 40
	[drop-cell self (clone =gravestone=) 
		   (+ (* 2 i) row) 
		   (+ (* 2 j) column)]))))

(define-prototype river-gateway (:parent =gateway=)
  (tile :initform "river-gateway")
  (name :initform "To the River")
  (description :initform "The dense forest gives way here.")
  (sequence-number :initform (genseq))
  (address :initform (generate-level-address 2)))

(define-method step river-gateway (stepper)
  [say self "The river meets the forest here. Press ENTER to continue on."])

(define-prototype ascent-gateway (:parent =gateway=)
  (tile :initform "ascent-gateway")
  (name :initform "To the Ascent")
  (description :initform "The trees thin here and the land becomes more icy.")
  (sequence-number :initform (genseq))
  (address :initform (generate-level-address 3)))

(define-method step ascent-gateway (stepper)
  [say self "The climb begins here. Press ENTER to continue on."])

(define-prototype passage-gateway (:parent =gateway=)
  (tile :initform "passage-gateway")
  (name :initform "Passage to the mountains")
  (description :initform "A steep passageway leads down through the mountains.")
  (sequence-number :initform (genseq))
  (address :initform (list '=passage= :sequence-number (genseq))))

(define-method step passage-gateway (stepper)
  [say self "A pass through the mountains. Press RETURN to enter."])

(define-method activate passage-gateway ()
  (if *lich-alive*
      [say self "The power of the Lich binds you here; you cannot leave!"]
      [parent>>activate self]))

(define-method drop-water forest (&optional &key (object =water=)
					    distance
					    (row 0) (column 0)
					    (graininess 0.3)
					    (density 100)
					    (cutoff 0))
  (clon:with-field-values (height width) self
    (let* ((h0 (or distance height))
	   (w0 (or distance width))
	   (r0 (- row (truncate (/ h0 2))))
	   (c0 (- column (truncate (/ w0 2))))
	   (plasma (xe2:render-plasma h0 w0 :graininess graininess))
	   (value nil))
      (dotimes (i h0)
	(dotimes (j w0)
	  (setf value (aref plasma i j))
	  (when (< cutoff value)
	    (when (or (null distance)
		      (< (distance (+ j r0) (+ c0 i) row column) distance))
	      (percent-of-time density
		(let ((cell (clone object)))
		  [replace-cells-at self i j cell]
		  [set-location cell i j])))))))))

(define-method drop-ruin forest (row column height width)
  ;; adjust to prevent blocking exit and blocking player at start
  (setf row (max 3 (min (- row (* height 2)))))
  (setf column (max 3 (min (- column (* width 2)))))
  (let (rectangle openings)
    (labels ((collect-point (&rest args)
	       (prog1 nil (push args rectangle)))
	     (drop-wall (r c)
	       (unless (and (= r 0)
			    (= c 0))
		 (let ((wall (clone =wall=)))
		   [replace-cells-at self r c wall]
		   [set-location wall r c]))))
      (trace-rectangle #'collect-point row column height width)
      ;; make sure there are openings
      (dotimes (i 6)
	(let* ((n (random (length rectangle)))
	       (point (nth n rectangle)))
	  (destructuring-bind (r c) point
	    ;; don't make gate holes on corners or above exit
	    (unless (or (and (= r row) (= c (+ -1 column (truncate (/ width 2)))))
			(and (= r row) (= c (+ column (truncate (/ width 2)))))
			(and (= r row) (= c (+ 1 column (truncate (/ width 2)))))
			(and (= r row) (= c column)) ;; top left 
			(and (= r row) (= c (+ -1 column width))) ;; top right
			(and (= r (+ -1 row height)) (= c column)) ;; bottom left
			(and (= r (+ -1 row height)) (= c (+ -1 column width)))) ;; bottom right
	      (push (nth n rectangle) openings)
	      (setf rectangle (delete (nth n rectangle) rectangle))))))
      ;; draw walls
      (dolist (point rectangle)
	(destructuring-bind (r c) point
	  (drop-wall r c)))
      ;; draw gates
      (dolist (point openings)
	(destructuring-bind (r c) point
	  (let ((debris (clone =debris=)))
	    [replace-cells-at self r c debris]
	    [set-location debris r c])))
      ;; drop floor, obliterating what's below
      (labels ((drop-floor (r c)
		 (prog1 nil
		   (percent-of-time 80
		     [replace-cells-at self r c (clone =ruin-floor=)]))))
	(trace-rectangle #'drop-floor (1+ row) (1+ column) (- height 2) (- width 2) :fill))
      (dotimes (n (random 3))
	(percent-of-time 70
	  [drop-cell self (clone =body=) (+ 2 row (random (- height 2))) (+ 2 column (random (- width 2)))
		     :exclusive t :probe t])))))

(define-method generate forest (&key (height *forest-height*)
				     (width *forest-width*)
				     sequence-number
				     description
				     (fireflies 100)
				     (graveyards 15)
				     (ruins 15)
				     (herbs 2)
				     (firewood 14)
				     (terrain-type :earth)
				     level snowing raining
				     (tree-grain 0.3)
				     (tree-density 30)
				     (archer-skeletons 0)
				     (water-grain 0.9)
				     (water-density 90)
				     (water-cutoff 0.2))
  (setf <height> height)
  (setf <width> width)
  (when description (setf <description> description))
  (setf <sequence-number> sequence-number)
  (setf <snowing> snowing <raining> raining)
  (setf <level> level)
  [create-default-grid self]
  (ecase terrain-type 
    (:earth [drop-earth self])
    (:tundra [drop-tundra self]))
  [drop-cell self (clone =storm=) 0 0]
  (dotimes (i fireflies)
    (let ((firefly (clone =firefly=)))
      [add-sprite self firefly]
      [update-position firefly 
		       (random (* 16 *forest-width*))
		       (random (* 16 *forest-height*))]))
  [drop-trees self :graininess tree-grain :density tree-density]
  [drop-water self :graininess water-grain :density water-density :cutoff water-cutoff]
  (dotimes (n graveyards)
    [drop-graves self (+ 20 (random (- *forest-height* 20))) (random *forest-width*)
		 (+ 4 (random 3)) (+ 4 (random 2))])
  (dotimes (n ruins)
    [drop-ruin self (random *forest-height*) (random *forest-width*) (+ 9 (random 8)) (+ 4 (random 8))])
  (let ((row (1+ (random 5)) )
	(column (1+ (random 20))))
    [drop-cell self (clone =drop-point=) row column
	       :exclusive t :probe t]
    (dotimes (n herbs)
      (multiple-value-bind (r c) [random-place self]
	[drop-cell self (clone =herb=) r c :exclusive t :probe t])))
  (dotimes (n firewood)
    (multiple-value-bind (r c) [random-place self]
      [drop-cell self (clone =firewood=) r c :exclusive t :probe t]))
  (dotimes (n archer-skeletons)
    (multiple-value-bind (r c) [random-place self]
      [drop-cell self (clone =archer-skeleton=) r c :exclusive t :probe t :loadout t]))
  (let* ((gateway (clone (ecase level
			   (1 =river-gateway=)
			   (2 =ascent-gateway=)
			   (3 =passage-gateway=))))
	 (row (+ (- height 10) (random 10))) ;; 20 FIXME
	 (column (+ 2 (random (- *forest-width* 4)))))
    (setf <gateway-row> row <gateway-column> column)
    [replace-cells-at *world* row column gateway]
    [set-location gateway row column])
  ;; drop Lich if needed
  (let ((lich (clone =lich=)))
    (when (= level 3)
      [drop-cell self lich (+ 60 (random 20)) (random width) :loadout t])))
    
(define-method gateway-coordinates forest ()
  (values <gateway-row> <gateway-column>))
    
(define-method begin-ambient-loop forest ()
  (play-sample "lutey")
  (play-music "nightbird" :loop t))

