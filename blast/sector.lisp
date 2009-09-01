(in-package :blast)

;;; Stars

(defcell star
  (tile :initform "star")
  (name :initform "Naked star")
  (description :initform "A lonely star."))

(defcell void
  (tile :initform "void")
  (name :initform "Interstellar space")
  (description :initform "The infinite cosmos beckons."))

(define-method step void (stepper)
  (when (has-field :endurium stepper)
    [stat-effect stepper :endurium -1]
    [>>say :narrator (format nil "Burned 1 kilo of endurium moving to ~D:~D"
			     (field-value :row stepper)
			     (field-value :column stepper))]))

(define-prototype starfield (:parent =void=)
  (tile :initform "starfield")
  (name :initform "Interstellar space")
  (description :initform "The depths of space yawn before you."))

;;; Zeta base

(define-prototype zeta-base-gateway (:parent =gateway=)
  (tile :initform "zeta-base")
  (name :initform "Zeta Base ruins")
  (address :initform '(=zeta-base= 
		       :width 50
		       :height 200
		       :asteroid-count 0
		       :biclops-count 10
		       :berserker-count 10
		       :polaris-count 0
		       :probe-count 50
		       :energy-gas-cluster-count 8
		       :room-size 8
		       :box-cluster-count 40
		       :room-count 65
		       :rook-count 0
		       :scanner-count 25
		       :energy-count 40))
  (description :initform 
"Zeta Base was one of our clandestine research stations before the
droids took control."))

(define-method step zeta-base-gateway (stepper)
  [>>narrateln :narrator "This is the old Zeta Base. Press RETURN to
  enter."])

;;; The mysterious Nebulas

(define-prototype nebula-m-gateway (:parent =gateway=)
  (tile :initform "nebula-m-gateway")
  (sequence-number :initform (genseq))
  (name :initform "Nebula, MX type")
  (address :initform (list '=nebula-m= :sequence-number (genseq)))
  (description :initform "You can mine asteroids for endurium fuel here."))

(define-method step nebula-m-gateway (stepper)
  [say self "The mysterious Nebula MX-~A. Press RETURN to enter." <sequence-number>])

;;; A mars-like planet with fractal terrain.

(define-prototype mars-gateway (:parent =gateway=)
  (tile :initform "mars-gateway")
  (name :initform "Mars-like planet")
  (address :initform '(=mars= :technetium 8)))

(define-method step mars-gateway (stepper)
  [>>narrateln :narrator "A nameless Mars-like planet. Press RETURN to enter."])

;;; Infested derelict freighters. 

(defvar *freighter-sequence-number* 0)

(define-prototype freighter-gateway (:parent =gateway=)
  (tile :initform "freighter-gateway")
  (name :initform "Infested derelict freighter")
  (address :initform (list '=freighter= 
			   ;; ensure all freighters are distinct
			   :rooms (+ 5 (random 10))
			   :stations (+ 3 (random 10))
			   :sequence-number 
			   (incf *freighter-sequence-number*)))
  (description :initform 
"A freighter ship penetrated by droids. The crew will have been
murdered, but valuable resources may be left inside."))

(define-method step freighter-gateway (stepper)
  [>>narrateln :narrator "An infested derelict freighter. Press RETURN to enter."])

;;; bio-hives

(defvar *hive-sequence-number* 0)

(define-prototype hive-gateway (:parent =gateway=)
  (tile :initform "hive-gateway")
  (name :initform "Biosilicate hive biome")
  (address :initform (list '=biome= 
			   :clusters (+ 5 (random 15))
			   ;; ensure all hives are distinct
			   :sequence-number 
			   (incf *hive-sequence-number*))))

(define-method step hive-gateway (stepper)
  [>>narrateln :narrator "A free-floating biosilicate hive sac. Press RETURN to enter."])

;;; caves

(defvar *cave-sequence-number* 0)

(define-prototype cavern-gateway (:parent =gateway=)
  (tile :initform "cave-gateway")
  (name :initform "Asteroid cave")
  (address :initform (list '=cavern=
			   ;; ensure all caves are distinct
			   :sequence-number 
			   (incf *cave-sequence-number*))))

(define-method step cavern-gateway (stepper)
  [say self "A free-floating cave. Press RETURN to enter."])

;;; star corridors

(defvar *vomac-sequence-number* 0)

(define-prototype vomac-gateway (:parent =gateway=)
  (tile :initform "vomac-gateway")
  (name :initform "Vomac combat corridor")
  (address :initform (list '=star-corridor=
			   ;; ensure all vomacs are distinct
			   :sequence-number 
			   (incf *vomac-sequence-number*))))

(define-method step vomac-gateway (stepper)
  [say self "A vomac star corridor. RETURN to enter."])

(defcell antares 
  (name :initform "Antares")
  (description :initform "The star Antares.")
  (tile :initform "antares"))

;;; The first mission: the ocean planet bombing run! 

(define-prototype ocean-gateway (:parent =gateway=)
  (tile :initform "ocean-gateway")
  (name :initform "The ocean planet Corva 3")
  (address :initform (list '=bay= 
			   :drones (+ 30 (random 40))
			   :carriers (+ 6 (random 10))
			   )))

(define-method step ocean-gateway (stepper)
  [>>narrateln :narrator "The water planet Corva 3. Press ENTER to land."])

;;; Ancient cube

(define-prototype yellow-cube (:parent =gateway=)
  (tile :initform "yellow-cube")
  (name :initform "Yellow cube")
  (description :initform 
"A strange, featureless yellow cube about a kilometer wide.")
  (address :initform (list '=cube= :sequence-number (genseq))))

;;; The local cluster

(defparameter *chang-message*
"Greetings, contractor. I'm Daytona Chang, your liason with Rogue
Nexus. While in our employ you will receive assignments from and
report to me. 

You've arrived in the Antares sector. Only short-range scans are
possible, so you'll have to map the area slowly. You'll need fuel to
reach Zeta Base, so we suggest heading to one of the nearby
nebulae. You can mine asteroids there to collect Endurium. Eliminate
any resistance you encounter.

There may be ships available at Zeta---if the droids haven't destroyed
them all. Don't take chances; protect the ship we've given you at all
costs, as there is no guarantee you'll find any replacement. 

You must penetrate the base and fight your way to the southernmost
wing, where the data we are seeking was stored. See if you can find
the computer and reactivate it.

You will be substantially rewarded for data, materials, and artifacts
collected during your mission---but do not allow these rewards to
compete with mission goals.

Your onboard computer is linked to our knowledge base---click any
object to see what information is available. You may also press F1 for
general help at any time.

Good luck, contractor. 

 -- CHANG


")

(define-prototype star-sector (:parent rlx:=world=)
  (ambient-light :initform 4)
  (automapped :initform t)
  (required-modes :initform '(:vehicle :spacesuit))
  (categories :initform '(:weightless :airless))
  (scale :initform '(1 ly))
  (edge-condition :initform :block)
  (described-p :initform nil)
  (description :initform *chang-message*))

(define-method describe star-sector ()
  (setf <description> (if (null <described-p>)
			  (prog1 *chang-message* 
			    (setf <described-p> t))
			  "The unexplored Antares sector."))
  (when <narrator>
    [>>newline :narrator]
    (dolist (line (rlx:split-string-on-lines <description>))
      [>>narrateln :narrator line])))

(define-method begin-ambient-loop star-sector ()
  (play-music "xiomacs" :loop t))
  
(define-method generate star-sector (&key (height 80)
					  (width 80)
					  sequence-number
					  (freighters 5)
					  (hives 4)
					  (caves 5)
					  (stars 80))
  (setf <height> height <width> width)
  [create-default-grid self]
  (dotimes (i width)
    (dotimes (j height)
      [drop-cell self (clone (if (zerop (random 7))
				 =starfield= 
				 =void=))
		 i j]))
  (dotimes (i stars)
    [drop-cell self (clone =star=) (random height) (random width) :exclusive t :probe t])
  [drop-cell self (clone =antares=) (random 5) (random 5)  :exclusive t :probe t]
  (dotimes (i freighters)
    [drop-cell self (clone =freighter-gateway=) (+ 10 (random 30)) (+ 8 (random 30)) :exclusive t :probe t])
  (dotimes (i hives)
    [drop-cell self (clone =hive-gateway=) (random 30) (random 30) :exclusive t :probe t])
  (dotimes (i caves)
    [drop-cell self (clone =cavern-gateway=) (+ 20 (random 40)) (+ 25 (random 30)) :exclusive t :probe t])
  (dotimes (i 5)
    [drop-cell self (clone =vomac-gateway=) (+ 20 (random 30)) (+ 20 (random 30)) :exclusive t :probe t])
  (dotimes (i 2)
    [drop-cell self (clone =yellow-cube=) (+ 35 (random 10)) (+ 35 (random 10)) :exclusive t :probe t])
  [drop-cell self (clone =zeta-base-gateway=) (+ 10 (random 5)) (+ 10 (random 5)) :exclusive t :probe t]
  [drop-cell self (clone =mars-gateway=) (random 13) (random 6) :exclusive t :probe t]
  [drop-cell self (clone =ocean-gateway=) (+ 20 (random 10)) (+ 20 (random 20)) :exclusive t :probe t]
  [drop-cell self (clone =nebula-m-gateway=) (random 10) (random 10) :exclusive t :probe t]
  [drop-cell self (clone =nebula-m-gateway=) (+ 15 (random 10)) (+ 15 (random 10)) :exclusive t :probe t])
