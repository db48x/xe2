(in-package :forest)

;;; Fire

(defparameter *fire-tiles* '("fire-1" "fire-2" "fire-3"))

(defcell fire 
  (tile :initform "fire-1")
  (clock :initform 300)
  (categories :initform '(:actor)))

(define-method step fire (stepper)
  (when [is-player stepper]
    (when (plusp [stat-value stepper :freezing])
      [say self "You dry yourself by the fire."]
      [dry stepper])
    (when (zerop [stat-value stepper :freezing])
      [say self "You are dry now."])))

(define-method run fire ()
  (decf <clock>)
  (setf <tile> (car (one-of *fire-tiles*)))
  (unless (plusp <clock>)
    [die self]))

;;; Supplies

(defcell arrows 
  (tile :initform "arrows")
  (count :initform nil))

(define-method initialize arrows (&key (count (+ 5 (random 12))))
  (setf <count> count))
    
(define-method step arrows (stepper)
  (when [is-player stepper]
    [say self "You found ~S arrows." <count>]
    [stat-effect stepper :arrows <count>]
    [die self]))
     
(defcell herb 
  (tile :initform "herb")
  (description :initform "This healing herb will restore some of your health.")
  (categories :initform '(:item :exclusive))
  (equip-for :initform '(:right-hand :left-hand)))

(define-method step herb (stepper)
  (when [is-player stepper]
    (if [take stepper :direction :here :category :item]
      [say self "You found a healing herb."]
      [say self "Your satchel is full."])))

(define-method use herb (user)
 (when (and user (has-field :hit-points user))
    (prog1 t
      [stat-effect user :hit-points 12]
      [say self "You consume the healing herb and quickly feel better."])))

(defcell firewood 
  (tile :initform (car (one-of '("firewood-1" "firewood-2"))))
  (categories :initform '(:exclusive))
  (description :initform "Five pieces of firewood are enough to make a campfire."))

(define-method step firewood (stepper)
  (when [is-player stepper]
    [say self "You collect the firewood."]
    [stat-effect stepper :firewood 1]
    [delete-from-world self]))

;;; A map of the journey; also acts as a compass

(defcell sanctuary-map 
  (name :initform "Map")
  (tile :initform "tiny-map")
  (description :initform "This map was given to you by Wythorn before you left town.")
  (categories :initform '(:item)))

(define-method use sanctuary-map (user)
  (let ((map-text '(((nil :image "sanctuary-map"))))
	(compass-text nil))
    (prog1 nil
      (when (clon:has-method :gateway-coordinates *world*)
	(setf compass-text 
	      (multiple-value-bind (gateway-row gateway-column) 
		  [gateway-coordinates *world*]
		(let* ((r2 (field-value :row user))
		       (c2 (field-value :column user))
		       (dir (direction-to r2 c2 gateway-row gateway-column)))
		  (list (list (list (format nil "Your destination is to the ~A."
					    (string-capitalize (symbol-name dir))))))))))
      [emote user (append compass-text map-text)])))
	 
(define-method step sanctuary-map (stepper)
  (when [is-player stepper]
    [say self "You don't need this map; you've memorized the way."]))

;;; The player
	     
(defparameter *arrow-tiles* '(:north "arrow-north"
			      :south "arrow-south"
			      :east "arrow-east"
			      :west "arrow-west"
			      :northwest "arrow-northwest"
			      :northeast "arrow-northeast"
			      :southwest "arrow-southwest"
			      :southeast "arrow-southeast"))

(defcell arrow 
  (name :initform "arrow")
  (speed :initform (make-stat :base 25))
  (categories :initform '(:actor))
  (clock :initform 8)
  (direction :initform nil))

(define-method impel arrow (direction)
  (setf <direction> direction))

(define-method run arrow ()
  [expend-default-action-points self]
  (if (and <direction> (not (eq <direction> :here)))
      (progn (setf <tile> (getf *arrow-tiles* <direction>))
	     (let ((target [category-in-direction-p *world* <row> <column> <direction> :target]))
	       (when target 
		 [damage target 3]
		 [play-sample self "knock"]
		 [die self])
	       (if [obstacle-in-direction-p *world* <row> <column> <direction>]
		   [die self]
		   [move self <direction>]))
	     (setf <clock> (max 0 (decf <clock>)))
	     (when (zerop <clock>)
	       [die self])
	     (when [obstacle-in-direction-p *world* <row> <column> <direction>]
	       (setf <clock> 0)))
      [die self]))

(define-method step arrow (stepper)
  (when [is-player stepper]
    [say stepper "This arrow is still good. You add it to your quiver."]
    [stat-effect stepper :arrows 1]
    [delete-from-world self]))

(defcell wooden-bow 
  (name :initform "Wooden bow")
  (categories :initform '(:item :weapon :equipment))
  (tile :initform "wooden-bow")
  (attack-power :initform (make-stat :base 5))
  (attack-cost :initform (make-stat :base 6))
  (accuracy :initform (make-stat :base 90))
  (weight :initform 3000)
  (equip-for :initform '(:left-hand)))

(define-method fire wooden-bow (direction)
  (if (plusp [stat-value <equipper> :arrows])
      (let ((arrow (clone =arrow=)))
	[stat-effect <equipper> :arrows -1]
	[drop <equipper> arrow]
	[impel arrow direction]
	[play-sample <equipper> "bow"])
      [say self "You are out of arrows!" :foreground ".red"]))

(defparameter *hunger-warn* 700)

(defparameter *hunger-warn-2* 850)

(defparameter *hunger-max* 1000)

(defparameter *hunger-damage-clock* 20)

(defparameter *freezing-warn* 80)

(defparameter *freezing-warn-2* 160)

(defparameter *freezing-max* 300)

(defparameter *freezing-damage-clock* 12)

(defparameter *bow-reload-clock* 10)

(defcell player 
  (tile :initform "player")
  (description :initform "You are an archer and initiate monk of the Sanctuary Order.")
  (name :initform "Monk")
  (dead :initform nil)
  (hit-points :initform (make-stat :base 30 :min 0 :max 30))
  (hunger :initform (make-stat :base 0 :min 0 :max 1000))
  (hunger-damage-clock :initform 0)
  (freezing :initform (make-stat :base 0 :min 0 :max 300))
  (freezing-damage-clock :initform 0)
  (bow-reload-clock :initform 0)
  (hearing-range :initform 1000)
  (firing-with :initform :left-hand)
  (arrows :initform (make-stat :base 20 :min 0 :max 40))
  (firewood :initform (make-stat :base 5 :min 0 :max 10))
  (rations :initform (make-stat :base 5 :min 0 :max 20))
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (strength :initform (make-stat :base 15 :min 0 :max 50))
  (defense :initform (make-stat :base 15 :min 0 :max 50))
  (dexterity :initform (make-stat :base 15 :min 0 :max 30))
  (intelligence :initform (make-stat :base 13 :min 0 :max 30))
  (equipment-slots :initform '(:right-hand :left-hand))
  (max-items :initform (make-stat :base 20))
  (movement-cost :initform (make-stat :base 10))
  (stepping :initform t)
  (categories :initform '(:actor :player :obstacle :target)))

(define-method fire player (direction)
  (if (zerop <bow-reload-clock>)
      (progn
	(setf <bow-reload-clock> *bow-reload-clock*)
	[add-category self :reloading]
	[parent>>fire self direction])
      (progn 
	[say self "Still reloading; cannot fire."])))

(define-method eat player ()
  (if (zerop [stat-value self :rations])
      [say self "You don't have any rations to eat."]
      (progn 
	[say self "You eat a bread ration. You feel full."]
	[stat-effect self :hunger -900]
	[stat-effect self :rations -1])))

(define-method move player (direction)
  (unless <dead>
    [parent>>move self direction]))

(define-method use-item player (n)
  (assert (integerp n))
  (let ((object [item-at self n]))
    (if object
	(if [in-category object :equipment]
	    [equip self n]
	    (when [use object self]
	      [remove-item self object]))
	[say self "There is nothing to use there."])))
 
(define-method enter player ()
  (let ((gateway [category-at-p *world* <row> <column> :gateway]))
    (if (null gateway)
	[>>say :narrator "No gateway to enter."]
	[activate gateway])))

(define-method emote player (text &key (timeout 20) (background-color ".blue"))
  (let ((balloon (clone =balloon= :text text :timeout timeout :background-color background-color))
	(other-balloon [category-at-p *world* <row> <column> :balloon]))
    (when other-balloon
      [die other-balloon])
    [play-sample self "talk"]
    [follow balloon self]
    [drop self balloon]))

(define-method quit player ()
  (xe2:quit :shutdown))

(define-method run player ()
  (unless <dead>
    (message "FREEZING: ~A" [stat-value self :freezing])
    [stat-effect self :hunger 1]
    (let ((hunger [stat-value self :hunger])
	  (freezing [stat-value self :freezing]))
      (when (= *hunger-warn* hunger)
	[say self "You are getting hungry. Press Control-E to eat a ration."])
      (when (= *hunger-warn-2* hunger)
	[emote self '((("I'm very hungry.")))]
	[say self "You are getting extremely hungry! Press Control-E to eat a ration."])
      (when (= *hunger-max* hunger)
	(if (minusp <hunger-damage-clock>)
	    (progn 
	      [say self "You are starving! You will die if you do not eat soon."]
	      [say self "Press Control-E to eat a ration."]
	      (setf <hunger-damage-clock> *hunger-damage-clock*)
	      [damage self 1])
	    (decf <hunger-damage-clock>)))
      (if (> hunger *hunger-warn*)
	  [add-category self :hungry]
	  [delete-category self :hungry])
      (if (= *hunger-max* hunger)
	  [add-category self :starving]
	  [delete-category self :starving])
      (when (= *freezing-warn* freezing)
	[say self "You are beginning to get soaked."])
      (when (= *freezing-warn-2* freezing)
	[say self "You are getting soaked! You will begin to freeze soon."])
      (when (= *freezing-max* freezing)
	(if (minusp <freezing-damage-clock>)
	    (progn 
	      [say self "You are freezing! You will die if you do not dry out soon."]
	      [say self "Press Control-C to make a campfire."]
	      (setf <freezing-damage-clock> *freezing-damage-clock*)
	      [damage self 1])
	    (decf <freezing-damage-clock>)))
      (if (= *freezing-max* freezing)
	  [add-category self :freezing]
	  [delete-category self :freezing])
      (if (< [stat-value self :hit-points] 10)
	  (progn [>>narrateln :narrator "LOW HEALTH WARNING! You will die soon if you do not heal." :foreground ".red"]
		 [add-category self :dying])
	  [delete-category self :dying])
      (when (zerop [stat-value self :hit-points])
	[die self])
      (setf <bow-reload-clock> (max 0 (- <bow-reload-clock> 1)))
      (when (zerop <bow-reload-clock>) [delete-category self :reloading])
      (when (and *status* <inventory>) [update *status*]))))

(define-method restart player ()
  (when <dead>
    (let ((player (clone =player=)))
      [destroy *universe*]
      [set-player *universe* player]
      [set-character *status* player]
      [play *universe*
	    :address (generate-level-address 1)]
      [loadout player])))

(define-method damage player (points)
  (unless <dead>
    [say self "You take ~A hit ~A of damage."
	 points (if (= 1 points) "point" "points")]
    [play-sample self (car (one-of '("unh-1" "unh-2" "unh-3")))]
    [stat-effect self :hit-points (- points)]))

(define-method die player ()
  (unless <dead>
    (setf <tile> "skull")
    [play-sample self "death"]
    [say self "You died. Press ESCAPE to try again."]
    (setf <dead> t)))

(define-method loadout player ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =wooden-bow=)]]
  [add-item self (clone =sanctuary-map=)]
  [emote self '((("I'd better get moving.")) (("The monastery is to the south.")))])

(define-method wet player ()
  [stat-effect self :freezing 20])

(define-method dry player ()
  [stat-effect self :freezing -300])

(define-method camp player ()
  (clon:with-field-values (row column) self
    (let (found)
      (if (>= [stat-value self :firewood] 5)
	  (progn (block placing
		   (dolist (dir '(:north :south :east :west))
		     (multiple-value-bind (r c) (step-in-direction row column dir)
		       (unless [category-at-p *world* r c '(:obstacle :water)]
			 (setf found t)
			 [drop-cell *world* (clone =fire=) r c]
			 [stat-effect self :firewood -5]
			 [stat-effect self :hit-points 10]
			 (return-from placing)))))
		 (if found
		     [say self "With the Spark incantation, the fire is soon burning."]
		     [say self "Couldn't find a place to build a fire."]))
	  [say self "You don't have enough firewood."]))))
	
	
