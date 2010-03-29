(in-package :cons-game)

;;; Sector exit

(define-prototype exit (:parent xe2:=launchpad=)
  (tile :initform "launchpad")
  (categories :initform '(:gateway :player-entry-point :action))
  (description :initform "Exit the area by activating this object with the Z key."))

(define-method do-action exit ()
  [exit *universe* :player [get-player *world*]])

;;; Indestructible wall of many colors

(defcell floor 
  (categories :initform '(:floor)))
  
(define-method initialize floor (&optional floor)
  (setf <tile> (or floor (field-value :floor *world*))))

(defcell barrier 
  (auto-loadout :initform t)
  (categories :initform '(:obstacle :barrier :target)))

(define-method initialize barrier (&optional barrier)
  (setf <tile> (or barrier (field-value :barrier *world*))))

(define-method loadout barrier ()
  (setf <tile> (or <tile> (field-value :barrier *world*))))

;;; Generic sector of alien base; this is specialized below.

(define-prototype sector (:parent xe2:=world=)
  ;; theme variables
  floor barrier accent
  ;; other
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (scale :initform '(1 xm))
  (edge-condition :initform :block)
  (grammar :initform 
	   '((world >> (=exit= :color :drop)))))

(define-method drop-floor sector (r c)
  [drop-cell self (clone =floor= <floor>) r c])

(define-method drop-barrier sector (r c)
  [drop-cell self (clone =barrier= <barrier>) r c])

(define-method drop-border sector ()
  (labels ((drop (r c)
	     (prog1 nil 
	       [drop-cell self (clone =barrier= <barrier>) r c])))
    (trace-rectangle #'drop 0 0 <height> <width>)))

(define-method generate sector (&rest params)
  [create-default-grid self]
  (dotimes (row <height>)
    (dotimes (column <width>)
      [drop-floor self row column]))
  [drop-border self]
  [parent>>generate self])

;;; Sector gateway can point to any kind of sector

(defparameter *sector-names* '(=security= =archive= =storage= =reactor= =corridor=))

(defparameter *sector-tiles* '(=security= "security-gateway"
			       =archive= "archive-gateway"
			       =storage= "storage-gateway"
			       =reactor= "reactor-gateway"
			       =corridor= "corridor-gateway"))

(define-prototype sector-gateway (:parent xe2:=gateway=)
  (name :initform "Sector gateway")
  (tile :initform "unknown-gateway")
  (categories :initform '(:gateway :actor))
  (world :initformp nil))

(define-method initialize sector-gateway (address)
  (setf <address> address)
  (let ((world (symbol-value (car <address>))))
    (assert (clon:object-p world))
    (clon:with-field-values (description name) world
      (setf <description> description <name> name))
    [update-tile self]))

(define-method run sector-gateway ()
  [update-tile self])

(define-method step sector-gateway (stepper)
  [describe self])

(define-method update-tile sector-gateway ()
  (setf <tile> (getf *sector-tiles* (car <address>))))

;;; Alien base consists of a grid of sectors

(define-prototype alien-base (:parent xe2:=world=)
  (name :initform (format nil "Xiobase #~S~S" (random 9) (+ 32768 (random 32768))))
  (description :initform 
"Welcome to CONS. Your mission is to infiltrate, explore, and
ultimately destroy a high-security enemy starbase of unknown purpose.
The colored squares above represent the sectors of the base you can
currently visit. Use the movement keys (see sidebar at right) to
select a sector; press Z to enter. Press F1 for help.")
  (overworld :initform t)
  (height :initform 5)
  (width :initform 5)
  (tile-size :initform 32)
  (scale :initform '(1 xm))
  (edge-condition :initform :block)
  (ambient-light :initform :total))

(define-method generate alien-base (&key sequence-number )
  (clon:with-field-values (height width) self
    [create-default-grid self]
    (dotimes (row height)
      (dotimes (column width)
	[drop-cell self (clone =sector-gateway= (list (car (one-of *sector-names*))
						      :sequence-number (genseq))) row column]))))

(define-method after-start-method alien-base ()
  [describe self])

(define-method begin-ambient-loop alien-base ()
  (play-music "mello"))

