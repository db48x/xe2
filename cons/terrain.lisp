(in-package :cons-game)

;;; Sector exit

(define-prototype exit (:parent xe2:=launchpad=)
  (tile :initform "launchpad")
  (categories :initform '(:gateway :player-entry-point :action)))

(define-method do-action exit ()
  [exit *universe* :player [get-player *world*]])

;;; Systematic variations 

;; TODO remove this section. 

(defparameter *security-theme* '(:floor "security-background"
				 :barrier "security-foreground"
				 :accent "security-accent"
				 :music "foo" :loop t))

(defparameter *archive-theme* '(:floor "archive-background"
				 :barrier "archive-foreground"
				 :accent "archive-accent"
				 :music "mello" :loop t))

(defparameter *storage-theme* '(:floor "storage-background"
				 :barrier "storage-foreground"
				 :accent "storage-accent"
				 :music "neo-eof" :loop t))

(defparameter *reactor-theme* '(:floor "reactor-background"
				 :barrier "reactor-foreground"
				 :accent "reactor-accent"
				 :music "beatup" :loop t))

(defparameter *corridor-theme* '(:floor "corridor-background"
				 :barrier "corridor-foreground"
				 :accent "corridor-accent"
				 :music "beatup" :loop t))

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
	   '((world >> (=launchpad= :color :drop)))))

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
  (tile :initform "unknown-gateway")
  (categories :initform '(:gateway :actor))
  (world :initform nil))

(define-method initialize sector-gateway (address)
  (setf <address> address)
  [update-tile self])

;; (define-method activate sector-gateway ()
;;   [ex
;; ;;  [play *universe* :address <address> :player [get-player *world*]])

(define-method run sector-gateway ()
  [update-tile self])

(define-method update-tile sector-gateway ()
  (setf <tile> (getf *sector-tiles* (car <address>))))

;;; Alien base consists of a grid of sectors

(define-prototype alien-base (:parent xe2:=world=)
  (overworld :initform t)
  (height :initform 5)
  (width :initform 5)
  (tile-size :initform 32)
  (scale :initform '(1 xm))
  (edge-condition :initform :block)
  (ambient-light :initform :total))

(define-method generate alien-base (&rest params)
  (clon:with-field-values (height width) self
    [create-default-grid self]
    (dotimes (row height)
      (dotimes (column width)
	[drop-cell self (clone =sector-gateway= (list (car (one-of *sector-names*))
						      :sequence-number (genseq))) row column]))))

;;; Corridor with opening eyes

(defcell road
  (description :initform "Security vehicle transit area.")
  (tile :initform "darkcyanworld"))

(define-prototype corridor (:parent xe2:=world=)
  gen-row gen-column 
  ;;
  (description :initform "You enter a long corridor.")
  (level :initform 1)
  ;;
  (ambient-light :initform :total)
  (required-modes :initform nil)
  (scale :initform '(3 m))
  (edge-condition :initform :block))

(define-method generate corridor (&key (height 100)
					    (width 50)
					    (sequence-number (genseq)))
  (setf *notes* nil)
  (setf <height> height <width> width)
  [create-default-grid self]
  (labels ((drop-barrier (r c)
	     (prog1 nil
	       [drop-cell self (clone =barrier=) r c])))
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =road=)
		 i j]))
    (dotimes (i 20)
      [drop-cell self (clone =block=) (random height) (random width)])
    (dotimes (i 20)
      [drop-cell self (clone =bomb=) (random height) (random width)])
    (dotimes (i 25)
      (let ((draw-function (if (= 0 (random 3))
			       #'trace-row #'trace-column)))
	(funcall draw-function #'drop-barrier
		 (+ 10 (random 50))
		 (+ 10 (random 50))
		 (+ 10 (random 50)))))
    [drop-cell self (clone =launchpad=) 10 10]))

(define-method begin-ambient-loop corridor ()
  (play-music "neo-eof" :loop t))

