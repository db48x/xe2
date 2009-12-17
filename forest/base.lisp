(in-package :forest)

;;; Status widget

(defvar *status* nil)

;;; Turn on timing after SDL init

(add-hook 'xe2:*initialization-hook*
	  #'(lambda ()
	      (xe2:enable-timer)
	      (xe2:set-frame-rate 15)
	      (xe2:set-timer-interval 0)
	      (xe2:enable-held-keys 1 3)))

;;; The World addresses of the levels in the game.

(defun generate-level-address (n)
  (ecase n 
    (1 (list '=forest= 
	     :description 
"You are outside of Nothbess town, heading south toward the
Monastery. It is cold and rainy."
	     :level n
	     :sequence-number (xe2:genseq) 
	     :height *forest-height*
	     :width *forest-width*
	     :fireflies 100
	     :graveyards 4
	     :ruins 3
	     :firewood 10
	     :raining t
	     :tree-grain 0.5
	     :tree-density 30
	     :water-grain 0.2
	     :water-density 0
	     :water-cutoff 0.2))
    (2 (list '=forest= 
	     :level n
	     :description 
"The river has swelled beyond its banks with meltwaters, and flooded
an old hamlet whose name is forgotten. 
It has begun to snow."
	     :sequence-number (xe2:genseq) 
	     :height *forest-height*
	     :width *forest-width*
	     :fireflies 100
	     :graveyards 6
	     :ruins 10
	     :snowing t
	     :firewood 25
	     :archer-skeletons 6
	     :tree-grain 0.2
	     :tree-density 30
	     :water-grain 0.5
	     :water-density 80
	     :water-cutoff 0.3))
    (3 (list '=forest= 
	     :level n
	     :description 
	     "The trees thin here as you ascend toward the mountain pass."
	     :sequence-number (xe2:genseq) 
	     :height *forest-height*
	     :width *forest-width*
	     :fireflies 20
	     :graveyards 0
	     :ruins 0
	     :terrain-type :tundra
	     :snowing t
	     :firewood 25
	     :archer-skeletons 8
	     :tree-grain 0.9
	     :tree-density 20
	     :water-grain 0.5
	     :water-density 0
	     :water-cutoff 0.3))
    (4 (list '=forest= 
	     :level n
	     :description 
	     "A grassy clearing is between you and the mountain pass."
	     :sequence-number (xe2:genseq) 
	     :height *forest-height*
	     :width *forest-width*
	     :fireflies 80
	     :graveyards 0
	     :herbs 0
	     :ruins 0
	     :dolmens 3
	     :terrain-type :grass
	     :raining t
	     :firewood 2
	     :archer-skeletons 3
	     :tree-grain 0.9
	     :tree-density 1
	     :water-density 0))
    (5 (list '=passage=))
    (6 (list '=monastery=))
    (7 (list '=quarters=))))
    
;;; Text overlay balloons

(defcell balloon 
  (categories :initform '(:drawn :actor :balloon))
  text timeout following
  (stroke-color :initform ".white")
  (background-color :initform ".gray40"))

(define-method initialize balloon (&key text (stroke-color ".white") (background-color ".blue")
					(style :balloon) (timeout nil) following)
  (setf <text> text)
  (setf <stroke-color> stroke-color)
  (setf <following> following)
  (setf <background-color> background-color)
  (setf <style> style)
  (setf <timeout> (if (floatp timeout)
		      ;; specify in (roughly) seconds if floating
		      (truncate (* 15 timeout))
		      ;; leave as frames if integer
		      timeout)))
  
(define-method follow balloon (cell)
  (setf <following> cell))

(define-method draw balloon (x y image)
  (clon:with-field-values (text style) self
    (let* ((offset (ecase style
		     (:balloon 16)
		     (:flat 0)))
	   (x0 (+ x offset))
	   (y0 (+ y offset))
	   (x1 nil)
	   (y1 nil)
	   (margin 4)
	   (height (+ (* 2 margin) (apply #'+ (mapcar #'formatted-line-height text))))
	   (width (+ (* 2 margin) (apply #'max (mapcar #'formatted-line-width text)))))
      (setf x0 (min x0 (- *room-window-width* width)))
      (setf y0 (min y0 (- *room-window-height* height)))
      (setf x1 (+ x0 offset))
      (setf y1 (+ y0 offset))
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
  (when <following>
    (multiple-value-bind (r c) [grid-coordinates <following>]
      ;; follow emoter
      [move-to self r c]))
  (when (integerp <timeout>)
    (when (minusp (decf <timeout>))
      [die self])))

