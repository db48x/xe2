;;; viewport.lisp --- tile engine display widget

;; Copyright (C) 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A world may have one or more viewport widgets. In a standard
;; viewport, cells are represented onscreen by uniformly-sized
;; graphical tiles; worlds are viewed from a bird's-eye
;; perspective. Other viewports may render schematic or compressed
;; views of the world (for example, an auto-map display.)

;;; Code:

(in-package :rlx)

(defstruct overlay func parameters clock)

(define-prototype viewport 
    (:parent =widget= :documentation "A map display for RLX worlds.")
  (world :documentation "The world object to be displayed.")
  (overlays :documentation "List of closures.")
  (margin :initform 6 :documentation "Scroll margin.")
  (origin-x :initform 0 
	    :documentation "The world x-coordinate of the tile at the viewport's origin.")
  (origin-y :initform 0 
	    :documentation "The world y-coordinate of the tile at the viewport's origin.")
  (origin-width :initform 10 :documentation "The width in tiles of the viewport.")
  (origin-height :initform 10 :documentation "The height in tiles of the viewport.")
  (tile-size :initform 16 :documentation "Size in pixels of a tile. They must be square."))

(define-method get-screen-coordinates viewport (cell-row cell-column)
  (let ((size <tile-size>))
    (let ((x0 (+ (/ size 2)
		 (* size
		    (- cell-column <origin-x>))))
	  (y0 (+ (/ size 2)
		 (* size
		    (- cell-row <origin-y>)))))
      (values x0 y0))))

(define-method add-overlay viewport (overlay)
  (pushnew overlay <overlays>))

(define-method set-world viewport (world)
  (setf <world> world))

(define-method set-tile-size viewport (size)
  (assert (integerp size))
  (setf <tile-size> size))
    
(define-method render viewport ()
;;  (declare (optimize (speed 3)))
  [adjust self] ;; hehe
  (let* ((world (or <world> *active-world*))
	 (origin-width <origin-width>)
	 (origin-height <origin-height>)
	 (origin-x <origin-x>)
	 (origin-y <origin-y>)
	 (image <image>)
	 (tile-size <tile-size>)
	 objects cell)
    (with-field-values (grid light-grid environment-grid phase-number
			     height width
			     turn-number ambient-light) world
      ;; blank the display
      [clear self]
      ;; draw the tiles
      (dotimes (i origin-height)
	(dotimes (j origin-width)
	  ;; is this square lit? 
	    (if (and (array-in-bounds-p grid (+ i origin-y) (+ j origin-x))
		     (or (eq :total ambient-light)
			 (= 1 (aref light-grid (+ i origin-y) (+ j origin-x)))))
		(progn 
		  (setf objects (aref grid 
				      (+ i origin-y)
				      (+ j origin-x)))
		  (dotimes (k (fill-pointer objects))
		    (setf cell (aref objects k))
		    (when (object-p cell)
		      (draw-resource-image (field-value :tile cell)
					   (* j tile-size) (* i tile-size)
					   :destination image))))
		;; not in bounds, or not lit; draw blackness
		(draw-resource-image ".blackness" (* j tile-size) (* i tile-size)
				     :destination image))))
      ;; update geometry
      (let ((width (* tile-size origin-width))
	    (height (* tile-size origin-height)))
	(unless (and (= width <width>)
		     (= height <height>))
	  [resize self :height height :width width]))
      ;; render overlays
      (dolist (overlay <overlays>)
	(funcall overlay image))
      (setf <overlays> nil))))

(define-method hit viewport (x y)
  (when [parent>>hit self x y]
    
    (let* ((x0 (- x <x>))
	   (y0 (- y <y>))
	   (r (truncate (/ y0 <tile-size>)))
	   (c (truncate (/ x0 <tile-size>)))
	   (cells (aref (field-value :grid (or <world> *active-world*))
			(+ <origin-y> r) (+ <origin-x> c))))
      (aref cells (1- (fill-pointer cells))))))

(define-method set-origin viewport (&key x y height width)
  (setf <origin-x> x
	<origin-y> y
	<origin-width> width
	<origin-height> height))

(define-method adjust viewport ()
  "Move the viewport's origin if required to keep the player onscreen."
  (let* ((world (or <world> *active-world*))
	 (world-width (field-value :width world))
	 (world-height (field-value :height world))
	 (player (field-value :player world))
	 (player-x (field-value :column player))
	 (player-y (field-value :row player))
	 (origin-x <origin-x>)
	 (origin-y <origin-y>)
	 (origin-height <origin-height>)
	 (origin-width <origin-width>)
	 (margin <margin>))
    ;; are we outside the "comfort zone"?
    (when (or 
	   ;; too far left
	   (> (+ origin-x margin) 
	      player-x)
	   ;; too far right
	   (> player-x
	      (- (+ origin-x origin-width)
		 margin))
	   ;; too far up
	   (> (+ origin-y margin) 
	      player-y)
	   ;; too far down 
	   (> player-y 
	      (- (+ origin-y origin-height)
		 margin)))
      ;; yes. recenter.
      (setf <origin-x> 
	    (max 0
		 (min (- world-width origin-width)
		      (- player-x 
			 (truncate (/ origin-width 2))))))
      (setf <origin-y> 
	    (max 0 
		 (min (- world-height origin-height)
		      (- player-y 
			 (truncate (/ origin-height 2)))))))))

;;; The minimap

(define-prototype minimap (:parent =viewport=)
  (category-map :initform '((:hidden ".black")
			    (:player ".white")
			    (:item ".gold")
			    (:boss ".yellow")
			    (:enemy ".red")
			    (:target ".blue")
			    (:friend ".green")
			    (:obstacle ".gray20"))) ;;; TODO ?
  (background-color :initform ".black")
  (border-color :initform ".gray20"))
		
(define-method render minimap ()
  [adjust self] ;; hehe
  (let* ((world (or <world> *active-world*))
	 (origin-width <origin-width>)
	 (origin-height <origin-height>)
	 (origin-x <origin-x>)
	 (origin-y <origin-y>)
	 (tile-size <tile-size>)
	 (category-map <category-map>)
	 (grid (field-value :grid world))
	 (image <image>)
	 objects 
	 cell
	 categories)
    (with-field-values (grid light-grid environment-grid phase-number
			     height width 
			     turn-number ambient-light) world
      ;; blank the display
      [clear self]
      ;; ;; draw the border
      ;; (draw-rectangle 0 0 <width> <height>
      ;; 		      :color <border-color>
      ;; 		      :destination <image>)
      ;; draw the minimap
      (dotimes (i origin-height)
	(dotimes (j origin-width)
	  (when (array-in-bounds-p grid i j)
	    (setf objects (aref grid 
				(+ i origin-y)
				(+ j origin-x)))
	    (block coloring
	      (dolist (mapping category-map)
		(destructuring-bind (category color) mapping
		  (dotimes (k (fill-pointer objects))
		    (setf cell (aref objects k))
		    (setf categories (field-value :categories cell))
		    ;; record location of player-entry-point if any
		    (when (member :player-entry-point categories)
		      (draw-circle (* tile-size (- j origin-x))
				   (* tile-size (- i origin-y))
				   4 
				   :destination image 
				   :color ".yellow"))
		    (when (member category categories)
		      (rlx:draw-box (* tile-size j) 
				    (* tile-size i)
				    tile-size tile-size
				    :destination image 
				    :stroke-color color
				    :color color)
		      (return-from coloring)))))))))
      ;; draw player indicator
      (draw-circle (* tile-size (- [player-column world] origin-x))
		   (* tile-size (- [player-row world] origin-y))
		   4 
		   :destination image :color ".white"))))

(defparameter *minimap-help-string* 
"This is the minimap, a form of radar.
Red dots represent enemies; the player's location is indicated in
white.")

(define-method select minimap ()
  (dolist (line (split-string-on-lines *minimap-help-string*))
    [>>narrateln :narrator line]))

(define-method hit minimap (x y)
  (when [parent>>hit self x y]
    self))
  
	
		  


;;; viewport.lisp ends here
