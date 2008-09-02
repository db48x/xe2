;;; vex.el --- interactive svg graphics in a buffer

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: hypermedia, multimedia, extensions

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

;; This is a proof-of-concept. Requires recent (i.e. CVS) GNU Emacs.

;;; Code:

(require 'cl)

(defvar vex-svg-preamble "<?xml version=\"1.0\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")

(defun* vex-begin-svg (&key (height 100) (width 100))
  (insert vex-svg-preamble)
  (insert (format "<svg xmlns=\"http://www.w3.org/2000/svg\"
 width=\"%d\" height=\"%s\">" height width)))

(defun vex-end-svg ()
  (insert "</svg>"))

(defvar vex-default-style "fill:#ffffff; stroke:#666666; stroke-width:1px;")

(defun* vex-insert-rectangle (&key (x 0) (y 0) 
				    (height 10) (width 10) 
				    (style vex-default-style) (rx 0))
  (insert (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" rx=\"%d\"
 style=\"%s\" />" x y height width rx style)))

(defun vex-image-map-rect (x0 y0 x1 y1)
  `(rect . ((,x0 . ,y0) . (,x1 . ,y1))))

;; (vex-image-map-rect 1 2 3 4)

;;; Demo

(defvar vex-keymap (make-sparse-keymap))

(defvar vex-ids [a b c d e f])

(defun vex-click (event)
  (interactive "e")
  (message "You have clicked: %S" (posn-area (second event))))

(defvar vex-svg-buffer (get-buffer-create "*vex-demo*"))

(defun vex-show-demo ()
  (interactive)
  (assert (image-type-available-p 'svg))
  (display-buffer vex-svg-buffer)
  (let (rectangles svg-string map (n 0))
    (setf svg-string (with-temp-buffer
		(vex-begin-svg :height 200 :width 200)
		(dotimes (n 5)
		  (let ((x (random* 150))
			(y (random* 150))
			(height (+ 10 (random* 40)))
			(width (+ 10 (random* 40))))
		    (vex-insert-rectangle :x x
					   :y y
					   :height height
					   :width width)
		    (push (vex-image-map-rect x y
					       (+ x width) (+ y height))
			  rectangles)))
		(vex-end-svg)
		(buffer-substring-no-properties (point-min) (point-max))))
    (with-current-buffer vex-svg-buffer
      (delete-region (point-min) (point-max))
      (setf cursor-type nil)
      (insert-image (create-image svg-string 'svg t 
				  :pointer 'arrow
				  :map (mapcar (lambda (r)
						 (define-key vex-keymap
						     (vector (aref vex-ids (incf n)) 'mouse-1)
						     'vex-click)
						 (list r (aref vex-ids n) (list 'pointer 'hand)))
					       rectangles)))
      (add-text-properties (point) (- (point) 1)
      			   (list 'local-map vex-keymap)))))
      
  


(provide 'vex)
;;; vex.el ends here
