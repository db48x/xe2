;;; vwindow.el --- overlay virtual windows onto part of a buffer

;; Copyright (C) 2007  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: data, lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'grid)

;;; Virtual windows

(defobject+ :vwindow 
    "Overlay virtual windows on part of a buffer."
  ;; Coordinates of the vwindow, with origin at top-left of the real
  ;; window
  (left 'integer :default 0)
  (top 'integer :default 0)
  width height ;; These are fixed when you call `vwindow-create'.
               ;; Text that doesn't fit in the vwindow will not be drawn.
  ;; Location and appearance of our fake cursor.
  (cursor-row 'integer :default 0) 
  (cursor-column 'integer :default 0)
  (cursor-style 'keyword :default :block)
  source-buffer ;; Place where display text is copied from.
  display-strings ;; A list of strings (possibly with text
		  ;; properties). Each string is a single line of the
		  ;; output to be superimposed on the buffer.
  overlays ;; A list of overlays, one per line, used to superimpose
	   ;; the vwindow onto the buffer.
  display-buffer ;; The buffer where the vwindow is to be displayed.
  display-options ;; Property list of display options (margins, faces, etc)
  decorations ;; Widgets that are specific to this vwindow.
  widgets ;; A grid of cells which can contain any widget.
  dragging ;; The widget being dragged (if any).
  joining ;; The widget being joined (if any).
  popup-p ;; Non-nil for popup vwindows.
  )

(defmethod+ :create :vwindow 
  (width height
	 &key (display-buffer (current-buffer)) 
	 source-buffer
	 (left 0) (top 0)
	 (cursor-row 0) (cursor-column 0)
	 cursor-style)
  (setf @width width 
	@height height
	@display-buffer display-buffer
	@source-buffer source-buffer
	@left left @top top
	@cursor-row cursor-row
	@cursor-column cursor-column
	@cursor-style cursor-style)
  self)

(defmethod+ :beep :vwindow ()
  (beep))

(defmethod+ :move :vwindow (left top)
  (when left 
    (setf @left left))
  (when top
    (setf @top top)))
    
;;; %%%%%%%%%%%%%%%
;; (defvar foo)
;; (setf foo (clone-object :vwindow))
;; (>> :create foo 0 0 :display-buffer (get-buffer-create "*VWINDOW*") :source-buffer "*scratch*")
;; (>> :move foo 1 2)
;; (>> :beep foo)
;; (>> :render foo)
;; (>> :show foo)
;; (>> :hide foo)
;;; %%%%%%%%%%%%%%%

(defsubst* current-line-string (&optional (end (point-at-eol)))
  (buffer-substring (point-at-bol) end))

(defmethod+ :render :vwindow ()
 (save-excursion
    (let ((line-count 0) line-lengths)
      ;; slice source text into display strings
      (with-current-buffer @source-buffer
	(save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
	    (push (current-line-string) @display-strings)
	    (push (length (car @display-strings))
		  line-lengths)
	    (incf line-count)
	    (forward-line 1))))
      (setf @width (apply #'max line-lengths)
	    @height line-count)
      (callf nreverse @display-strings))))

(defmethod+ :show :vwindow ()
     (save-excursion
	;; create overlays if needed
	(unless @overlays
	  (dotimes (n @height)
	    (push (make-overlay n (1+ n) @display-buffer)
		  @overlays)))
	;; now update the overlays and display them
	(let ((strings @display-strings)
	      (ov @overlays))
	  (dotimes (n @height)
	    (let (bol eol)
	      (save-excursion 
		(goto-line (+ 1 n @top))
		(setf bol (point-at-bol)
		      eol (point-at-eol)))
	      (let* ((begin (+ @left bol))
		     (end (min (+ begin (length (car strings)))
			       eol)))
		(move-overlay (car ov) begin end)
		(overlay-put (pop ov) 'display (pop strings))))))))

(defmethod+ :hide :vwindow ()
	    (mapcar #'delete-overlay @overlays)
	    (setf @overlays nil))
	    

;; ;; TODO Picture-mode borders

;; ;; TODO allow application to control which lines are updated when

;; (defun* vwindow-hide (v)
;;   (mapc #'delete-overlay (vwindow-overlays v))
;;   (setf (vwindow-overlays v) nil))

;; ;;; Vwidgets

;; (defstruct vwidget 
;;   parent ;; parent vwidget, if any
;;   children ;; list of child widgets
;;   top left height width)

;; (defun vwidget-create 


;; (defvar baz)

;; (defvar foobuf (get-buffer-create "foobuf"))
;; (setf foobuf (get-buffer-create "foobuf"))

;; (defun* vwindow-demo ()
;;   (interactive)
;;   (let ((data-buffer (get-buffer-create "*vwindow demo*")))
;;     (with-current-buffer data-buffer
;;       (delete-region (point-min) (point-max))
;;       (dotimes (n 5)
;; 	(insert (format "Hello %d\n" n))))
;;     (setf baz (vwindow-create :display-buffer foobuf
;; 			     :data-buffer data-buffer
;; 			     :left 5 :top 10))
;;     (vwindow-show baz)))

;; baz
;; (vwindow-demo)
;; (vwindow-hide baz)

      

(provide 'vwindow)
;;; vwindow.el ends here


