;;; cell.el --- abstract spreadsheet engine for GNU Emacs

;; Copyright (C) 2006, 2007  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: hypermedia, tools, lisp
;; Package-Version: 1.8
;; Time-stamp: <2008-08-10 06:02:00 dto>
;; Version: $Id: cell.el,v 1.80 2007/09/11 09:30:56 dto Exp dto $
;; Website: http://dto.freeshell.org/notebook/

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

;; This file is NOT part of GNU Emacs.

(require 'cl)
(require 'grid)

;;; Cell data structure

;; A cell is a data-containing object in a spreadsheet. 

(defstruct cell
  value ;; The Lisp value (or Eon object) contained in the spreadsheet cell.
  label ;; A string (possibly with text properties) to be displayed.
  width ;; When non-nil, this is the floating-point width (in character widths.)
  tag);; An optional function that may be used to change the
      ;; presentation of the value.

;;; Drawing cells into a buffer

(defvar cell-no-label-string "()")

(defvar cell-blank-width 7)

(defun cell-insert (cell &optional face)
  (let* ((label (cell-label cell))
	 (string (or label (format "%S" (cell-value cell)))))
    (insert string)
    (length string)))

(defun cell-insert-blank (width &optional face)
  (when (> width 0)
    (insert (propertize (make-string width ? ) 
			'face (or face 'cell-blank-face)))
    width))

(defun cell-insert-spacer (width &optional face)
  (insert "Q")
  (backward-char)
  (put-text-property (point) (1+ (point)) 'display
		     `(space . (:width ,width)))
  (when face
    (put-text-property (point) (1+ (point)) 'face face))
  (forward-char))

(defun cell-insert-header (width number)
  (let* ((label (format "%d" number))
	 (blank (make-string (- width (length label)) ? ))
	 (face (if (evenp number)
		  'cell-axis-face
		'cell-axis-odd-face)))
    (insert (propertize (concat blank label)
			'face face))))
  
;;; Spreadsheets full of cells

(defstruct sheet 
  grid            ;; A two-dimensional array of spreadsheet cells.
  display-buffer  ;; The emacs display-buffer where this cell sheet is displayed.
  cursor          ;; Selected cell location in the form (ROW COLUMN).
  name            ;; String name of sheet. This is used to construct the buffer name.
  column-stops    ;; A vector of integers where v[x] is first text column of sheet column x.
  column-styles   ;; A vector of property lists used to customize the appearance of columns.
  row-styles      ;; A vector of property lists used to customize the appearance of rows.
  border-style    ;; When non-nil, draw cell borders.
  header-style    ;; When non-nil, draw row and column headers.
  raw-display-p   ;; When non-nil, use raw display mode.
  selected-tool   ;; Keyword symbol identifying the method to be applied.
  tool-data       ;; Arguments for tool method invocation.
  )

(make-variable-buffer-local
 (defvar *cell-sheet* nil
   "The spreadsheet object displayed in the current buffer."))

(defun* create-cell-sheet (rows columns &optional (name "*untitled sheet*"))
  "Prepare a blank cell sheet of size ROWS x COLUMNS with name NAME."
  (let ((S (make-sheet :name name 
		       :grid (make-grid rows columns)
		       :column-stops (make-vector (+ 1 columns) 0)
		       :column-styles (make-vector (+ 1 columns) nil)
		       :row-styles (make-vector (+ 1 rows) nil)
		       :border-style t :header-style t
		       :display-buffer (get-buffer-create name))))
    (with-current-buffer (sheet-display-buffer S)
      (cell-mode)
      (setf *cell-sheet* S) ;; local var
      (buffer-disable-undo)
      (setf cursor-type nil)
      (setf truncate-lines t)
      (setf buffer-read-only t)
      (goto-char (point-min)))
    S))

(defvar cell-sheet-minimum-header-width 2)

;;; Rendering the spreadsheet

(define-method render spreadsheet ()
  "Render the spreadsheet to its display buffer."
  (with-current-buffer <display-buffer>
    (let* ((inhibit-read-only t)
	   (grid <grid>)
	   (rows (grid-rows grid))
	   (columns (grid-columns grid))
	   (column-stops <column-stops>)
	   (cursor <cursor>)
	   (cursor-row (first cursor))
	   (cursor-column (second cursor))
	   (cursor-cell (gref grid cursor-row cursor-column))
	   (widths (make-vector columns 0))
	   (cell nil)
	   (header-style <header-style>)
	   (label nil)
	   (column-width 0)
	   (cell-width 0)
	   (row-header-width nil)
	   (face nil))
      ;; (setf header-line-format 
      ;; 	    (format " :cursor %S :tool %S :tool-data %S "
      ;; 		    cursor <selected-tool> <tool-data>))
      (if <raw-display-p>
	  ;; raw display mode. just insert all the cell labels with a
	  ;; newline between rows.
	  (progn 
	    (setf cursor-type 'hollow)
	    (delete-region (point-min) (point-max))
	    (dotimes (row rows)
	      (dotimes (column columns)
		(setf cell (gref grid row column))
		(when cell
		  (setf label (cell-label cell))
		  (when label
		    (insert label))))
	      ;; move to next line
	      (insert "\n"))
	    ;; move point to where the spreadsheet cursor says it should be
	    (goto-char (+ 1 cursor-column 
			  (* cursor-row 
			     (+ 1 (grid-columns grid))))))
	  ;; we're not in raw display mode. so, we need to find the
	  ;; column widths and stops before rendering, and draw
	  ;; whatever fancy borders and headers are required.
	  ;;
	  ;; how many digits are in the longest row number?
	  (when header-style
	    (setf row-header-width (length (format "%d" rows))))
	  ;; factor in headers if needed
	  (setf (aref column-stops 0) (or row-header-width 0))
	  ;; now calculate the column widths
	  (dotimes (column columns)
	    (setf column-width 0)
	    (dotimes (row rows)
	      (setf cell (gref grid row column))
	      (setf column-width 
		    (max column-width
			 (if cell
			     (+ 1 
				(ceiling (or (cell-width cell)
					     (length (or (cell-label cell)
							 cell-no-label-string)))))
			     ;; there's no cell at this location. 
			     cell-blank-width))))
	    (setf (aref widths column) column-width)
	    (when (< column columns)
	      (setf (aref column-stops (+ 1 column))
		    (+ column-width (aref column-stops column)))))
	  ;; now draw to the buffer!
	  (delete-region (point-min) (point-max))
	  ;; draw the column headers if needed
	  (when header-style
	    (cell-insert-header row-header-width 0)
	    (dotimes (column columns)
	      (cell-insert-header (- (aref column-stops (+ column 1)) 
						 (aref column-stops column)) column)))
	  (insert "\n")
	  (dotimes (row rows)
	    ;; draw the row header if needed
	    (when header-style
	      (cell-insert-header row-header-width row))
	    ;; render the cells in the row
	    (dotimes (column columns)
	      (setf column-width (aref widths column))
	      (setf cell (grid-get grid row column))
	      (if cell
		  ;; we've got a non-blank cell. draw it.
		  (progn
		    (setf face (list 'cell-default-face
				     (if (evenp column) 'cell-blank-face
				       'cell-blank-odd-face)))
		    (setf cell-width (cell-insert cell face))
		    ;; fill column if needed 
		    (let ((display-width (or (cell-width cell) (length (cell-label cell)))))
		      (if display-width
			  ;; we need to insert spacers to make things
			  ;; line up, because inserted images have
			  ;; made the cell's display-width
			  ;; non-integral.
			  (progn 
			    (cell-insert-spacer (+ 1.0 
							       (- (ceiling display-width)
								  display-width))
							    face)
			    (cell-insert-blank (- column-width 
							      (ceiling display-width)
							      1) 
							   face))
			;; no special handling needed
			(cell-insert-blank (- column-width cell-width)))))
		;; we've got a blank cell
		(setf face (if (evenp column) 'cell-blank-face
			     'cell-blank-odd-face))
		(setf cell-width (cell-insert-blank column-width face)))
	      ;; add text properties to store (ROW COLUMN) for retrieval upon mouse click
	      (let* ((end (point))
		     (beg (- end column-width)))
		(put-text-property beg end 'cell-position (list row column))))
	    ;;
	    ;; this row is completed. move along.
	    (insert "\n"))
	  ;;
	  ;; we're done rendering. now display the cursor
	  (let* ((cursor-cell-display-width (when cursor-cell 
					      (@ cursor-cell :display-width)))
		 (cursor-width (if cursor-cell-display-width
				   (ceiling cursor-cell-display-width)
				   (- (aref column-stops (+ cursor-column 1)) 
				      (aref column-stops cursor-column)))))
	    ;;
	    ;; move point to the right place
	    (goto-char (point-min))
	    (forward-line (+ 1 cursor-row))
	    (forward-char (aref column-stops cursor-column))
	    ;;
	    ;; adjust cursor when images are present in any cells to left
	    (dotimes (c cursor-column)
	      (let ((cl (grid-get grid cursor-row c)))
		(when cl
		  (when (cell-width cl)
		    (backward-char 1)))))
	    ;;
	    ;; now draw the cursor
	    (let* ((p (point)))
	      (put-text-property p (+ p (if cursor-cell
					    (length (@ cursor-cell :label))
					  cell-blank-width))
				 'face 'cell-cursor-face))
	    ;;
	    (set-buffer-modified-p nil))))))

(define-method move-cursor spreadsheet (to)
  "Move the cursor toward TO."
    (let* ((grid <grid>)
	   (rows (grid-rows grid))
	   (cols (grid-columns grid))
	   (cursor <cursor>)
	   (cursor-row (first cursor))
	   (cursor-column (second cursor))
	   (new-cursor 
	    (if (listp to)
		to
	      (case to
		((:up :down :left :right)
		 ;; move one cell in the specified direction
		 (case to
		   (:up (if (/= 0 cursor-row)
			    (list (- cursor-row 1) cursor-column)
			  cursor))
		   (:left (if (/= 0 cursor-column)
			      (list cursor-row (- cursor-column 1))
			    cursor))
		   (:down (if (< cursor-row (- rows 1))
			      (list (+ cursor-row 1) cursor-column)
			    cursor))
		   (:right (if (< cursor-column (- cols 1))
			       (list cursor-row (+ cursor-column 1))
			     cursor))))
		;; move to beginning or end of line
		(:beginning-of-line 
		 (list cursor-row 0))
		(:end-of-line 

		 (list cursor-row (- cols 1)))))))
      (setf <cursor> new-cursor)
      (if <raw-display-p>
	  ;; just move point to where cursor should go
	  (progn
	    (let ((buffer-position (+ 1 (second new-cursor)
				      (* (first new-cursor)
					 (+ 1 (grid-columns grid))))))
	      (goto-char buffer-position))))
      ;; now render
      [render: self]))

;;; Major mode and interactive commands

(defvar cell-mode-map nil)

(when (null cell-mode-map)
  (setq cell-mode-map (make-sparse-keymap))
  (define-key cell-mode-map (kbd "C-n") 'cell-move-cursor-down)
  (define-key cell-mode-map (kbd "C-p") 'cell-move-cursor-up)
  (define-key cell-mode-map (kbd "C-f") 'cell-move-cursor-right)
  (define-key cell-mode-map (kbd "C-b") 'cell-move-cursor-left))

(define-derived-mode cell-mode nil
  "Cell" "An abstract spreadsheet mode."
  nil)

(defun cell-move-cursor (direction)
  (interactive)
  (

(defun cell-move-cursor-up ()
  (interactive)
  (cell-move-cursor :up))

(defun cell-move-cursor-left ()
  (interactive)
  (cell-move-cursor :left))

(defun cell-move-cursor-down ()
  (interactive)
  (cell-move-cursor :down))

(defun cell-move-cursor-right ()
  (interactive)
  (cell-move-cursor :right))

(defun cell-beginning-of-line ()
  (interactive)
  (cell-move-cursor :beginning-of-line))

(defun cell-end-of-line ()
  (interactive)
  (cell-move-cursor :end-of-line))

(defun cell-set-tool (tool)
  (interactive "SKeyword symbol for tool: ")
  (with-local-spreadsheet
    (setf <selected-tool> tool)
    [render: self]))

(defun cell-set-tool-data ()
  (interactive)
  (with-local-spreadsheet
    (setf <tool-data>
	  (read-from-minibuffer "Set tool data to: "
				<tool-data>))
    [render: self]))

;; TODO update these so that they work again
 
;; ;;;; Adding rows and columns

;; (defun cell-insert-row ()
;;   (interactive)
;;   (with-current-cell 
;;    (setf (cell-grid sheet) (grid-insert-row grid cursor-row))
;;    (cell-update)))

;; (defun cell-insert-column ()
;;   (interactive)
;;   (with-current-cell
;;    (let ((columns (+ 1 (grid-columns (cell-grid sheet)))))
;;      (setf (cell-grid sheet) (grid-insert-column grid cursor-column))
;;      (setf (cell-column-stops sheet) (make-vector (+ 1 columns) 0))))
;;   (cell-update))

;; ;;;; Removing rows and columns

;; (defun cell-kill-row ()
;;   (interactive)
;;   (with-current-cell
;;    (setf (cell-grid sheet) (grid-delete-row grid cursor-row))
;;    (cell-update)))

;; (defun cell-kill-column ()
;;   (interactive)
;;   (with-current-cell
;;    (setf (cell-grid sheet) (grid-delete-column grid cursor-column))
;;    (cell-update)))
				  				 			     
;;;; Mouse support

;; (defun cell-mouse-move-cursor (event)
;;   (interactive "e")
;;   (with-current-cell
;;    (when event
;;      (destructuring-bind (event-type position &optional ignore) event
;;        (let* ((clicked-position (posn-point position))
;; 	      (clicked-cell (get-text-property clicked-position 'cell-mode-position)))
;; 	 ;;
;; 	 ;; are we in raw display mode? 
;; 	 (when (getf properties :raw-display)
;; 	   (goto-char clicked-position)
;; 	   ;;
;; 	   ;; bounds check 
;; 	   (let ((clicked-row (/ clicked-position (+ 1 (grid-columns grid))))
;; 		 (clicked-column (+ -1 (% clicked-position (+ 1 (grid-columns grid))))))
;; 	     (when (and (<= 0 clicked-row)
;; 			(<= 0 clicked-column)
;; 			(< clicked-row (grid-rows grid))
;; 			(< clicked-column (grid-columns grid)))
;; 	       (setf (cell-cursor sheet) 
;; 		     (list clicked-row clicked-column)))))
;; 	 ;;
;; 	 ;; not in raw display
;; 	 (when clicked-cell
;; 	   (destructuring-bind (clicked-row clicked-column) clicked-cell
;; 	     (setf (cell-cursor sheet) clicked-cell)
;; 	     ;; clear selection
;; 	     (setf (cell-selection sheet) nil)
;; 	     (cell-render sheet))))))))

;;;; Customization

(defgroup cell-mode nil
  "Options for cell-mode."
  :group 'applications)

;;; Faces

(defvar cell-default-face
  (defface cell-default-face '((t (:foreground "gray80" :background "gray60")))
    "Default face for cells."))

(defvar cell-mouse-face 
  (defface cell-mouse-face '((t (:foreground "yellow" :background "darkblue")))
    "Mouseover face for cells."))

(defface cell-blank-face '((t (:background 
			       "gray20" 
			       :box 
			       (:line-width 1 :color "grey30"))))
  "Face for blank cells." :group 'cell)

(defface cell-blank-odd-face '((t (:background 
				   "gray18" 
				   :box 
				   (:line-width 1 :color "grey29"))))
  "Face for blank cells in odd columns" :group 'cell)
 
(defface cell-axis-face '((t :foreground "gray45" 
			     :background "gray25" 
			     :box (:line-width 1 :color "grey30")))
  "Face for numbered axes."  :group 'cell)
      			   
(defface cell-axis-odd-face '((t :foreground "gray42" 
			     :background "gray23" 
			     :box (:line-width 1 :color "grey29")))
  "Face for numbered axes in odd columns." :group 'cell)


;;; Abstract user operations

;; These user interface widgets do not know anything about mouse
;; clicks or key-presses. Instead, they recieve more abstract
;; "operations" which may then be bound to various input methods such
;; as the keyboard, mouse, speech synthesizer, braille terminal, and
;; so on.

;; Simple yes-or-no questions:

;; :YES 
;; :NO

;; More serious operations that will take a long time, or that can't
;; be undone:

;; :CONFIRM
;; :CANCEL

;; Generic tools framework:

;; :SELECT-TOOL
;; :APPLY-TOOL  <--- arguments come from a tool options dialog

;; Moving data around:

;; :MARK
;; :KILL
;; :YANK

;; Choosing options and then executing them:

;; :SELECT
;; :EXECUTE

;; Rearranging, creating and destroying objects:

;; :UNDO
;; :REDO

;; :REMOVE
;; :ADJOIN
;; :DELETE
;; :CLONE

;; :MOVE
;; :JOIN 
;; :CUT
;; :POSITION

;;; Textboxes

;;; Numbers

;;; Sliders

;;; Buttons

;;; Templates

;;; Toolbars

;;; Dataflows
	   
(provide 'cell)
;;; cell.el ends here


