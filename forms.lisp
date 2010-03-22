;;; forms.lisp --- port of cell-mode to common lisp

;; Copyright (C) 2006, 2007, 2010  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: 

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

(in-package :xe2)

(defvar *forms* nil "Hash table mapping string form names to form objects.")

(defun initialize-forms-table-maybe (&optional force)
  (when (or force (null *forms*))
    (setf *forms* (make-hash-table :test 'equal))))

(define-prototype form 
    (:parent =widget= :documentation  "An interactive graphical spreadsheet.")
  (world :documentation "The xe2:=world= of objects to be displayed.")
  rows columns
  (cursor-row :initform 0) 
  (cursor-column :initform 0)
  (needs-update :initform nil)
  (name :documentation "String name of the form.")
  (column-widths :documentation "A vector of integers where v[x] is the pixel width of form column x.")
  (column-styles :documentation "A vector of property lists used to customize the appearance of columns.")
  (row-spacing :initform 1 :documentation "Number of pixels to add between rows.")
  (zebra-stripes :documentation "When non-nil, zebra stripes are drawn.")
  (row-styles :documentation "A vector of property lists used to customize the appearance of rows.")
  (border-style :initform t :documentation "When non-nil, draw cell borders.")
  (draw-blanks :initform t :documentation "When non-nil, draw blank cells.")
  (header-style :initform t :documentation "When non-nil, draw row and column headers.")
  (selected-tool :documentation "Keyword symbol identifying the method to be applied.")
  (tool-data :documentation "Arguments for tool method invocation."))

(define-method initialize form (&key (name "*untitled form*") world)
  (initialize-forms-table-maybe)
  [parent>>initialize self]
  (when world 
    [configure self world]))

(define-method configure form (&optional world)
  (when world (setf <world> world))
  (setf <rows> (field-value :height world)
	<columns> (field-value :width world))
  (setf <column-widths> (make-array (+ 1 <columns>) :initial-element 0)
	<column-styles> (make-array (+ 1 <columns>))
	<row-styles> (make-array (+ 1 <rows>))))

(define-method cell-at form (row column)
  [top-cell-at <world> row column])

(define-method row-height form (row)
  (let ((height 0) cell)
    (dotimes (column <columns>)
      (setf cell [cell-at self row column])
      (when cell
	(setf height (max height [form-height cell]))))
    height))

(define-method column-width form (column)
  (let ((width 0) cell)
    (dotimes (row <rows>)
      (setf cell [cell-at self row column])
      (when cell
	(setf width (max width [form-width cell]))))
    width))

(define-method compute-column-widths form ()
  (dotimes (column <columns>)
    (setf (aref <column-widths> column)
	  [column-width self column])))

(define-method queue-update form ()
  (setf <needs-update> t))

(defparameter *even-columns-format* '(:background ".gray50" :foreground ".gray10"))
(defparameter *odd-columns-format* '(:background ".gray45" :foreground ".gray10"))

(define-method render form ()
  [clear self]
  (when <world>
    [compute-column-widths self]
    (let ((image <image>))
      (let ((x 0) (y 0))
	(dotimes (row <rows>)
	  (setf x 0)
	  (dotimes (column <columns>)
	    (let ((cell [cell-at self row column]))
	      (if (null cell)
		  (when <draw-blanks>
		    (draw-box x y 
			      [column-width self column]
			      [row-height self row] 
			      :stroke-color ".gray30"
			      :color (if (evenp column) ".gray50" ".gray45")
			      :destination image))
		  (progn [compute cell]
			 [form-render cell image x y]))
	      (incf x (aref <column-widths> column))))
	  (incf y (+ <row-spacing> [row-height self row])))))))
	  
;;; A data cell just prints out the stored value.

(defparameter *data-cell-style* '(:foreground ".gray40" :background ".white"))

(defcell data-cell data)

(define-method set data-cell (data)
  (setf <data> data))

(define-method get data-cell ()
  <data>)

(define-method compute data-cell ()
  ;; update the label
  (setf <label> (list (cons (format nil " DATA: ~S  " <data>) *data-cell-style*))))

;;; A var cell stores a value into a variable. 

(defparameter *var-cell-style* '(:foreground ".white" :background ".blue"))

(defcell var-cell variable)

(define-method initialize var-cell (variable)
  (setf <variable> variable))

(define-method set var-cell (value)
  [set-variable *world* <variable> value])

(define-method get var-cell ()
  [get-variable *world* <variable>])

(define-method compute var-cell ()
  (setf <label> (list (cons (format nil " VARIABLE: ~A  " <variable>) *var-cell-style*))))

;;; forms.lisp ends here
