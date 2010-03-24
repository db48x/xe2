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
  (name :documentation "String name of the form.")
  (column-widths :documentation "A vector of integers where v[x] is the pixel width of form column x.")
  (row-heights :documentation "A vector of integers where v[x] is the pixel height of form row x.")
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
  [install-keybindings self]
  (setf <cursor-row> 0 <cursor-column> 0)
  (setf <rows> (field-value :height world)
	<columns> (field-value :width world))
  (setf <column-widths> (make-array (+ 1 <columns>) :initial-element 0)
	<row-heights> (make-array (+ 1 <rows>) :initial-element 0)
	<column-styles> (make-array (+ 1 <columns>))
	<row-styles> (make-array (+ 1 <rows>))))

(define-method cell-at form (row column)
  [top-cell-at <world> row column])

(define-method install-keybindings form ()
  (bind-key-to-method self "RETURN" nil :select)
  (bind-key-to-method self "UP" nil :move-cursor-up)
  (bind-key-to-method self "DOWN" nil :move-cursor-down)
  (bind-key-to-method self "LEFT" nil :move-cursor-left)
  (bind-key-to-method self "RIGHT" nil :move-cursor-right))

(define-method current-cell form ()
  [cell-at self <cursor-row> <cursor-column>])

(define-method select form ()
  (let ((cell [current-cell self]))
    (when cell
      [select cell])))

(defparameter *blank-cell-string* '(" ........ "))

(define-method row-height form (row)
  (max (formatted-string-height *blank-cell-string*)
       (let ((height 0) cell)
	 (dotimes (column <columns>)
	   (setf cell [cell-at self row column])
	   (when cell
	     (setf height (max height [form-height cell]))))
	 height)))

(define-method column-width form (column)
  (max (formatted-string-width *blank-cell-string*)
       (let ((width 0) cell)
	 (dotimes (row <rows>)
	   (setf cell [cell-at self row column])
	   (when cell
	     (setf width (max width [form-width cell]))))
	 width)))

(define-method compute-geometry form ()
  (dotimes (column <columns>)
    (setf (aref <column-widths> column)
	  [column-width self column]))
  (dotimes (row <rows>)
    (setf (aref <row-heights> row)
	  [row-height self row])))

(defparameter *even-columns-format* '(:background ".gray50" :foreground ".gray10"))
(defparameter *odd-columns-format* '(:background ".gray45" :foreground ".gray10"))

(define-method handle-key form (event)
  ;; possibly forward event to current cell. used for the event cell, see below.
  (let ((cell [current-cell self]))
    (if (and cell (has-method :handle-key cell))
	(or [handle-key cell event]
	    [parent>>handle-key self event])
	[parent>>handle-key self event])))

;; TODO (define-method hit form (x y) 

(define-method render form ()
  [clear self]
  (when <world>
    (with-field-values (cursor-row cursor-column row-heights world
				   row-spacing rows columns draw-blanks column-widths) self
      [compute-geometry self]
      (let ((image <image>)
	    (x 0) (y 0) (data nil)
	    (cursor-dimensions nil))
	(dotimes (row rows)
	  (setf x 0)
	  (setf data nil)
	  (dotimes (column columns)
	    (let ((column-width (aref column-widths column))
		  (row-height (aref row-heights row))
		  (cell [cell-at self row column]))
	      (if (null cell)
		  (progn (setf data nil)
			 (when draw-blanks
			   (draw-box x y 
				     column-width 
				     row-height  
				     :stroke-color ".gray30"
				     :color (if (evenp column) ".gray50" ".gray45")
				     :destination image)))
		  (let ((*world* world))
		    (when data 
		      [set cell data])
		    [compute cell]
		    (setf data [get cell])
		    [form-render cell image x y column-width]))
	      ;; possibly draw cursor
	      (when (and (= row cursor-row) (= column cursor-column))
		(setf cursor-dimensions (list x y column-width row-height)))
	      ;; move to next column right
	      (incf x (aref column-widths column))))
	  ;; move to next row down
	  (incf y (+ row-spacing (aref row-heights row))))
	;; render cursor if any
	(when cursor-dimensions
	  (destructuring-bind (x y w h) cursor-dimensions
	    [draw-cursor self x y w h]))))))

;;; Cursor

(define-method draw-cursor form (x y width height)
  (draw-rectangle x y width height :color ".red" :destination <image>))

(define-method move-cursor form (direction)
  (with-field-values (cursor-row cursor-column rows columns) self
    (let ((cursor (list cursor-row cursor-column)))
      (setf cursor (ecase direction
		     (:up (if (/= 0 cursor-row)
			      (list (- cursor-row 1) cursor-column)
			      cursor))
		     (:left (if (/= 0 cursor-column)
				(list cursor-row (- cursor-column 1))
				cursor))
		     (:down (if (< cursor-row (- rows 1))
				(list (+ cursor-row 1) cursor-column)
				cursor))
		     (:right (if (< cursor-column (- columns 1))
				 (list cursor-row (+ cursor-column 1))
				 cursor))))
      (destructuring-bind (r c) cursor
	(setf <cursor-row> r <cursor-column> c)))))

(define-method move-cursor-up form ()
  [move-cursor self :up])

(define-method move-cursor-down form ()
  [move-cursor self :down])

(define-method move-cursor-left form ()
  [move-cursor self :left])

(define-method move-cursor-right form ()
  [move-cursor self :right])

;;; A data cell just prints out the stored value.

(defparameter *data-cell-style* '(:foreground ".gray40" :background ".white"))

(defcell data-cell data)

(define-method set data-cell (data)
  (setf <data> data))

(define-method get data-cell ()
  <data>)

(define-method compute data-cell ()
  ;; update the label
  (setf <label> (list (cons (format nil " ~S  " <data>) *data-cell-style*))))

;;; A var cell stores a value into a variable, and reads it.

(defparameter *var-cell-style* '(:foreground ".white" :background ".blue"))

(defcell var-cell variable)

(define-method initialize var-cell (variable)
  (setf <variable> variable))

(define-method set var-cell (value)
  [set-variable *world* <variable> value])

(define-method get var-cell ()
  [get-variable *world* <variable>])

(define-method compute var-cell ()
  (setf <label> (list (cons (format nil ">> ~A  " <variable>) *var-cell-style*))))

;;; Event cell picks up next event when clicked

(defparameter *event-cell-style* '(:foreground ".yellow" :background ".forest green"))

(defcell event-cell event capturing)

(define-method set event-cell (event)
  (setf <event> event))

(define-method get event-cell ()
  <event>)

(define-method handle-key event-cell (event)
  (when <capturing> 
    ;; signal that we handled this event
    (prog1 t
      (setf <event> event)
      (setf <capturing> nil))))
  
(define-method compute event-cell () 
  (setf <label> 
	(list (cons (if <capturing>
			" CAPTURING... "
			(destructuring-bind (key &rest modifiers) <event>
			  (if modifiers
			      (let ((mod-string (format nil " ~A" 
							(apply #'concatenate 'string 
							       (mapcar #'(lambda (mod)
									   (format nil "~A " mod))
								       modifiers)))))
				(if (string= "JOYSTICK" key)
				    (concatenate 'string key mod-string)
				    (concatenate 'string mod-string key " ")))
			      (concatenate 'string " " key " "))))
		    *event-cell-style*))))

(define-method select event-cell ()
  ;; capture next event
  (setf <capturing> t))

;;; Comment cell just displays text.

(defparameter *comment-cell-style* '(:foreground ".white" :background ".gray20"))

(defcell comment-cell comment)

(define-method initialize comment-cell (comment)
  (setf <comment> comment))
  
(define-method set comment-cell (comment) nil)

(define-method get comment-cell ()
  <comment>)

(define-method compute comment-cell () 
  (setf <label> (list (cons (format nil " ~A " <comment>) *comment-cell-style*))))

;;; Button cell executes some lambda.

(defparameter *button-cell-style* '(:foreground ".yellow" :background ".red"))

(defparameter *button-cell-highlight-style* '(:foreground ".red" :background ".white"))

(defparameter *button-cell-highlight-time* 15)

(defcell button-cell button clock)

(define-method initialize button-cell (&key closure text)
  (setf <closure> closure)
  (setf <clock> 0)
  (setf <label> (list (cons text *button-cell-style*))))
  
(define-method set button-cell (button) nil)

(define-method get button-cell () <closure>)

(define-method compute button-cell () 
  (with-fields (label clock) self
    (unless (zerop clock)
      (decf clock))
    (setf (cdr (first label))
	  (if (plusp clock)
	      *button-cell-highlight-style*
	      *button-cell-style*))))

(define-method select button-cell ()
  (funcall <closure>)
  (setf <clock> *button-cell-highlight-time*))

;; (error cons-game::*form*)

;;; forms.lisp ends here
