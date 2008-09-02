;;; grid.el --- simple two-dimensional grid library

;; Copyright (C) 2007  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: data

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

;;; Dependencies

(require 'cl)

;;; The basics

(defun make-grid (rows columns &optional seed vector-p)
  "Create a grid of size ROWS x COLUMNS.  Each cell is filled
with a copy of SEED; the copy is made with `copy-tree'."
  (let ((grid (make-vector rows nil)))
    (dotimes (row rows) 
      (setf (aref grid row)
	    (make-vector columns nil))
      (dotimes (col columns) 
	(setf (gref grid row col)
		(copy-tree seed vector-p))))
    grid))

(defsubst grid-get (grid row column)
  "Retrieve the value of ROW, COLUMN from the GRID."
  (aref (aref grid row) column))

(defsubst grid-set (grid row column value)
  "Store VALUE in the GRID at location ROW, COLUMN."
  (let ((row (aref grid row)))
    (setf (aref row column) value)))

(defsubst grid-columns (grid)
  "Return the number of columns in GRID."
  (length (aref grid 0)))

(defsubst grid-rows (grid)
  "Return the number of rows in GRID."
  (length grid))

;;; Insertion and deletion

(defun vector-insert (oldvec pos elt)
  "Insert ELT into OLDVEC at POS, moving elements at POS and
afterward down the list."
  (let* ((len (length oldvec))
	 (newvec (make-vector (+ len 1) nil)))
    (dotimes (i (+ 1 len))
      (setf (aref newvec i) (cond 
			     (( < i pos)
			      (aref oldvec i))
			     (( equal i pos)
			      elt)
			     (( > i pos) 
			      (aref oldvec (- i 1))))))
    newvec))

(defun vector-delete (oldvec pos)
  "Remove position POS from OLDVEC."
  (let* ((len (length oldvec))
	 (newvec (make-vector (- len 1) nil)))
    (dotimes (i (- len 1))
      (setf (aref newvec i) (cond
			     (( < i pos)
			      (aref oldvec i))
			     (( >= i pos)
			      (aref oldvec (+ i 1))))))
    newvec))

(defun grid-insert-row (grid row &optional seed)
  "Returns a copy of GRID with a row inserted at row ROW. You should
replace the original grid with this one."
  (let* ((newrow (make-vector (grid-columns grid) nil)))
    (vector-insert grid row newrow)))
	
(defun grid-insert-column (grid col)
  "Returns a copy of GRID with a column inserted at column COL. You
should replace the original grid with this one."
  (dotimes (i (grid-rows grid))
    (setf (aref grid i) (vector-insert (aref grid i) col nil)))
  grid)
          
(defun grid-delete-row (grid row)
  "Returns a copy of GRID with the row ROW removed."
  (vector-delete grid row))

(defun grid-delete-column (grid col)
  "Returns a copy of GRID with the column COL removed."
  (dotimes (i (grid-rows grid))
    (setf (aref grid i) (vector-delete (aref grid i) col)))
  grid)

;;; Setf support for grids

(defalias 'gref 'grid-get)
(defsetf gref grid-set) 

(provide 'grid)
;;; grid.el ends here

