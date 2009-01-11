;;; pool.lisp --- simple data pool for reusing lisp structs and other data

;; Copyright (C) 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: lisp, data

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

;; A data pool re-uses 

;;; Code:

(in-package :rlx)

(defstruct pool-element active-p data)

(defstruct pool 
  ;; The following are required slots. You must specify a value for
  ;; them when using `create-pool' (see below).
  constructor ;; Function that creates a new element.
  growth-rate ;; Real size multiplier when growing array.
  ;; Read-only slots. 
  vector ;; The storage array for the data.
  size ;; Number of positions in <vector>
  count ;; Number of data-occupied positions in <vector>
  )

(defun create-pool (&key constructor growth-rate initial-size)
  (assert (functionp constructor))
  (assert (and (numberp growth-rate) (integerp initial-size)))
  (let ((vector (make-array initial-size 
				 :element-type 'pool-element 
				 :adjustable t)))
    (make-pool :constructor constructor
	       :growth-rate growth-rate 
	       :size initial-size
	       :vector vector
	       :count 0)))

(defun pool-active-at-p (pool n)
  (let* ((vector (pool-vector pool))
	 (element (aref vector n)))
    (when element
      (pool-element-active-p element))))

(defun pool-allocate-position (pool n)
  (let ((element (pref pool n)))
    (setf (pref pool n) 
	  (make-pool-element :data (funcall (pool-constructor pool))))))

(defun pref (pool n)
  (aref (pool-vector pool) n))

(defun set-pref (pool n value)
  (setf (aref (pool-vector pool) n) value))

(defsetf pref set-pref)

(defun pref-data (pool n)
  (when (pool-active-at-p pool n)
    (pool-element-data (pref pool n))))

(defun set-pref-data (pool n data)
  (let ((element (pref pool n)))
    (when (null (pref pool n))
      (pool-allocate-position pool n))
    (setf (pool-element-data (pref pool n)) data)
    (setf (pool-element-active-p (pref pool n)) t)))

(defsetf pref-data set-pref-data)

(defun pool-remove (pool n)
  (assert (pool-active-at-p pool n))
  (setf (pool-element-active-p (pref pool n))
	nil))

(provide 'pool)
;;; pool.lisp ends here
