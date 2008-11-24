;;; browser.lisp --- object discovery and interaction widget menu thing

;; Copyright (C) 2008  David O'Toole

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

;;; Commentary:

;; Browsable items should provide the following fields:

;;  :tile  --- String image icon name
;;  :name  --- String object name

;; You must also define some methods:

;;  [is-disabled item] should return non-nil when the menu item is to
;;  be grayed out.

;;  [open item] should return either:
;; 
;;    - a new array to browse, or
;;    - a function to invoke after exiting the menu

;;; Code:

(in-package :rlx)

(define-prototype browser (:parent =formatter=)
  (collection :documentation "The vector of CLON objects being browsed.")
  (cursor :initform 0
	  :documentation "The array index of the currently selected object.")
  (history :documentation "Recently browsed collections."))

(define-method cursor-next browser ()
  (when (array-in-bounds-p <collection> (+ 1 <cursor>))
    (incf <cursor>)))

(define-method cursor-previous browser ()
  (when (array-in-bounds-p <collection> (- <cursor> 1))
    (decf <cursor>)))

(define-method cursor-item browser ()
  (aref <collection> <cursor>))

(define-method follow browser ()
  (let* ((item [cursor-item self]))
    (push item <history>)
    (setf <collection> 
	  [open item])))

(define-method back browser ()
  (setf <collection> (pop <history>)))

(define-method print-object browser (object &optional selected-p)
  "Print the OBJECT in the browser as a new formatted line.
When SELECTED-P is non-nil, draw the highlighted (or otherwise
visually distinguished) version of the line."
  (if (null object)
      [println self " [EMPTY] " :foreground ".gray20"]
      (progn 
	(let ((tile (field-value :tile object))
	      (name (field-value :name object)))
	  (if selected-p
	      [print self ">" :foreground ".yellow" :background ".purple"]
	      [print self " "])
	  [print self " "]
	  [print self nil :image tile]
	  [print self " "]
	  [println self name]))))

(define-method update browser ()
  (let ((collection <collection>)
	(cursor <cursor>))
    [delete-all-lines self]
    (dotimes (n (length collection))
      [print-object self (aref collection n) (= cursor n)])))

(define-method set-collection browser (collection)
  (setf <collection> collection)
  (setf <cursor> 0))

;;; A menu item cell.

;; TODO menu item cell

;;; An equipment browser

(define-prototype equipment (:parent =browser=))

(define-method print-object equipment (object &optional selected-p)
  (declare (ignore selected-p))
  (let* ((tile (field-value :tile object))
	 (name (field-value :name object))
	 (equipment (field-value :equipment object))
	 (equipment-slots (field-value :equipment-slots object))
	 (fill-width (apply #'max 5 (mapcar #'(lambda (s) 
					      (length (symbol-name s)))
					  equipment-slots)))
	 item)
    [print self "Equipment for: "]
    [print self name]
    [print self nil :image tile]
    [space self]
    [println self name]
    (dolist (slot equipment-slots)
      (setf item [equipment-slot object slot])
      [print self (format nil (format nil "~~~dS " fill-width) slot)]
      (when item
	[print self nil :image (field-value :tile item)]
	[space self]
	[print self (field-value :name item)])
      [newline self])
    [newline self]))
    
;;; browser.lisp ends here
