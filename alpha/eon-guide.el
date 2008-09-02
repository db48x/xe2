;;; eon-guide.el --- programmer's guide to Eon

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: oop

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

;;; Commentary

;; (UNDER CONSTRUCTION)

;; This file is a programmer's tutorial and reference guide for the
;; Eon object system. It presumes a working knowledge of both Emacs
;; Lisp and at least some parts of the CL package.

;; Commentary and examples are interspersed. I've tried to make them
;; into little interactive demos, where you can evaluate the top-level
;; forms one at a time with C-x C-e. You can do M-x orgstruct-mode to
;; get overviews of the file.

;;; Links

;; http://dto.mamalala.org/notebook/eon.html

;;; Quick Start

(require 'eon)

;; We'll need our own scratch buffer to display demo output in.

(defvar *eon-demo-buffer* (get-buffer-create "*eon-demo-buffer*"))

(defmacro with-demo-buffer (&rest body)
  (declare (indent 0))
  `(with-current-buffer *eon-demo-buffer*
     ,@body))

(defun clear-demo-buffer ()
  (interactive)
  (with-demo-buffer
    (delete-region (point-min) (point-max))))

;; Now let's put that buffer in the other window.

(progn
  (delete-other-windows)
  (switch-to-buffer-other-window *eon-demo-buffer*)
  (other-window 1))

;; Let's define a prototype object representing a circle.  It should
;; have fields for the X and Y coordinates of the center of the
;; circle, and of course the radius.

(define-prototype circle 
  (:documentation "A simple circle object.")
  (x :initform 0)
  (y :initform (+ 2 2))
  (radius :initform 1 :documentation "The radius of the circle."))

;; To create an instance, we use the `clone' function.

(defvar circle)
(setf circle (clone :circle)) 

;; (The keyword :circle refers to the circle prototype defined above
;; with `define-prototype'. You can use a selector--i.e. a symbol like
;; :circle--in place of an object in many places.)

;; Try this: 

(slot-value :x circle)
(setf (slot-value :x circle) 7)

;; The "@" operator is an alias for `slot-value'.

(setf (@ :y circle) 8) 
(defvar fnorb :radius)
(setf (@ fnorb circle) 1.2)
(list (@ :x circle)
      (@ :y circle)
      (@ :radius circle))

;; And now a few methods. Within the body of a `define-method' (and
;; several other Eon macros) several special syntax features are
;; available:

;; <foo> is a slot reference. It becomes (@ :foo self).
;; [bar: object a b c ...] is a method call. It becomes 
;;   (>> :bar object a b c ...).

(define-method area circle ()
  "Compute the area of the circle."
  (let ((r <radius>))
    (* pi r r)))

;; To invoke a method, use the `>>' operator:

(>> :area circle)

(define-method draw circle ()
  "Draw the circle to the demo-buffer using artist-mode commands."
  (with-demo-buffer
    (goto-char (point-min))
    (artist-mode 1)
    (artist-draw-circle <x> <y> (+ <radius> <x>) <y>)
    (artist-mode-off)))

(define-method move circle (x y)
  "Move the circle to X,Y."
  (setf <x> x
	<y> y))

;; Now for a brief demo. The macro `prog+' lets you execute a block of
;; code in Eon syntax, without writing a method.

(prog+ circle
  (clear-demo-buffer)
  (setf <radius> (+ 3 (random* 6)))
  [move: self (+ 10 (random* 50)) (+ 6 (random* 10))]
  (message "Area: %S" [area: self])
  [draw: self])

;; Execute that block several times while watching the demo buffer.

;; Now for a quick peek behind the scenes. This is what the above
;; `prog+' expands into:

(apply (function
	(lambda (self)
	 (clear-demo-buffer)
	 (set-slot-value :radius self (+ 3 (random* 6)))
	 (invoke-method :move self (+ 10 (random* 50)) (+ 6 (random* 10)))
	 (invoke-method :draw self)
	 (message "Area: %S" (invoke-method :area self))))
       circle
       nil)

;; Now we will make a new prototype based on our existing circle
;; object. This one will be labeled. 

(define-prototype labeled-circle
  (:parent circle
   :documentation "A labeled circle.")
  (label :initform "[circle]" :documentation "The label to be displayed."))

(define-method initialize labeled-circle ()
  (message "labeled-circle initialization")
  [circle:initialize: self])

(define-method draw labeled-circle ()
  (with-demo-buffer
    (let ((len (length <label>)))
      (artist-mode 1)
      (artist-draw-rect (- <x> (/ len 2))
			(- <y> 2)
			(+ <x> (/ len 2) 3)
			(+ 2 <y>))
      (goto-char (point-min))
      (artist-next-line (1- <y>))
      (artist-forward-char (1- <x>))
      (setf (buffer-substring (point) (+ len (point)))
	    <label>)))
  [circle:draw: self])

(defvar lc)
(setf lc (clone :labeled-circle))

(prog+ lc
  (clear-demo-buffer)
  (setf <radius> (+ 8 (random* 10)))
  [move: self (+ 10 (random* 50)) (+ 6 (random* 10))]
  (setf <label> (format "(%S,%S)" <x> <y>))
  [draw: self])

;;; TODO Background
;;; TODO Objects and prototypes
;;; TODO Slots and methods
;;; TODO Selectors and modules

;;; Printing an object

;; This is mainly for debugging purposes, and has nothing to do with
;; proper serialization.

(defun print-object (object)
  (let ((pointer (find-object object))
	(slots ()))
    (while pointer
      (setf slots (object-slots pointer))
      (while slots
	(let* ((slot (pop slots))
	       (value (pop slots)))	
	  (insert (format "  %S => %S\n" slot value))))
      (setf pointer (object-parent pointer)))))

(defun print-object-to-string (object)
  (with-temp-buffer
    (print-object object)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun print-object-to-viewer (object)
  (with-current-buffer (get-buffer-create "*object viewer*")
    (delete-region (point-min) (point-max))
    (print-object object)
    (display-buffer (current-buffer))))


(provide 'eon-guide)
;;; eon-guide.el ends here
