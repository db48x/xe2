;;; eon.el --- micro minimal object system for GNU Emacs Lisp

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: oop

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

;; TODO bring slots into scope <foo>
;; TODO define methods
;; TODO method invocation with [verb object args... ] 
;; TODO inclusion

;;; Code:

(require 'cl)

;;; Dealing with symbols

(defun make-keyword (name)
  (let ((string (etypecase name
		  (symbol (symbol-name name))
		  (string name))))
    (if (string-match "^:" string)
	(intern string)
	(intern (concat ":" string)))))

(defun delimited-symbol (name delimiter)
  (let ((symname (if (stringp name) 
		     name 
		     (symbol-name name))))
    ;; already delimited?
    (if (string-match (regexp-quote delimiter) symname)
	(intern symname)
	;; no, delimit it
	(intern (concat delimiter symname delimiter)))))

(defvar struct-symbol-delimiter "++")

(defun struct-symbol (class-name)
  (delimited-symbol class-name struct-symbol-delimiter))
  
(defvar class-symbol-delimiter "+")

(defun class-symbol (class-name)
  (delimited-symbol class-name class-symbol-delimiter))

(defvar constructor-symbol-delimiter "@")

(defun constructor-symbol (class-name)
  (delimited-symbol class-name constructor-symbol-delimiter))

;;; Class definitions

(defstruct class name constructor struct-name options slots methods)

(defun class-exists (class-name)
  (let ((symbol (class-symbol class-name)))
    (and (boundp symbol)
	 (class-p (symbol-value symbol)))))

(defun class-definition (class-name)
  (let ((symbol (class-symbol class-name)))
    (if (class-exists class-name)
	(symbol-value symbol)
	(error "No such class %s." class-name))))

(defun set-class-definition (class-name class-definition)
  (assert (class-p class-definition))
  (let ((symbol (class-symbol class-name)))
    (when (class-exists class-name)
      (message "Warning; redefinition of class %S" class-name))
    (setf (symbol-value symbol)
	  class-definition)))

(defsetf class-definition set-class-definition)

(defmacro define-class (name options &rest slots)
  (let* ((class-name (class-symbol name))
	 (struct-name (struct-symbol name))
	 (constructor-name (constructor-symbol name)))
    `(progn
       (defstruct (,struct-name (:constructor ,constructor-name))
	 ,@slots)
       (setf (class-definition ',class-name)
	     (make-class :name ',class-name
			 :struct-name ',struct-name
			 :constructor #',constructor-name
			 :options ',options
			 :slots ',slots)))))

;; (define-class baz () bar baz quux)

(provide 'eon)
;;; eon.el ends here
