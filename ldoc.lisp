;;; ldoc.lisp --- extract and format documentation from lisp files

;; Copyright (C) 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: lisp, tools

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

(defun clon-prototype-p (form)
  (when (symbolp form)
    (let* ((name (symbol-name form))
	   (len (length name)))
      (and (string= "=" (subseq name 0 1))
	   (string= "=" (subseq name (- len 1) len))))))

(defun clon-method-p (form)
  (when (symbolp form)
    (let* ((delimiter ">>")
	   (name (symbol-name form))
	   (len (length name))
	   (delimiter-pos (search delimiter name)))
      (when (numberp delimiter-pos)
	(values (subseq name 0 delimiter-pos)
		(subseq name (+ 2 delimiter-pos)))))))
    
(defun remove-delimiters (form)
  (let* ((name (symbol-name form))
	 (len (length name)))
    (subseq name 1 (- len 1))))

(defun document-symbol (symbol stream)
  "Documentation string."
  (let* ((type (if (clon-prototype-p symbol)
		   'variable
		   (if (fboundp symbol) 
		       (if (macro-function symbol) 
			   'function 
			   'function)
		       'variable)))
	 (type-name (if (clon-method-p symbol)
			"method" (if (clon-prototype-p symbol)
				     "prototype"
				     (if (fboundp symbol)
					 (if (macro-function symbol)
					     "macro" "function")
					 "variable"))))
	 (doc (if (clon-prototype-p symbol)
		  (field-value :documentation (symbol-value symbol))
		  (documentation symbol type)))
	 (field-descriptors (when (clon-prototype-p symbol)
			      (field-value :field-descriptors (symbol-value symbol))))
	 (name (if (clon-prototype-p symbol)
		   (remove-delimiters symbol)
		   (if (clon-method-p symbol)
		       (multiple-value-bind (method-name prototype-name) (clon-method-p symbol)
			 (format nil "~A // ~A" method-name prototype-name))
		       (symbol-name symbol))))
	 (args (when (fboundp symbol) (sb-introspect:function-arglist (fdefinition symbol)))))
    (format stream "** ~A (~A)" name type-name)
    (fresh-line stream)
    (when args
      (format stream "*** Arguments")
      (fresh-line stream)
      (format stream "~A" args)
      (fresh-line stream))
    (when doc
      (format stream "*** Documentation")
      (fresh-line stream)
      (format stream "~A" doc))
    (when field-descriptors
      (format stream "*** Fields")
      (fresh-line stream)
      (dolist (d field-descriptors)
	(fresh-line stream)
	(destructuring-bind (name (&key documentation initform &allow-other-keys)) d
	  (when name (format stream "**** ~S (field)" name))
	  (when documentation 
	    (format stream "***** Documentation")
	    (fresh-line stream)
	    (format stream "~A" documentation)
	    (when initform 
	      (format stream "***** Initialization form")
	      (fresh-line stream)
	      (format stream "~S" initform))))))
    (fresh-line stream)))

(defun do-heading (name stream)
  (fresh-line stream)
  (format stream "* ~A" name)
  (fresh-line stream))

(defun document-package (package-name stream)
  (let (syms protos methods proto-hashes)
    (do-external-symbols (sym package-name)
      (when (< 3 (length (symbol-name sym)))
	(push sym syms)))
    (setf syms (sort syms #'string<))
    (format stream "#+OPTIONS: toc:t *:nil")
    (fresh-line stream)
    (format stream "#+TITLE: DOCUMENTATION FOR PACKAGE ~S" package-name) 
    (fresh-line stream)
    ;; sort symbols
    (setf syms (remove-if #'(lambda (s)
    			       (when (clon-prototype-p s)
    				 (push s protos)))
    			   syms))
    (setf syms (remove-if #'(lambda (s)
    			       (when (clon-method-p s)
    				 (push s methods)))
    			   syms))
    ;; document prototypes
    (dolist (p protos)
      (let (pile)
	(dolist (m methods)
	  (multiple-value-bind (method-name proto-name) 
	      (clon-method-p m)
	    (when (string= proto-name (symbol-name p))
	      (push m pile))))
	(setf pile (sort pile #'(lambda (s z)
				  (multiple-value-bind (method-name1 proto-name1)
				      (clon-method-p s)
				    (multiple-value-bind (method-name2 proto-name2)
					(clon-method-p z)
				      (string< method-name1 method-name2))))))
	(when pile
	  (do-heading (symbol-name p) stream)
	  (dolist (p pile)
	    (document-symbol p stream)))))
    ;; document syms
    (dolist (sym syms)
      (document-symbol sym stream))))


(defun document-package-to-file (package-name output-file)
  (with-open-file (stream output-file :direction :output :if-exists :supersede)
    (document-package package-name stream)))
			    
;; (document-package :clon t)
;; (document-package-to-file :xe2 #P"/home/dto/notebook/xe2-reference.org")

;;; ldoc.lisp ends here
