;;; pak.el --- resource data interchange format for elisp and CL

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: lisp, data, tools

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

;; TODO Read/write pak files
;; TODO Generate pak index of a directory?
;; TODO Emacs tree widget view of mod resources?
;; TODO import resources by making .pak files for existing media automatically

;;; Code:

;; See pak.lisp for more information.

(defstruct pak-resource 
  name type notes file data client-data)

(defun pak-resource-to-plist (res)
  (list :name (pak-resource-name res)
	:type (pak-resource-type res)
	:notes (pak-resource-notes res)
	:file (pak-resource-file res)
	:data (pak-resource-data res)))

(defun pak-write-sexp-to-file (filename sexp)
  (with-temp-buffer
    (insert (format "%S" sexp))
    (write-file filename)))

(defun pak-read-sexp-from-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (read (current-buffer))))

(defun pak-write (filename resources)
  (pak-write-sexp-to-file (mapcar #'pak-resource-to-plist resources) filename))

(defun pak-read (filename)
  (mapcar #'make-pak-resource (pak-read-sexp-from-file filename)))

(defun pak-read-table (filename) 
  (let ((table (make-hash-table :test 'equal)))
    (prog1 table
      (dolist (res (pak-read filename))
	(setf (gethash (pak-resource-name res) table)
	      res)))))

(defun pak-write-table (filename table)
  (let (resources)
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v resources))
	     table)
    (pak-write filename resources)))

(provide 'pak)
;;; pak.el ends here
