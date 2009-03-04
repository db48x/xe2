;;; narration.lisp --- log and narrate gameplay

;; Copyright (C) 2009  David O'Toole

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

;;; Code:

(in-package :rlx)

;;; Narration widget

(defvar *default-message-verbosities*
  '(:move 3
    :move-cell 5
    :step 2
    :expend-action-points 3
    :expend-default-action-points 3
    :stat-effect 2
    :damage 1
    :narrate nil 
    :narrateln nil
    :print-object-tag nil
    :newline nil
    :print-separator nil))
    
(defvar *message-verbosities* *default-message-verbosities*)

(defun set-message-verbosities (plist &optional (include-default t))
  (setf *message-verbosities* (append plist 
				      (if include-default 
					  *default-message-verbosities*
					  nil))))

(define-prototype narrator (:parent =formatter=)
  (verbosity :initform 1 
	     :documentation "Integer between 1 and 3 (inclusive).")
  (excluded-actions :documentation 
"List of action keywords to be excluded from narration.
Usually it should contain at least :move."
		    :initform (list :step :move-cell :expend-action-points :expend-default-action-points
				    :move :narrate :narrateln :print-object-tag :newline :print-separator))
  (passive-voice-actions :documentation
"List of action words to use passive voice in narrating.
http://en.wikipedia.org/wiki/Passive_voice"
                         :initform nil))

(define-method set-verbosity narrator (&optional (value 1))
  (setf <verbosity> value))

(define-method narrate narrator (control-string &rest args)
  [print self 
	 (apply #'format nil control-string args)])

(define-method narrateln narrator (control-string &rest args)
  [println self 
	   (apply #'format nil control-string args)])

(define-method print-object-tag narrator (ob)
  [print-image self (field-value :tile ob)]
  [space self]
  [print self (let ((str (symbol-name (object-name (object-parent ob)))))
		(subseq str 1 (search "=" str :from-end t)))]
  [space self]
  (when (= 5 <verbosity>)
    [print self (object-address-string ob) :foreground ".gray50"]))

(define-method print-separator narrator ()
  [print self "  :  " :foreground ".gray20"])

(define-method narrate-message narrator (sender action receiver args &optional force)
  (let ((A (or sender rlx:=asterisk=))
	(B (if (has-field :tile receiver) 
	       receiver 
	       rlx:=gray-asterisk=))
	(action-verbosity (getf *message-verbosities* action t)))
    (when (member action <passive-voice-actions>)
      (rotatef A B))
    (when (or force
	      (and (not (null action-verbosity))
		   (or (eq t action-verbosity)
		       (and (numberp action-verbosity)
			    (>= <verbosity> action-verbosity)))))
      [print-object-tag self A]
      [print-separator self]
      [print-image self (icon-image action)]
      [space self]
      [print self (symbol-name action)
	     :foreground ".white" :background ".gray30"]
      [print-separator self]
      [print-object-tag self B]
      [print-separator self]
      ;; print args
      (dolist (arg args)
	[space self]
	(if (object-p arg)
	    [print-object-tag self arg]
	    [print self (format nil "~A" arg)]))
      [newline self])))



;;; narration.lisp ends here
