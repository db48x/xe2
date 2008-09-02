;;; ligeti.el --- process-music composition system for csound

;; Copyright (C) 2007  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: multimedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'cl)
(require 'csound-x)

;; (@* "Initialization")

(defun ligeti-init ()
  (setf ligeti-activity-generators (make-hash-table :test 'equal))
  ;;
  (setf ligeti-activities (make-hash-table :test 'equal))
  (setf ligeti-next-activity-number 1)
  ;;
  (setf ligeti-samples (make-hash-table :test 'equal))
  (setf ligeti-next-sample-number 1)
  ;;
  (setf ligeti-sequences (make-hash-table :test 'equal))
  (setf ligeti-next-sequence-number 1))

;; (@* "Samples")
;;
;; A sample is a string identifying a stored sound (typically in a
;; .wav file)

(defvar ligeti-sample-extension "wav" "Files with this extension
are considered samples.")

(defvar ligeti-samples nil "Hash-table mapping samples to
integers.  Because function tables are numbered in csound, we
need to keep track of which samples go with which table numbers.")

(defvar ligeti-next-sample-number 1)

(defun ligeti-load-sample (sample)
  "Load the sample SAMPLE from the current project. Return the assigned sample number."
  ;;
  ;; create hash table if necessary
  (when (null ligeti-samples)
    (setf ligeti-samples (make-hash-table :test 'equal)))
  (setf (gethash sample ligeti-samples) ligeti-next-sample-number)
  (prog1 ligeti-next-sample-number 
    (incf ligeti-next-sample-number)))

(defun ligeti-sample-number (sample)
  "Get the function-table number from the sample named SAMPLE. If
SAMPLE has not yet been assigned a number, assign it."
  (let ((number (gethash sample ligeti-samples)))
    (if (numberp number)
	number
      (ligeti-load-sample sample))))

(defun ligeti-sample-file (sample)
  (expand-file-name (format "%s.%s" sample ligeti-sample-extension) 
		    (ligeti-project-directory ligeti-current-project)))
      
(defun ligeti-samples-in-project (project)
  (mapcar 'file-name-sans-extension 
	  (directory-files (ligeti-project-directory ligeti-current-project)
			   nil (concat ".*\\." ligeti-sample-extension "$"))))

(defun ligeti-load-all-samples (project)
  (setf ligeti-next-sample-number 1)
  (setf ligeti-samples nil)
  (mapcar 'ligeti-load-sample (ligeti-samples-in-project project)))

;; (@* "Projects")
;;
;; A project is a directory full of files related to a single musical
;; work. The files may be ligeti sequences, sound samples, csound
;; renderings, or any other required data. Only one project is
;; selected at a time.

(defvar ligeti-directory "~/ligeti" "Directory where ligeti
projects are saved and loaded.")

(defvar ligeti-current-project nil "Name of current project.")

(defun ligeti-project-directory (project)
  (expand-file-name project (file-name-as-directory ligeti-directory)))

(defun ligeti-project-file (file)
  (expand-file-name file (ligeti-project-directory ligeti-current-project)))

(defun ligeti-project-config (project)
  (expand-file-name (concat project ".ligeti") 
		    (ligeti-project-directory ligeti-current-project)))

(defun ligeti-find-project (&optional project)
  "Load PROJECT from the ligeti-directory.
If no such project exists, create it."
  (interactive)
  (when (null project) 
    (setf project (completing-read "Find ligeti project: " 
				   (directory-files ligeti-directory)
				   (lambda (p) (not (string-match "^\\..*" p)))
				   nil)))
  (ligeti-init)
  (let ((project-dir (ligeti-project-directory project)))
    (if (file-exists-p project-dir)
	(if (file-directory-p project-dir)
	    (progn 
	      (ligeti-load-all-samples project)
	      ;; FIXME: load sequences
	      )
	  (error "File exists, but is not a directory."))
      ;;
      ;; create new project
      (make-directory project-dir :parents)
      (message "Created new ligeti project in %s" project-dir)))
  ;;
  ;; now select the project
  (message "Loaded project %s" project)
  (setf ligeti-current-project project))
            		     
;; (@* "Activity generators")

(defvar ligeti-activity-generators nil "Hash table mapping
activity generator names to activity generators.")

(defmacro define-ligeti-activity (name activity-params event-params docstring &rest body)
  "Define a new activity generator. Calls are of the form

   (defactivity NAME ACTIVITY-PARAMS EVENT-PARAMS DOCSTRING BODY...)

where ACTIVITY-PARAMS and EVENT-PARAMS are of the form

   ((:param1 :tooltip TOOLTIP-STRING :docstring DOCSTRING
                       :column-header HEADER-STRING)
    (:param2 PLIST...)
    ...)

The ACTIVITY-PARAMS are arguments to the activity generator
function. They may alter the generated activity. The EVENT-PARAMS
are used during performance."  
  (labels ((build-param-list (specs) 
			     `(&key 
			       ,@(mapcar (lambda (s)
					   (intern (substring (symbol-name s) 1)))
					 (mapcar 'car specs))))
	   (format-param-docs (specs)
			      (concat "\n\nParameters: \n"
				      (mapconcat (lambda (spec)
						   (destructuring-bind 
						       (&key tooltip docstring &allow-other-keys) 
						       (cdr spec)
						     (format " %S ---  %s\n%s" 
							     (car spec)
							     tooltip docstring)))
						 specs
						 "\n"))))
    (let ((function-name (intern (concat "ligeti::" (symbol-name name)))))
      `(progn
	 (defun* ,function-name
	     ,(build-param-list activity-params)
	   ,(format-param-docs activity-params)
	   ,@body)
	 (when (null ligeti-activity-generators)
	   (setf ligeti-activity-generators (make-hash-table :test 'equal)))
	 (puthash ,(symbol-name name) 
		  (list ',function-name ',event-params)
		  ligeti-activity-generators)))))
  
;; (@* "Activities")

(defstruct ligeti-activity instrument-number instrument-spec event-params)
  
(defvar ligeti-activities nil "Hash table mapping activity names to activities.")

(defvar ligeti-next-activity-number 1)

(defun ligeti-make-activity (name generator-name &rest activity-params)
  (let ((entry (gethash generator-name ligeti-activity-generators)))
    (if entry 
	(destructuring-bind (generator event-params) entry 
	  (puthash name
		   (make-ligeti-activity :instrument-spec (apply generator activity-params)
					 :instrument-number ligeti-next-activity-number
					 :event-params event-params)
		   ligeti-activities)
	  (prog1 ligeti-next-activity-number
	    (incf ligeti-next-activity-number)))
      (error "no such activity generator: %s" generator-name))))
	
;; (@* "Events")
;; 
;; For now, events are lists of the form (start-time duration p4 p5 p6
;; ...)  The start-time comes from the cell-mode row number.  The
;; duration is determined by the number of rows between the event and
;; the next event (or the next noteoff.)

(defun ligeti-make-event (start-time duration &rest p-fields)
  "Create an event. This is trivial now but the format may change in the future."
  (append (list instrument-number start-time duration) p-fields))

;; (@* "Sequences")
;; 
;; Sequences gather a bundle of events together to be sent to a
;; particular activity.

(defstruct ligeti-sequence name activity event-params events)

(defvar ligeti-sequences nil "Hash table mapping sequence names to sequences.")

(defun ligeti-sequence-file (sequence-name)
  (ligeti-project-file (concat sequence-name ".ligeti.cell")))

(defun ligeti-make-sequence (&rest args)
  (let* ((sequence (apply 'make-ligeti-sequence args))
	 (sequence-name (ligeti-sequence-name sequence))
	 (sequence-file (ligeti-sequence-file sequence-name)))
    (puthash sequence-name sequence ligeti-sequences)
    (with-temp-buffer 
      (linkd-insert-datablock-template (ligeti-sequence-activity sequence))
      (write-region (point-min) (point-max) sequence-file)
    (find-file sequence-file))))

;; (@* "Sequencer UI")

(define-cell-type ligeti
  "Cell type for ligeti sequence data."
  (:beep (beep))
  (setf outlet inlet)
  (setf label (prin1-to-string inlet)))

(define-cell-mini-mode ligeti 
  "Mode for editing Ligeti event sequences."
  `((,(kbd "[") . ligeti-cell-decrement)
    (,(kbd "]") . ligeti-cell-increment)
    (,(kbd "{") . ligeti-cell-big-decrement)
    (,(kbd "}") . ligeti-cell-big-increment)
    (,(kbd "RET") . ligeti-cell-edit-value))
  ;;
  ;; startup code. decide what to do with the object
  (cond ((listp object)
	 ;;
	 ;; it's a plist
	 (cell-sheet-from-plist object))
	;;
	;; it's an activity name
	((stringp object)
	 (ligeti-make-sequence-sheet object))
	(t (error "Object not an activity name or existing sheet."))))

(defvar ligeti-default-sequence-length 32)

(defun ligeti-make-sequence-sheet (activity-name)
  (let ((activity (gethash activity-name ligeti-activities)))
    (when (null activity)
      (error "No such activity: %s" activity-name))
    (let ((params (ligeti-activity-event-params activity)))
      (make-cell-sheet :grid (make-grid ligeti-default-sequence-length
					(length params))
		       :properties (list :headers t :ligeti-activity activity-name)
		       :column-properties params
		       :column-stops (make-vector (+ 1 (length params)) 0)
		       :cursor '(0 0)))))
    	  
;; (@* "Rendering to csound")

(defun ligeti-to-csound ()
  (interactive)
  (let (sexps)
    (labels ((--> (&rest S) (mapcar (lambda (s)
				      (push s sexps))
				    S)))
      ;;
      ;; activities --> instruments
      (maphash (lambda (name spec)
		 (destructuring-bind 
		     (&key instrument-spec instrument-number &allow-other-keys) spec
		   (--> :instr instrument-number `(apply 'list ',instrument-spec))))
	       ligeti-activities)
      ;;
      ;; samples --> ftables
      (--> :score)
      (maphash (lambda (sample table-number)
		 (--> `(insert ,(format "f %d 0 0 1 \"%s\" 0 0 0\n" 
					table-number (ligeti-sample-file sample)))))
	       ligeti-samples))
    ;;
    ;; put it all together
    (cons 'csound-composition (nreverse sexps))))



		     


(provide 'ligeti)
;;; ligeti.el ends here
