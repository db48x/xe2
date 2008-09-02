;;; ushell.el --- universal shell

;; Copyright (C) 2007, 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: lisp

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

;;; Commentary:

;; This is a Universal Shell inspired by the Symbolics Genera
;; operating system. In designing ushell I started with what I could
;; learn about Genera from the various manuals I was able to get a
;; hold of, and also watched a fascinating video (link below) of a
;; user playing with Genera. I'd like to extend it with a few ideas of
;; my own, and hopefully the result will be something unique.

;; You can see a 20+ minute video of Genera at
;; http://www.lemonodor.com/archives/misc-gems/lispm.mov.gz My first
;; thought after seeing it was "I just saw the Emacs of the future."

;; Some old Genera manuals can be found at
;; http://www.andreas.org/genera/Programming%20the%20User%20Interface.pdf

;; Features of ushell:

;;  - A mega-extensible self-documenting command-line interface for
;;    absolutely everything---Emacs Lisp, Common Lisp, UNIX
;;    programs/shells, etc. inspired by Symbolics Genera

;;  - Comprehensive mouse support

;;  - Generic data presentation UI makes different interfaces possible

;;  - Interactively construct calls to any function/command/UNIX
;;    program/whatever, by choosing arguments with keyboard/mouse

;;  - Command output can be plain text like a normal shell, or
;;    clickable text (such as a pop-up menu for every file that
;;    appears in a directory listing), or a picture, or an interactive
;;    dialog box with options and a "go" button. You can save the
;;    entire log as a sort of history, and save commonly-used forms
;;    for later re-use.

;;  - Integrated event notification and logging system.

;;  - Undoubtedly boatloads of other stuff.

;;; Code:

(require 'eon)

;;; Presentation types


 (defmacro* define-presentation-type (type-name data-arglist &key
				     parser printer
				     viewspec-choices description
				     describer no-deftype history
				     expander abbreviation-for
				     choose-displayer
				     accept-values-displayer
				     menu-displayer
				     default-preprocessor
				     history-postprocessor
				     highlighting-box-function
				     presentation-type-arguments
				     typep)
;;   "Define a new presentation type.

;; TYPE-NAME is the name of the new type.

;; DATA-ARGLIST is a Common Lisp-style argument list.
;; TODO describe what it does

;; The following keywords are valid in PRESENTATION-ARGS:

;;   :PRINTER         Function to print an object of this type.
;;   :PARSER          Function to read an object of this type.
;;                    :DEFAULT  
;;                    :DEFAULT-SUPPLIED
;;                    :DEFAULT-TYPE
;;                    :INITIALLY-DISPLAY-POSSIBILITIES
;;   :DESCRIBER       Function that describes an object of this type.
;;   :DESCRIPTION    A string describing the type, if :DESCRIBER is not supplied.
;;   :DEFAULT-PREPROCESSOR TODO
;;   :CHOOSE-DISPLAYER TODO
;;   :MENU-DISPLAYER TODO 
;;   :ACCEPT-VALUES-DISPLAYER TODO
;;   :VIEWSPEC-CHOICE TODO
;;   :PRESENTATION-SUBTYPEP TODO      
;;   :TYPEP TODO
;;   :NO-DEFTYPE TODO
;;   :HISTORY TODO
;;   :PRESENTATION-TYPE-ARGUMENTS TODO
;;   :DATA-ARGUMENTS-ARE-DISJOINT TODO
;;   :DO-COMPILER-WARNINGS TODO
;; ")
				     
;;;; Extended argument lists

;; An "extended argument list" is like an ordinary CL argument list,
;; but with each argument name `arg' replaced by a triple:

;;   (ARG PRESENTATION-TYPE OPTIONS)

;; These triples may be arranged in the extended argument list just as
;; in a `destructuring-bind', i.e. `&optional', `&key', and all the
;; other destructuring features:

;; ((POSITIONAL-ARG1 TYPE OPTIONS) (POSITIONAL-ARG2 TYPE OPTIONS)
;;  &KEY (KEYWORD-ARG1 TYPE OPTIONS) (KEYWORD-ARG2 TYPE OPTIONS))

;; ARG is the argument name (a symbol).
;; PRESENTATION-TYPE is evaluated.
;; In OPTIONS the following keywords are valid:

;;   :PROMPT    A string (or a form evaluating to a string) used as the
;;              prompt for this argument.

;;   :PROMPT-MODE   :raw means that prompt is just printed.
;;                  :normal (the default) specifies standard reformatting:
;;                
;;                        Command Name (type1) :  <---- bright red input star
;;                                (type2 [default: foo) ...
;;                                (keywords) :Keyword Name (type3)

;;   :DEFAULT   The default value for the argument. With no default,
;;              the presentation history is consulted for a value.

;;   :DEFAULT-TYPE   The presentation type of the argument value. Use
;;                   this with :default when the default value could
;;                   be interpreted more than one way.

;;   :PROVIDE-DEFAULT  When non-nil, the above options relating to
;;                     defaults are activated.

;;   :DISPLAY-DEFAULT   When non-nil, the default is printed in the
;;                      prompt. Default is t.

;;   :DOCUMENTATION     The documentation string.

;;   :NAME   User-visible name of the argument. The default is "Foo Bar
;;           Baz" for the command `foo-bar-baz'.

;;   :CONFIRM   When non-nil,

;;   :WHEN Only read the value if this predicate-form returns non-nil.
;;   :MENTIONED-DEFAULT Wha..?

;;;; Presenting Lisp objects

;; (defun* present (object &optional (presentation-type (type-of object))
;; 			&rest options 
;; 			&key (acceptably t)
			

;;   "Make a presentation of the OBJECT.")

;; (defun* present-to-string (object (presentation-type (type-of object))
;; 				  &rest options
;; 				  &key acceptably index string)
				  
;;     "Present OBJECT as an output string that can be read back by `accept'.")

;;(defmacro* with-output-as-presentation
;; with-character-style
;; surrounding-output-with-border
;; with-output-as-presentation

;;;; Accepting Lisp objects

;; (defun* accept (presentation-type)
;;   "Prompt the user for an object of PRESENTATION-TYPE, and return
;; the object (if any.)")

;; (defun* prompt-and-accept

;; (defun* accept-from-string (string presentation-type)

;; (defun* menu-choose

;; (defun* menu-choose-from-set

;; (defun* accept-values

;; (defun* accept-variable-values

;; (defun* accepting-values

;; Examples: 

;; (present 8 'integer)
;; (present t 'boolean)
;; (accept '((integer 0 64) :base 8))
;; (accept '(member foo bar baz))
;; (accept '(and integer (satisfies evenp)))

;;;; Commands and command tables

;; This program implements its own (backward compatible) interactive
;; command processing and keybinding framework. Eon commands are to
;; methods as interactive functions are to ordinary functions, and
;; command tables are a lot like Emacs' keymaps. But Eon's command
;; system is different in several ways:

;;  - Command tables can inherit keybindings from multiple other
;;    tables

;;  - Command tables can be mixed and matched freely to create a
;;    control scheme; not strictly tied to major/minor mode.

;;  - Commands hook into the presentation system, enabling the
;;    construction of a universal command shell (i.e. ushell.el).

;;  - Unlike methods, commands are not attached to objects.

(defvar *command-tables* (make-hash-table :test 'equal) 
  "A hash table mapping strings (names of command tables) to the
tables themselves.")

(make-variable-buffer-local
 (defvar *command-table* "The default command-table for this buffer."))

(make-variable-buffer-local
 (defvar *last-command-value* "Value returned by the most recent command in this buffer."))

(defun find-command-table (x)
  (etypecase x
    (object x)
    (string (gethash x *command-tables*))))

(defproto+ command-table "A set of available commands with optional accelerators."
  name ;; string name of command table 
  commands ;; hashtable mapping string command names to command objects TODO
  inherit-from ;; list of parent tables
  accelerators ; hashtable mapping Emacs input events to command names
  keymap ;; emacs keymap for input events
  )

(defmethod+ initialize command-table (name &optional inherit-from)
  (setf @name name)
  (setf @inherit-from inherit-from)
  (puthash name self *command-tables*)
  (setf @keymap (make-sparse-keymap)))

(defmethod+ find-command-for-event command-table (event)
  (or (gethash event @accelerators)
      (some (lambda (table)
	      (>> :find-command 
		  (find-command-table table)
		  event))
	    @inherit-from)))

(defmethod+ execute-command command-table (command &rest args)
  (apply (gethash command @commands) args))
	    
(defmethod+ do-event-command command-table (event)
  (let ((c (>>find-command-for-event event)))
    (if c
	(>>execute-command c args)
	(error "No command found for event %S" event))))
  
(defmethod+ install-command command-table (command-name command-defun-symbol) 
  (puthash command-name @commands command-defun-symbol))

(defmethod+ install-accelerator command-table (command-name event)
  (puthash event command-name @accelerators)
  (define-key @keymap event 
    (eon-command-defun-symbol (normalized-symbol command-name))))

(defmethod+ delete-command command-table (command)
  (remhash command-name @commands))
   	    ;; TODO remove keybinding

(defun delete-command-table (name)
  (remhash name *command-tables*))

;;; (defun eon-default-command-name (ob)
;;;   "Return the user-visible command name of the string in the default style."
;;;    (replace-regexp-in-string "-" " " 
;;; 			     (upcase-initials (etypecase ob
;;; 						(symbol (normalized-symbol

(defun eon-command-defun-symbol (symbol)
  (join-symbols "command" ":" nil (normalized-symbol symbol)))

(defmacro* defcommand+ (name-and-options arguments &rest body)
  "Define a new command.  Commands are like methods, but not
attached to objects. Commands are also like interactive functions
in Emacs Lisp, but their command-table keybindings don't have to
depend on the current major/minor mode.

NAME-AND-OPTIONS is either a symbol naming the command, or a list
of the form:
nnnn
 (NAME :KEY1 VALUE1 :KEY2 VALUE2 ...)

If this second form is used, the following keywords are valid:

 :NAME  A string presented to the user as the name of the command.
        The default is \"Foo Bar\" for symbol `foo-bar'. 
 :COMMAND-TABLE  String or symbol name of the command table where
                 this command will be installed. 
 :PROVIDE-OUTPUT-DESTINATION-KEYWORD  Whether the command should
                 accept the :output-destination modifier.
                 The default is t.
 :VALUES  Whether this command returns values; default is nil.

ARGUMENTS is an extended argument list describing the arguments of
the command; see `eon-parse-extended-argument-list'."
  (declare (indent 2))
  (let ((defun-symbol (eon-command-defun-symbol name)))
    (prog1 defun-symbol
      `(let (name options)
	 (etypecase name-and-options
	   (list (setf name (eon-default-command-name (first name-and-options))
		       options (rest name-and-options)))
	   (symbol (setf name name-and-options)))
	 (defun* ,defun-symbol arguments 
	   ;; store the return value too
	   (setf *last-command-value* (progn ,@body)))
	 ;; register it in a command table if neccessary
	 (let ((table (getf options :command-table)))
	   (when table
	     (>> :install-command table name ,defun-symbol)))))))
      
;;; (defmacro define-command-accelerator (name command-table
;;; 				      characters options arguments &rest body)
  ;; (defun execute-command (command &rest arguments)


;;;; Predefined command tables

;; TODO Subvert/wrap the emacs command system
;; Colon Full Command
;; Standard Scrolling
;; Standard Arguments
;; Unshifted Arguments
;; Marked Text
;; Global
;; User

;;;; Command accelerators

;; (defmacro define-command-accelerator 

;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;;;; Verbs

;;;;; Nouns

;;;;; Modifiers

;;;; Executing commands

;;;; Reading input

;;;; Command prompt

;;;; Formatting command names

;;;; Interactive choosing of nouns, verbs, and modifiers

;;;; History and undo

;;;; Mouse support

;;;; Read-Execute-Display Loop

;;;; Evaluating Lisp forms at the prompt
    

;;; Major mode definition

;;; Buttonizing the output of existing programs

;;; defpackage+

;; (defvar +packages+ (make-hash-table :test 'equal)
;;   "Hash table mapping keyword symbols to property lists.")

;; (defvar +package+ nil "Keyword symbol naming current package.")

;; (defmacro* defpackage+ (package version-string
;; 				&rest options 
;; 				&key documentation 
;; 				requirements
;; 				export
;; 				&aux (version version-string))
;;   `(prog1 package 
;;      (puthash package options +packages+)
;;      (define-package ,(normalized-symbol-name package)
;; 	 ,version-string ,documentation ,requirements)))

;; (defmacro* in-package+ (package)
;;   `(eval-when-compile 
;;      (when (gethash package +packages+)
;;        (setf +package+ package))))

;;; defvar+

;; TODO automatic defcustom writing for general CL arglists
;; TODO clamping?
;; TODO Error handling?
;; TODO configurable undo-history?
;; TODO aliases/obsolete variables
;; (defmacro* defvar+ (var &optional init-form docstring
;; 			&rest options 
;; 			&key (init-time :now)
;; 			update-form 
;; 			(scope :global)
;; 			safety 
;; 			type
;; 			value-predicate)
;;   "Define VAR as a variable, and return VAR.

;; With no OPTIONS specified, acts just like `defvar'.

;; Otherwise, VAR is bound to a function that checks whether the
;; variable has been initialized, initializes it if necessary, and
;; then returns the value of the variable. 

;; Then you should replace references to VAR with (VAR),
;; including cases like:

;;  (setf (VAR) some-value)

;; to gain additional control over the initialization and updating
;; of the variable.

;; If INIT-TIME is `:lazy', the variable is not initialized until
;; the first call to (VAR). This is useful when the variable's
;; intended value is some resource (a network process or a large
;; completion table) that you don't want to compute until it is
;; really needed. 

;; The default value of INIT-TIME is `:now', which acts like
;; ordinary `defvar' with respect to initialization.

;; If you have specified an UPDATE-FORM, `setf' will evaluate it
;; after each update. Use this when other data need to be updated
;; each time this variable is set.

;; If you provide a VALUE-PREDICATE, the variable will be updated if
;; and only if the vew value satisfies the
;; predicate. (i.e. `stringp', `oddp', or perhaps:

;;  (lambda (x) (typep '(integer 0 255)))

;; SCOPE may be :global (the default), :buffer, :frame.

;; Example:

;;  (defvar+ myvar (prog1 \"Initial value of myvar.\" 
;;                        (message \"Initialized myvar.\"))
;;    \"Must always be a string.\"
;;    :init-time :lazy
;;    :value-predicate #'stringp
;;    :scope :buffer
;;    :update-form (message \"Updated myvar.\"))

;;    (myvar)
;;    (setf (myvar) \"foo\")
;;    (myvar)
;;    (setf (myvar) nil) 
;;    (myvar)
;; "
;;   (if (null options)
;;       `(defvar ,var ,init-form ,docstring)
;;     (let ((scope-modifier (ecase scope
;; 			    (:global #'identity)
;; 			    (:buffer #'make-variable-buffer-local)
;; 			    (:frame #'make-variable-frame-local))))
;;     `(lexical-let ((predicate ,value-predicate)
;; 		   var-flag)
;;        (labels ((update (v)
;; 			 (if (funcall predicate v)
;; 			   (prog1 v
;; 			     (setf ,var v)
;; 			     (setf var-flag t)
;; 			     ,update-form)
;; 			   ,var)))
;; 	 (,scope-modifier (defvar ,var nil ,docstring))
;; 	 (defun ,var ()
;; 	   (if var-flag
;; 	       ,var
;; 	     (update ,init-form)))
;; 	 (defsetf ,var update)
;; 	 (when (eq ,init-time :now)
;; 	   (setf ,var ,init-form)))
;;        ',var))))

;;; Workspaces

;;; Persistent Lisp data stores: the savef macro inverts expressions into object creator scripts



;; TODO port this from common lisp
;; TODO review cmu codewalker stuff

;; (defun fold-model (model)
;;   "Turn a model with many duplicate (but equal) sublists into a
;; model with a table of objects and references between
;; them. Returns a hash table mapping sexps to integers, and the
;; transformed model."
;;   (let ((sexps->integers (make-hash-table :test 'equal))
;; 	(id 0)
;; 	(m (copy-tree model)))
;;     (labels ((fold-sexp (L)
;; 	       (let* ((sexp (car L))
;; 		      (sexp-id nil))
;; 		 ;;
;; 		 ;; don't match keywords or already-substituted references
;; 		 (when (and (listp sexp)
;; 			    (not (null sexp))
;; 			    (not (equal 'folded-reference (car sexp))))
;; 		   (fold-sexp sexp)
;; 		   (if (setf sexp-id (gethash sexp sexps->integers))
;; 		       (nsubst `(folded-reference ,sexp-id) sexp L :test 'equal)
;; 		       ;;
;; 		       ;; it's not in the hashtable. put it in
;; 		       (progn 
;; 			 (incf id)
;; 			 (setf (gethash sexp sexps->integers) id))))			
;; 		 ;;
;; 		 (when (not (null L))
;; 		   (fold-sexp (cdr L))))))
;;       (fold-sexp m)
;;       ;;
;;       ;; give the object table a "root object"
;;       (setf (gethash m sexps->integers) 0)
;;       ;;
;;       ;; now fold sexps that are in the hash table already
;;       (let ((new-sexps (make-hash-table :test 'equal)))
;; 	;;
;; 	;; first make a copy; we can't modify a hash table while iterating over it
;; 	(maphash (lambda (k v)
;; 		   (setf (gethash k new-sexps) v))
;; 		 sexps->integers)
;; 	;;
;; 	;; now fold the keys of the copy while modifying the original 
;; 	(maphash (lambda (k v)
;; 		   (fold-sexp k))
;; 		 new-sexps))
;;       ;;
;;       ;; return the mapping and the folded model
;;       (values sexps->integers m))))
      
;; (defun serialize-model (model)
;;   "Serialize a model into a set of sexps suitable for writing to a text file."
;;   (multiple-value-bind (sexp-hash folded-model) (fold-model model)
;;     (let ((sexps nil))
;;       (maphash (lambda (k v)
;; 		 (push (cons v k) sexps))
;; 	       sexp-hash)
;;       (sort sexps (lambda (x y)
;; 			(> (car x) (car y))))
;;       (nreverse sexps))))
	      
;; (defun write-model (model filename)
;;   "Write a model to disk."
;;   (with-open-file (file filename :direction :output 
;; 			:if-exists :overwrite
;; 			:if-does-not-exist :create)
;;     (format t "~S" model)
;;     (format file "~S" model)))

;; ;; TODO convert recursion to iteration below

;; (defun load-worksheet (filename)
;;   "Construct a worksheet from a file."
;;   (let* ((model (read-model filename))
;; 	 (integers->sexps (make-hash-table :test 'eql))
;; 	 (integers->objects (make-hash-table :test 'eql))
;; 	 (worksheet-model nil))
;;     ;;
;;     ;; grab the worksheet object, which is always first
;;     (setf worksheet-model (car model))
;;     ;;
;;     ;; read in all the sexps
;;     (dolist (m model)
;;       (setf (gethash (car m) integers->sexps) (cdr m)))
;;     ;;
;;     ;; now expand the sexps into objects by recursively unfolding all references
;;     ;;
;;     (labels ((remove-class-keywords (plist)
;;  	       ;; TODO ditch this
;;  	       ;; we do this because it makes it easier to
;;  	       ;; pass the plist to make-instance during
;;  	       ;; the unmodeling process.
;;  	       (let ((plist1 plist)
;;  		     (plist2 nil))
;;  		 (do ((p (pop plist1) (pop plist1)))
;;  		     ((null plist1))
;;  		   (if (equal :class p)
;;  		       (pop plist1) ; skip value after keyword
;;  		       (push p plist2)))
;;  		 ;;
;;  		 ;; handle the last element
;;  		 (push (car (last plist)) plist2)
;;  		 (prog1 
;;  		     (reverse plist2))))
;; 	     ;;
;; 	     (unmodel-object (plist)
;; 	       (let* ((plist2 (remove-class-keywords plist))
;; 		      (object-class (getf plist :class))
;; 		      (object (apply #'make-instance object-class plist2)))
;; 		 (unmodel object)
;; 		 object))
;; 	     ;;
;; 	     (expand (sexp)
;; 	       (if (not (listp sexp))
;; 		   sexp
;; 		   ;;
;; 		   ;; what type of list? 
;; 		   (cond
;; 		     ;;
;; 		     ;; a folded reference? 
;; 		     ((equal 'folded-reference (car sexp))
;; 		      (let* ((reference-number (car (cdr sexp)))
;; 			     (object (gethash reference-number integers->objects)))
;; 			;;
;; 			;; if already in object cache, return it.
;; 			;; otherwise, put it in
;; 			(if object
;; 			    object
;; 			    ;;
;; 			    ;; time to make the donuts!
;; 			    (let ((reference-sexp 
;; 				   (gethash reference-number integers->sexps)))
;; 			      (setf (gethash reference-number integers->objects)
;; 				    (expand reference-sexp))))))
;; 		     ;;
;; 		     ;; a modeled object?
;; 		     ((listp sexp)
;; 		      ;;
;; 		      ;; expand all subforms
;; 		      (setf sexp (mapcar #'expand sexp))
;; 		      ;;
;; 		      ;; create object when ready
;; 		      (if (equal :class (car sexp))
;; 			  (unmodel-object sexp)
;; 			  ;; otherwise just return sexp
;; 			  sexp))))))
;;       ;;
;;       ;;
;;       (values 
;;        (expand (gethash 0 integers->sexps))
;;        integers->objects integers->sexps))))

;; (defproto+ ushell "A universal shell."
;;   buffer
;;   history)

;; (defvar ushell-default-buffer-name "*ushell*")

;; (defvar ushell-prompt-string ">> ")

;; (defmethod+ insert ushell (&rest args)
;;   (with-current-buffer <buffer>
;;     (apply #'insert args)))

;; (defmethod+ prompt ushell ()
;;   [insert: self ushell-prompt-string])

;; (defmethod+ initialize ushell ()
;;   (setf <buffer> (get-buffer-create ushell-default-buffer-name))
;;   [insert: self "USHELL v0.01a\n"]
;;   [prompt: self])

;; (define-derived-mode ushell-mode nil 
;;   "Ushell:" "A universal shell mode.")

;; (defvar ushell-mode-map nil)
;; (when (null ushell-mode-map)
;;   (setq ushell-mode-map (make-sparse-keymap))
;;   (define-key ushell-mode-map 

(provide 'ushell)
;;; ushell.el ends here
