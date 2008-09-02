;;; clon.lisp --- Common Lisp Object Network

;; Copyright (C) 2007, 2008  David O'Toole
;;
;; Author: David O'Toole <dto@gnu.org>
;; Keywords: oop
;; Version: 1.207
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; CLON is a simple prototype-based object system for Common Lisp. In
;; this alternative view of object-orientation, there are no classes;
;; instead, objects are cloned from other objects called "prototypes",
;; from which the new objects may inherit data and behavior. The
;; details of inheritance, message passing, and field lookup are
;; inspired by the Io language and the Garnet UI toolkit.

;; http://en.wikipedia.org/wiki/Prototype-based_programming
;; http://en.wikipedia.org/wiki/Message_passing
;; http://www.cliki.net/Garnet
;; http://iolanguage.com/about/

;;; Example:

;; (define-prototype rectangle ()
;;   width height)

;; (define-method initialize rectangle (&key width height)
;;   (setf <width> width)
;;   (setf <height> height))

;; (define-method area rectangle ()
;;   (* <width> <height>))

;; (define-method print rectangle (&optional (stream t))
;;   (format stream "height: ~A width: ~A area: ~A"
;; 	  <height> <width> 
;; 	  [area self]))

;; (defvar rect (clone =rectangle= :width 10 :height 8))

;; [print rect]

;; Result: "height: 8 width: 10 area: 80"
  
;;; Code: 

(in-package :rlx)

;;; Utility functions

(defun make-keyword (S)
  "Make the symbol or string S into a keyword symbol."
  (etypecase S
    (string (intern (string-upcase S) :keyword))
    (symbol (intern (symbol-name S) :keyword))))

;; :. prototype-names > 

(defun make-special-variable-name (S)
  "Make the symbol S into a special variable name. This is used to
make the names of the objects made with `define-prototype'."
 (intern (concatenate 'string "=" (symbol-name S) "=")))

;;; Object data structure

;; Each object's "bookkeeping data" is stored in a structure. The
;; structure itself stands for the object.

(defstruct object
  ;; The most important features of an object are its "fields" or data
  ;; members. We use a property list to represent the field
  ;; collection. Methods are just function-valued fields. See
  ;; `field-value' and `set-field-value'.
  fields
  ;; Objects can inherit field values from a prototype object which
  ;; then influences the new object's behavior. We must store a link
  ;; to this "parent" object so that `field-value' can obtain the
  ;; inherited field values.
  parent
  ;; Objects may have names. A name is a symbol that identifies the
  ;; object. Named objects are "prototypes" from which other objects
  ;; may be created.
  name)

;;; Fields

;; An object's field collection is just a property list. The function
;; `field-value' implements the chaining field lookups that make
;; inheritance work in CLON.

;; If a field value is not present in a given object's field
;; collection, the object's parent is also checked for a value, and
;; then its parent, and so on. This is how objects can inherit data
;; and behavior from prototypes. See `field-value'.

;; When you set the value of any field, the parent's value is
;; hidden from that point on. There is no way to remove a local field
;; value. See `set-field-value'.

(defvar *lookup-failure* (gensym)
  "A value returned in order to signify the failure of a field lookup.
When looking up fields, we need a default value to indicate that a
lookup failed (see `getf'). But this value could be a perfectly
legitimate field value, and the system would then falsely report a
field lookup error because it could not tell the difference. By using
an uninterned symbol as that default value, we can be sure that it
won't be `eq' to anything. See `field-value'.

This is only used internally. In most situations, a field access or
method call that references a non-existent field will signal a
`no-such-field' error.")

(define-condition no-such-field (error)
  ((field-name :initarg :field-name :accessor field-name)
   (object :initarg :object :accessor object))
  (:report (lambda (condition stream)
	     ;; TODO improve object printing
	     (format stream "No such field ~S in object ~S." 
		     (field-name condition)
		     (object condition)))))
		     
;; Next come the main user-level functions for setting/getting field
;; values. 

(defun field-value (field object &optional noerror)
  "Return the value of FIELD in OBJECT.
If the FIELD has no value in OBJECT, then the object's parent is also
checked, and so on. If a value is found during these checks, it is
returned. If a value cannot be found, an error of type `no-such-field'
is signaled, unless NOERROR is non-nil; in that case,
`*lookup-failure' is returned. See also `has-field'."
  (let (pointer result found)
    (setf pointer object)
    ;; search the chain of objects for a field value.
    (loop while (and pointer (not found)) do
	 (setf result (getf (object-fields pointer)
			    field *lookup-failure*))
	 (if (eq *lookup-failure* result)
	     ;; it's not here. search the parent, if any.
	     (setf pointer (object-parent pointer))
	     ;; we found a value in this object.
	     (setf found t)))
    (if found 
	result
	(if noerror 
	    *lookup-failure*   
	    (error 'no-such-field :field-name field :object object)))))

(defun set-field-value (field object value)
  "Set OBJECT's FIELD to VALUE.
The new value overrides any inherited value."
  (setf (getf (object-fields object) field)
	value))

(defsetf field-value set-field-value)

(defun has-field (field object)
  "Return non-nil if FIELD has any value in OBJECT."
  (not (eq *lookup-failure* (field-value field object :noerror))))

;;; Field options

;; Every prototype has a set of field options. Field options record
;; metadata regarding fields; for example, `define-method' stores the
;; argument list and documentation string for the method in the
;; prototype's field options property list.

(defun field-options (field object)
  "Obtain the options property list for FIELD in OBJECT."
  (cdr (assoc field (field-value :field-descriptors object))))

(defun set-field-options (field object options)
  "Set the options property list to OPTIONS for FIELD in OBJECT." 
  (setf (field-value :field-descriptors object)
	(remove-duplicates (acons field options
				  (field-value :field-descriptors object))
			   :key #'car :from-end t)))

(defsetf field-options set-field-options)

(defun field-option-value (field object option)
  "Return the value of the OPTION for FIELD in OBJECT."
  (getf (field-options field object) option))

(defun set-field-option-value (field object option value)
  "Set the value of the OPTION for FIELD in OBJECT to VALUE."
  (setf (getf (field-options field object) option)
	value))
  
(defsetf field-option-value set-field-option-value)

;;; Methods and messages

;; Methods are function-valued fields whose first argument is the
;; object to operate on. The remaining arguments are the arguments to
;; the method. The `define-method' macro defined later will insert
;; this implicit `self' argument for you, and implement some syntactic
;; sugar.

;; First comes the basic function for sending a message synchronously
;; and obtaining a return value.  

(defun send (sender method-key object &rest args)
  "Invoke the method identified by METHOD-KEY on the OBJECT with ARGS.
If the method is not found, attempt to forward the message. The SENDER
argument is ignored."
  ;; See also `send-queue' and `send-parent'
  (declare (ignore sender))
  (assert (object-p object))
  (let ((func (field-value method-key object :noerror)))
    (if (not (eq *lookup-failure* func))
	(apply func object args)
	;; no such method. try forwarding
	(apply (field-value :forward object)
	       object method-key args))))

;; :. forwarding >

;; When a message cannot be delivered because no corresponding
;; function was found, CLON attempts to re-send the message via the
;; object's `forward' method (if any).

;; An object's `forward' method should accept the method-key as the
;; first argument, and the arguments of the original message as the
;; remaining arguments.

;; We also want to be able to invoke the prototype's version of a
;; method; for example during initialization, one might wish to run
;; the parent's initializer as the first statement in the child's.

(define-condition null-parent (error)
  ((method-key :initarg :message :accessor method-key)
   (object :initarg :object :accessor object))
  (:report (lambda (condition stream)
	     (format stream "Cannot find parent method ~S for object ~S." 
		     (method-key condition)
		     (object condition)))))

(defun send-parent (sender method-key object &rest args)
  "Invoke the parent's version of a method on OBJECT with ARGS."
  (declare (ignore sender))
  (let ((method-source (if (object-name object)
			   (object-parent object)
			   (object-parent (object-parent object)))))
    (if method-source
	(apply (field-value method-key method-source) object args)
	(error 'null-parent :method-key method-key :object object))))

;; In some situations, an object will wish to queue up messages to be
;; sent elsewhere at a later time. `send-queue' will do this.

;; First we need a general queue mechanism.

;; :. queueing >

(defstruct queue head tail count max)

(define-condition empty-queue (error) ())

(defun unqueue (Q)
  (when (null (queue-head Q))
    (error 'empty-queue))
  (when (eq (queue-head Q)
	    (queue-tail Q))
    ;; only one item is in the queue; kill the tail pointer
    (setf (queue-tail Q) nil))
  ;; now unqueue
  (decf (queue-count Q))
  (pop (queue-head Q)))

(defun queue (item Q)
  (let ((element (cons item nil)))
    (if (null (queue-tail Q))
	;; handle empty queue
	(progn 
	  (setf (queue-tail Q) element
		(queue-head Q) (queue-tail Q)
		(queue-count Q) 1))
	;; handle nonempty queue
	(progn 
	  (setf (cdr (queue-tail Q))
		element)
	  (pop (queue-tail Q))
	  (incf (queue-count Q)))))
  ;; now prevent exceeding any max that's been set. this is useful to
  ;; prevent allocating all memory when you don't care about throwing
  ;; away old objects.
  (when (and (numberp (queue-max Q))
	     (< (queue-max Q) (queue-count Q)))
    (unqueue Q)))

;; And now the message queueing functionality.

(defvar *message-queue* nil "This variable is bound to the current
message queue, if any.")

(defun queue-message (sender method-key receiver args)
  "Enter a message into the current `*message-queue*'."
  (queue (list sender method-key receiver args) *message-queue*))

(defun queued-messages-p ()
  "Return non-nil if there are queued messages."
  (not (null (queue-head *message-queue*))))

(defun unqueue-message ()
  "Remove the next message from the queue. The returned message is a
list of the form (METHOD-KEY RECEIVER ARGS)."
  (unqueue *message-queue*))

(defun unqueue-and-send-message ()
  (let ((msg (unqueue-message)))
    (destructuring-bind (sender method-key receiver args) msg
      (apply #'send sender method-key receiver args))))

(defmacro with-message-queue (expr &body body)
  "Run the BODY forms, capturing any queued output messages to the
message queue resulting from the evaluation of EXPR."
  `(let ((*message-queue* ,expr))
     ,@body))

(defun send-queue (sender method-key object &rest args)
  "Queue a message. Returns nil."
  (queue-message sender method-key object args))

;;; Message send syntax

;; We use reader macros to enable cleaner-looking message sends. With
;; the following definitions you can type
;;
;;   [message-name object arg1 arg2 ...]
;; 
;; instead of
;; 
;;   (send sender :message-name object arg1 arg2 ...)
;;
;; This works both inside and outside of method bodies.
;;
;; Several other shortcuts are provided:
;; 
;;   (send-parent sender :message-name object arg1 arg2 ...)
;;
;; can be abbreviated
;;
;;   [parent>>message-name object arg1 arg2 ...]
;;
;; For example:
;;
;;   [parent>>initialize self]
;;
;; One may also write
;; 
;;   [queue>>foo ...]
;;
;; To have a message queued for later delivery.

;; Note: the included file "clon.el" has Emacs settings for syntax
;; highlighting and proper parenthesis/bracket matching.

;; The reader macro picks apart the first symbol in the [a b c ...]
;; to determine the message being sent, and also which of
;; `send-queue', `send-parent', or `send' to use in sending it.

(defun message-symbol (string delimiter)
  (let ((index (search delimiter string)))
    (make-keyword (if (numberp index)
		      (subseq string (+ 2 index))
		      string))))

(defun operation-symbol (string delimiter)
  (let* ((index (search delimiter string))
	 (op-name (if (numberp index)
		      (concatenate 'string
				   "send-"
				   (subseq string 0 index))
		      "send")))
    (intern (string-upcase op-name) :rlx)))

(defvar *sender* nil "This variable is bound to the object (if any) to
receive sent messages.")

(defun message-reader (stream char)
  (declare (ignore char))
  (let* ((elements (read-delimited-list #\] stream t))
	 (message-symbol (first elements))
	 (arguments (rest elements))
	 (send-selector (symbol-name message-symbol))
	 (delimiter ">>"))
    ;; The rewritten method call is produced.
    (append (list 
	     (operation-symbol send-selector delimiter) 
	     '*sender*
	     (message-symbol send-selector delimiter))
	    arguments)))

;; Install the reader macro.

(set-macro-character #\[ #'message-reader)
(set-macro-character #\] (get-macro-character #\)))
(set-syntax-from-char #\] #\))

;;; Field reference syntax

;; Within method bodies, you can access the fields of `self' with the
;; shorthand
;;
;;   <foo> 
;;
;; instead of writing
;;
;;   (field-value :foo self)
;;
;; For example:
;;
;;   (princ <name>)
;;   (setf <width> 10)
;; 
;; Because `self' is not bound outside of method bodies, we need a
;; code-walker to implement the syntax described above. (Reader macros
;; can't be made to apply only within the body of a macro call.)

(defun transform-tree (tester transformer tree)
  (cond ((consp tree)
	 ;; it's a cons. process the two subtrees.
	 (destructuring-bind (left . right) tree
	   (cons
	    ;; process left subtree.
	    (if (funcall tester left)
		(funcall transformer left)
		;; nothing to transform here. move on down the left side.
		(if (consp left)
		    (transform-tree tester transformer left)
		    left))
	    ;; process right subtree.
	    (transform-tree tester transformer right))))
	;; it's not a cons. test it.
	((funcall tester tree)
	 (funcall transformer tree))
	;; it failed the test. leave it alone.
	(t tree)))

;; Now we turn to the syntax itself and the required tree
;; transformations.

(defun field-reference-p (form)
  "Return non-nil if FORM is a symbol like <foo>."
  (if (symbolp form)
      (let* ((name (symbol-name form))
	     (len (length name)))
	(and (string= "<" (subseq name 0 1))
	     (string= ">" (subseq name (- len 1) len))))))

(defun transform-field-reference (ref)
  "Change the symbol REF from <foo> to (field-value :foo self)."
  (let ((name (symbol-name ref)))
    (list 'field-value (make-keyword (subseq name 1 
					     (- (length name) 1))) 'self)))

(defun transform-method-body (body)
  "Process the forms in BODY to transform field references."
  (transform-tree #'field-reference-p
		  #'transform-field-reference
		  body))

;; The `define-method' macro defined below is the main top-level facility
;; for adding methods to prototypes.

(defmacro define-method
    (method-name prototype-name arglist &body method-body)
  "Define a new method.

METHOD-NAME is a symbol naming the method.  PROTOTYPE-NAME is the name
of the prototype you are defining a method for. This should be a
symbol (without equals signs---see :. prototype-names >). ARGLIST is
the argument list for the method. If METHOD-BODY begins with a string,
this string becomes the documentation string for the method.

Any DECLARE forms must appear as the first non-string sexp.

The forms in METHOD-BODY are executed when the method is invoked.
The hidden argument `self' may be referred to as needed within
the method body; it is bound to the object upon which the method
was invoked."
  ;; build the components of the defun
  (let* ((documentation (if (stringp (first method-body))
			    (first method-body)))
	 (body2 (remove-if #'stringp (transform-method-body method-body)))
	 ;; handle DECLARE forms when these appear first
	 (declaration (when (and (listp (first body2))
				 (eq 'declare (first (first body2))))
			(first body2)))
	 ;; :. prototype-names >
	 (prototype-special-name (make-special-variable-name prototype-name))
	 (field-name (make-keyword method-name))
	 (defun-symbol (intern (concatenate 'string
					    (symbol-name method-name) 
					    ">>"
					    (symbol-name prototype-name)))))
    `(progn 
       (if (not (boundp ',prototype-special-name))
	   (error (format nil "Cannot define method ~A for nonexistent prototype ~A"
			',method-name ',prototype-name)))
       (let ((prototype (symbol-value ',prototype-special-name)))
	 ;; define the method's Lisp function
	 (defun ,defun-symbol (self ,@arglist)
	   ,@(if documentation (list documentation))
	   ,declaration
	   (let ((*sender* self))
	     ,@(if declaration 
		   (rest body2)
		   body2)))
	 ;; store the method's function in the prototype's field
	 (set-field-value ,field-name prototype ',defun-symbol)
	 ;; add new method-descriptor for this method to the prototype
	 (set-field-option-value ,field-name prototype
				 :documentation ,documentation)
	 (set-field-option-value ,field-name prototype
				 :arguments ',arglist)))))

;;; Prototypes

;; Objects are created by cloning them from "prototypes". Prototypes
;; are named objects that represent prototypical instances of a
;; certain kind of object. This section implements `define-prototype',
;; the top-level user macro for defining new object prototypes.

;; First we need to define the written syntax for field options, so
;; that we can write these options into prototype declarations later.

(defun transform-declaration-field-descriptor (D)
  "Convert the declaration-field-descriptor D into a canonical field
descriptor.

The descriptor D must be either a symbol, in which case a field is
defined with no options, or a list of the form:

 (:FIELD-NAME . OPTIONS)

Where OPTIONS is a property list of field options.

The returned entry will be of the form:

 (:FIELD-NAME OPTIONS) 

and will be suitable for use with the functions that operate on field
descriptors, and for inclusion in the association list
<field-descriptors>.

See also `define-prototype'.
"
  (etypecase D
    (symbol (list (make-keyword D) nil))
    (list (list (make-keyword (car D)) (cdr D)))))

(defun compose-blank-fields (descriptors)
  ;; TODO document this
  (let (fields)
    (dolist (d descriptors)
      (push nil fields)
      (push (make-keyword (car d)) fields))
    fields))

;; Initforms (i.e. the values of the field option `:initform')
;; initialize fields. A field's initform (if any) is evaluated when an
;; object is cloned from a prototype, and becomes the value of the
;; field for the new instance (instead of inheriting the prototype's
;; value.) See also `define-prototype'.

;; Now we make setf statements (i.e. "field initializers") out of these
;; field initforms.

(defun make-field-initializer (descriptor)
  "Create a setf statement that initializes a field.
The initform is taken from DESCRIPTOR. If there is no initform
specified, no setf statement is generated, because in this case the
slot value is inherited."
  (destructuring-bind (field (&key initform &allow-other-keys)) descriptor
    (if initform `(set-field-value ,field self ,initform))))

(defmacro define-prototype (name
			    (&key parent 
				  documentation
				  &allow-other-keys)
			    &body declaration-field-descriptors)
  "Create a new object prototype (possibly based on another prototype).

NAME should be a symbol naming the prototype. A special variable is
created, with equals signs bracketing the name; this variable's value
is the resulting prototype. For example, if your prototype is named
`foo', the special variable will be named `=foo=', and you create
objects with:

 (clone =foo=)

See also `clone' and the radio keyword :. prototype-names >

The second argument is a property list of options for the
prototype. Valid keys are:

 :DOCUMENTATION     The documentation string for this prototype.
 :PARENT            The parent prototype from which the new prototype will 
                    inherit fields. This form is evaluated.
                     
DECLARATION-FIELD-DESCRIPTORS should be a list, each entry of which is
either a list of the form

  (FIELD-NAME . OPTIONS)

or, simply a symbol naming the field---a shorthand for declaring a
field with that name and no options. See also
`transform-declaration-field-descriptor'.

OPTIONS is a property list of field options. Valid keys are:

 :INITFORM          A form evaluated to initialize the field
                    upon cloning. If :initform is not provided,
                    the value is inherited from the PARENT.
                    With \":initform nil\", the field is initialized 
                    with the value nil.
 :DOCUMENTATION     Documentation string for the field.
"
  (let* ((descriptors (mapcar #'transform-declaration-field-descriptor 
			      declaration-field-descriptors))
	 ;; :. prototype-names >
	 (proto-symbol (make-special-variable-name name))
	 (field-initializer-body (delete nil (mapcar #'make-field-initializer 
						     descriptors))))
    `(progn
       ;; Need this at top-level for compiler to know about the special var
       (defparameter ,proto-symbol nil)
       (let* ((fields (append ',(compose-blank-fields descriptors)
			      (list
			       :field-descriptors ',descriptors
			       :documentation ,documentation
			       :initialize-fields (function (lambda (self) 
						    ,@field-initializer-body)))))
	      (prototype (make-object :fields fields
				      :name ',proto-symbol
				      :parent ,parent)))
	 (defparameter ,proto-symbol prototype)
	 (send nil :initialize-fields prototype)
	 ;; the prototype's parent may have an initialize method.
	 ;; if so, we need to initialize the present prototype.
	 (if (has-field :initialize prototype)   
	     (send nil :initialize prototype))
	 prototype))))

;;; Cloning objects

(defun clone (prototype &rest initargs)
  "Create a new object from PROTOTYPE and pass INITARGS to the
initializer."
  (let ((new-object (make-object :parent prototype)))
    (send nil :initialize-fields new-object)
    (if (has-field :initialize new-object)
	(apply #'send nil :initialize new-object initargs))
    new-object))

;;; Printing objects

(defun object-address-string (ob)
  (let ((string (with-output-to-string (s)
		  (print-unreadable-object (ob s :identity t)))))
    (subseq string
	    (1+ (search "{" string))
	    (search "}" string))))
  
;;; clon.lisp ends here
