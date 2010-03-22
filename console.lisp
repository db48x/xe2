;;; console.lisp --- core operations for XE2

;; Copyright (C) 2006, 2007, 2008, 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: multimedia, games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The "console" is the library which provides all XE2 system
;; services. Primitive operations such as setting the resolution,
;; displaying bitmaps, drawing lines, playing sounds, file access, and
;; keyboard/mouse input are handled here. 

;; Currently it uses the cross-platform SDL library (via
;; LISPBUILDER-SDL) as its device driver, and wraps the library for
;; use by the rest of XE2.

;; http://lispbuilder.sourceforge.net/

(in-package :xe2) 

;;; Message logging

(defparameter *message-logging* nil)

(defun message (format-string &rest args)
  "Print a log message to the standard output. The FORMAT-STRING and
remaining arguments are passed to `format'.

When the variable `*message-logging*' is nil, this output is
disabled."
  (when *message-logging*
    (apply #'format t format-string args)
    (fresh-line)))

;;; Sequence numbers

(defvar *sequence-number* 0)

(defun genseq (&optional (x 0))
  "Generate an all-purpose sequence number."
  (+ x (incf *sequence-number*)))

;;; Physics timestep callback

(defvar *dt* 10)

(defvar *physics-function* nil)

(defun do-physics (&rest args) 
  (when (functionp *physics-function*)
    (apply *physics-function* args)))

;;; Mixer channels

(defvar *channels* 128 "Number of audio mixer channels to use.")
   
;;; Hooks

(defun add-hook (hook func)
  "Hooks are special variables whose names are of the form
`*foo-hook*' and whose values are lists of functions taking no
arguments. The functions of a given hook are all invoked (in list
order) whenever the hook is run with `run-hook'.

This function arranges for FUNC to be invoked whenever HOOK is triggered with
`run-hook'. The function should have no arguments."
  (pushnew func (symbol-value hook)))

(defun remove-hook (hook func)
  "Stop calling FUNC whenever HOOK is triggered."
  (setf (symbol-value hook)
	(delete func (symbol-value hook))))

(defun run-hook (hook)
  "Call all the functions in HOOK, in list order."
  (dolist (func (symbol-value hook))
    (funcall func)))

;;; Vector utility macro 

(defmacro do-cells ((var expr) &body body)
  "Execute the forms in BODY with VAR bound successively to the
elements of the vector produced by evaluating EXPR."
  (let ((counter (gensym))
	(vector (gensym)))
    `(progn
       (let* ((,var nil)
	      (,vector (progn ,expr)))
	 (when (vectorp ,vector)
	   (let ((,counter (fill-pointer ,vector)))
	     (decf ,counter)
	     (loop while (>= ,counter 0) 
		   do (setf ,var (aref ,vector ,counter))
		   (progn (decf ,counter)
			  (when ,var ,@body)))))))))

;;; The active widgets list 

(defvar *active-widgets* nil "List of active widget objects. 
These widgets receive input events and are rendered to the screen by
the console. See also `send-event-to-widgets'.

Do not set this variable directly from a module; instead, call
`install-widgets'.")

(defun show-widgets ()
  "Draw the active widgets to the screen."
  (dolist (widget *active-widgets*)
    (with-field-values (image x y) widget
      (when image
	[render widget]
	(sdl:draw-surface-at-* image x y)))))

(defvar *module-widgets* nil "List of widget objects in the current module.")

(defun install-widgets (&rest widgets)
  "User-level function for setting the active widget set. Note that
XE2 may override the current widget set at any time for system menus
and the like."
  (setf *module-widgets* widgets)
  (setf *active-widgets* widgets))
;; TODO why does this crash: 
;;  (show-widgets))

;;; Key repeat emulation

(defvar *key-table* (make-hash-table :test 'equal))

(defvar *held-keys* nil)

(defun enable-held-keys (&rest args)
  "Enable key repeat on every frame when held. Arguments are ignored
for backward-compatibility."
  (when args 
    (message "Warning. DELAY and INTERVAL arguments to XE2:ENABLE-HELD-KEYS are deprecated and ignored."))
  (setf *key-table* (make-hash-table :test 'equal))
  (setf *held-keys* t))
  ;; (let ((delay-milliseconds (truncate (* delay (/ 1000.0 *frame-rate*))))
  ;; 	(interval-milliseconds (truncate (* interval (/ 1000.0 *frame-rate*)))))
  ;;   (sdl:enable-key-repeat delay-milliseconds interval-milliseconds)))

(defun disable-held-keys ()
  "Disable key repeat."
  (setf *held-keys* nil))
;;  (sdl:disable-key-repeat))

(defun send-held-events ()
  (unless (null *key-table*)
    (maphash #'(lambda (event counter)
		 (dispatch-event event)
		 ;; A counter value of -1 means that the key release
		 ;; happened before the event had a chance to be sent.
		 ;; These must be removed immediately after sending once.
		 (if (minusp counter)
		     (remhash event *key-table*)
		     ;; Otherwise, keep counting how many frames the
		     ;; key is held for.
		     (when (numberp (gethash event *key-table*))
		       (incf (gethash event *key-table*)))))
	     *key-table*)))

(defun break-events (event)
  (labels ((break-it (event2 ignore)
	     (when (intersection event event2 :test 'equal)
	       (message "Breaking ~S due to match with ~S." event2 event)
	       (remhash event2 *key-table*))))
    (maphash #'break-it *key-table*)))

;;; Event handling and widgets

(defvar *event* nil)

(defun send-event-to-widgets (event)
  "Keyboard, mouse, joystick, and timer events are represented as
event lists of the form:

:      (STRING . MODIFIERS)

Where MODIFIERS is a list of symbols like :shift, :control, :alt,
 :timer, :system, :mouse, and so on.

The default event handler attempts to deliver a keypress to one of
the widgets in `*active-widgets*'. See widgets.lisp and the docstrings
below for more information.

This function attempts to deliver EVENT to each of the *active-widgets*
one at a time (in list order) until one of them is found to have a
matching keybinding, in which case the keybinding's corresponding
function is triggered. If none of the widgets have a matching
keybinding, nothing happens, and this function returns nil."
  (some #'(lambda (widget)
	    [handle-key widget event])
	*active-widgets*))

(defvar *event-handler-function* #'send-event-to-widgets
  "Function to be called with keypress events. This function should
accept an event list of the form

  (STRING . MODIFIERS)

where STRING is a string representing the key, and MODIFIERS is a list
of key modifier symbols like :shift, :control, :alt, and so on.

The modifier list is sorted; thus, events can be compared for
equality with `equal' and used as hashtable keys.

The default event handler is `send-event-to-widgets', which see. An
XE2 game can use the widget framework to do its drawing and event
handling, or override `*event-handler-function*' and do something
else.")

(defun normalize-event (event)
  "Convert EVENT to a normal form suitable for `equal' comparisons."
  (setf (rest event)
	(sort (remove-duplicates (delete nil (rest event)))
	      #'string< :key #'symbol-name))
  event)

(defun dispatch-event (event)
  "Send EVENT to the handler function."
  (setf *event* event)
  (if *event-handler-function*
      (progn (message "TRANSLATED EVENT: ~A" event)
	     (funcall *event-handler-function* event))
      (error "No event handler registered.")))

(defun hit-widgets (x y &optional (widgets *active-widgets*))
  "Hit test the WIDGETS to find the clicked widget."
  (some #'(lambda (widget)
	    [hit widget x y])
	(reverse widgets)))

;;; Translating SDL key events into XE2 event lists

(defparameter *other-modifier-symbols* '(:button-down :button-up :axis))

(defun make-key-modifier-symbol (sdl-mod)
  "Translate from the SDL key modifier symbol SDL-MOD to our own
key event symbols."
  (if (or (member sdl-mod *joystick-button-symbols*)
	  (member sdl-mod *other-modifier-symbols*))
      sdl-mod
      (ecase sdl-mod
	(:SDL-KEY-MOD-NONE nil)
	(:SDL-KEY-MOD-LSHIFT :shift)
	(:SDL-KEY-MOD-RSHIFT :shift)
	(:SDL-KEY-MOD-LCTRL :control)
	(:SDL-KEY-MOD-RCTRL :control)
	(:SDL-KEY-MOD-LALT :alt)
	(:SDL-KEY-MOD-RALT :alt)
	(:SDL-KEY-MOD-LMETA :meta)
	(:SDL-KEY-MOD-RMETA :meta)
	;; fix for windows
	(:SDL-KEY-MOD-NUM nil)
	(:SDL-KEY-MOD-CAPS :caps-lock)
	(:SDL-KEY-MOD-MODE nil)
	(:SDL-KEY-MOD-RESERVED nil)
	;; for compatibility:
	(:SDL-KEY-NONE nil)
	(:SDL-KEY-LSHIFT :shift)
	(:SDL-KEY-RSHIFT :shift)
	(:SDL-KEY-LCTRL :control)
	(:SDL-KEY-RCTRL :control)
	(:SDL-KEY-LALT :alt)
	(:SDL-KEY-RALT :alt)
	(:SDL-KEY-LMETA :meta)
	(:SDL-KEY-RMETA :meta)
	;; fix for windows
	(:SDL-KEY-MOD-NUM nil)
	(:SDL-KEY-CAPS :caps-lock)
	(:SDL-KEY-MODE nil)
	(:SDL-KEY-RESERVED nil)
	)))
  
(defun make-key-string (sdl-key)
  "Translate from :SDL-KEY-X to the string \"X\"."
  (let ((prefix "SDL-KEY-"))
    (subseq (symbol-name sdl-key)
            (length prefix))))

(defun make-event (sdl-key sdl-mods)
  "Create a normalized event out of the SDL data SDL-KEY and SDL-MODS.
The purpose of putting events in a normal form is to enable their use
as hash keys."
  (message "SDL KEY AND MODS: ~A" (list sdl-key sdl-mods))
  (normalize-event
   (cons (if (eq sdl-key :joystick) 
	     "JOYSTICK"
	     (if (eq sdl-key :axis) 
		 "AXIS"
		 (make-key-string sdl-key)))
	 (mapcar #'make-key-modifier-symbol
		 (cond ((keywordp sdl-mods)
			(list sdl-mods))
		       ((listp sdl-mods)
			sdl-mods)
		       ;; catch apparent lispbuilder-sdl bug?
		       ((eql 0 sdl-mods)
			nil))))))

;;; Joystick support (gamepad probably required)

(defparameter *joystick-button-symbols*
  '(:button-0 :button-1 :button-2 :button-3 :button-4 :button-5 :button-6 :button-7 :button-8 :button-9
    :button-10 :button-11 :button-12 :button-13 :button-14 :button-15 :button-16 :button-17 :button-18 :button-19
    :left :right :up :down :select :start))

(defparameter *generic-joystick-mapping*
  '((0 . :button-0)
    (1 . :button-1)
    (2 . :button-2)
    (3 . :button-3)
    (4 . :button-4)
    (5 . :button-5)
    (6 . :button-6)
    (7 . :button-7)
    (8 . :button-8)
    (9 . :button-9)
    (10 . :button-10)
    (11 . :button-11)
    (12 . :button-12)
    (13 . :button-13)
    (14 . :button-14)
    (15 . :button-15)
    (16 . :button-16)
    (17 . :button-17)
    (18 . :button-18)
    (19 . :button-19)
    (20 . :button-20)))

(defvar *joystick-dead-zone* 2000)

(defvar *joystick-axis-mapping* '((0 :left :right)
				  (1 :up :down)))

(defun axis-value-to-direction (axis value)
  (let ((entry (assoc axis *joystick-axis-mapping*)))
    (if entry 
	(if (plusp value)
	    (second (cdr entry))
	    (when (minusp value)
	      (first (cdr entry)))))))

(defvar *joystick-axis-values* (make-array 100 :initial-element 0))

(defun do-joystick-axis-event (axis value state)
  (dispatch-event (make-event :axis 
			      (list (axis-value-to-direction axis value)
				    state))))
	
(defun update-joystick-axis (axis value)
  (let ((state (if (< (abs value) *joystick-dead-zone*)
		   :button-up :button-down)))
    (setf (aref *joystick-axis-values* axis) value)))

(defun poll-joystick-axis (axis)
  (aref *joystick-axis-values* axis))

(defvar *joystick-mapping* *generic-joystick-mapping*)

(defun translate-joystick-button (button)
  (cdr (assoc button *joystick-mapping*)))

(defun symbol-to-button (sym)
  (let ((entry (some #'(lambda (entry)
			 (when (eq sym (cdr entry))
			   entry))
		     *joystick-mapping*)))
    (when entry 
      (car entry))))

(defvar *joystick-device* nil)

(defvar *joystick-buttons* nil
  "The nth element is non-nil when the nth button is pressed.")

(defvar *joystick-position* nil "Current position of the joystick, as a direction keyword.")

(defun reset-joystick ()
  "Re-open the joystick device and re-initialize the state."
  (setf *joystick-device* (sdl-cffi::sdl-joystick-open 0))
  (setf *joystick-buttons* (make-array 100 :initial-element nil))
  (setf *joystick-position* :here))

(defun update-joystick (button state)
  "Update the table in `*joystick-buttons*' to reflect the STATE of
the BUTTON. STATE should be either 1 (on) or 0 (off)."
  (setf (aref *joystick-buttons* button) (ecase state
					   (1 t)
					   (0 nil)))
  (let ((sym (translate-joystick-button button)))
    (labels ((pressed (sym) 
	       (let ((b (symbol-to-button sym)))
		 (when (integerp b)
		   (aref *joystick-buttons* b)))))
      (setf *joystick-position* 
	    (or (cond ((and (pressed :up) (pressed :right))
		       :northeast)
		      ((and (pressed :up) (pressed :left))
		       :northwest)
		      ((and (pressed :down) (pressed :right))
		       :southeast)
		      ((and (pressed :down) (pressed :left))
		       :southwest)
		      ((pressed :up)
		       :north)
		      ((pressed :down)
		       :south)
		      ((pressed :right)
		       :east)
		      ((pressed :left)
		       :west))
		:here)))
    (message "UPDATE-JOYSTICK: BUTTON(~S) STATE(~S)" button state)))

(defun poll-joystick-button (button)
  "Return 1 if the button numbered BUTTON is pressed, otherwise 0."
  (sdl-cffi::sdl-joystick-get-button *joystick-device* button))

(defun poll-all-buttons ()
  (dolist (entry *joystick-mapping*)
    (destructuring-bind (button . symbol) entry
      (update-joystick button (poll-joystick-button button)))))

(defun generate-button-events ()
  (let ((button 0) state sym)
    (loop while (< button (length *joystick-buttons*))
	  do (setf state (aref *joystick-buttons* button))
	     (setf sym (translate-joystick-button button))
	     (when (and state sym)
	       (dispatch-event (make-event :joystick sym)))
	     (incf button))))

;;; The active world

(defvar *world* nil 
"The current world object. Only one may be active at a time. See also
worlds.lisp. Cells are free to send messages to `*world*' at
any time, because it is always bound to the world containing the cell
at the time the cell method is run.")

(defun world ()
  "Return the current world."
  *world*)

;;; Auto-zooming images

(defvar *zoom-factor* 1 
"When set to some integer greater than 1, all image resources are
scaled by that factor unless marked with the property :nozoom t.")

(defun is-zoomed-resource (resource)
  "Return non-nil if the RESOURCE should be zoomed by `*zoom-factor*'."
  (not (getf (resource-properties resource)
	     :nozoom)))

(defun zoom-image (image &optional (factor *zoom-factor*))
  "Return a zoomed version of IMAGE, zoomed by FACTOR.
Allocates a new image."
  (assert (integerp *zoom-factor*))
  (lispbuilder-sdl-gfx:zoom-surface *zoom-factor* *zoom-factor*
				    :surface image
				    :smooth nil))

;;; Timing

(defvar *frame-rate* 30 
"The intended frame rate of the game. Recommended value is 30.
Don't set this variable directly; use `set-frame-rate' instead.")

(defun set-frame-rate (rate)
  "Set the frame rate for the game. The recommended default is 30.
You only need to set the frame rate if you are using the timer; see
`enable-timer'."
  (message "Setting frame rate to ~S" rate)
  (message "WARNING: SET-FRAME-RATE is deprecated and does nothing.")
  (setf *frame-rate* rate)
  (setf (sdl:frame-rate) rate))

(defvar *clock* 0 "Number of frames until next timer event.")

(defvar *timer-p* nil "Non-nil if timer events are actually being sent.")

(defun enable-timer ()
  "Enable timer events. The next scheduled event will be the first sent."
  (setf *timer-p* t))

(defun disable-timer ()
  "Disable timer events."
  (setf *timer-p* nil))

(defvar *timer-event* (list nil :timer) 
  "Since all timer events are identical, this is the only one we need.")

(defvar *timer-interval* 15 
"Number of frames to wait before sending each timer event.
Set this to 0 to get a timer event every frame.
Don't set this yourself; use `set-timer-interval'.")

(defun set-timer-interval (interval)
  "Set the number of frames to wait before sending each timer event.
Set it to 0 to get a timer event every frame."
  (setf *timer-interval* interval))

;;; Screen dimensions

(defvar *screen-width* 640 "The width (in pixels) of the game
window. Set this in the game startup file.")

(defun set-screen-width (width)
  (setf *screen-width* width))

(defvar *screen-height* 480 "The height (in pixels) of the game
window. Set this in the game startup file.")

(defun set-screen-height (height)
  (setf *screen-height* height))

;;; The main loop of XE2

(defvar *next-module* "standard")

(defvar *quitting* nil)

(defvar *fullscreen* nil "When non-nil, attempt to use fullscreen mode.")

(defvar *window-title* "XE2")

(defun run-main-loop ()
  "Initialize the console, open a window, and play.
We want to process all inputs, update the game state, then update the
display."
;;  (setf *physics-function* nil)
  (let ((fps (make-instance 'sdl:fps-unlocked :dt *dt* :ps-fn #'do-physics)))
    (if *fullscreen*
	(sdl:window *screen-width* *screen-height*
		    :fps fps 
		    :title-caption *window-title*
		    :flags sdl:SDL-FULLSCREEN)
	(sdl:window *screen-width* *screen-height*
		    :fps fps
		    :title-caption *window-title*)))
  (reset-joystick)
  (sdl:clear-display sdl:*black*)
  (show-widgets)
  (sdl:update-display)
  (run-hook '*initialization-hook*)
  (let ((events-this-frame 0))
    (sdl:with-events ()
      (:quit-event () (prog1 t))
      (:mouse-motion-event (:state state :x x :y y :x-rel x-rel :y-rel y-rel)
			   nil)
      (:mouse-button-down-event (:button button :state state :x x :y y)
				(let ((object (hit-widgets x y *active-widgets*)))
				  (if (null object)
				      (message "")
				      (progn 
					;; deliver messages in a queued environment
					(sdl:clear-display sdl:*black*)
					(when *world*
					  (when (field-value :message-queue *world*)
					    (with-message-queue (field-value :message-queue *world*)
					      (case button
						(1 (when (has-method :select object) 
						     [select object]))
						(3 (when (has-method :activate object) 
						     [activate object]))))
					    [process-messages *world*]))))))
      (:mouse-button-up-event (:button button :state state :x x :y y)
			      nil)
      (:joy-button-down-event (:which which :button button :state state)
			      (when (assoc button *joystick-mapping*)
				(update-joystick button state)
				(dispatch-event (make-event :joystick
							    (list (translate-joystick-button button) 
								  :button-down)))))
      (:joy-button-up-event (:which which :button button :state state)  
			    (when (assoc button *joystick-mapping*)
			      (update-joystick button state)
			      (dispatch-event (make-event :joystick
							(list (translate-joystick-button button) 
							      :button-up)))))
      (:joy-axis-motion-event (:which which :axis axis :value value)
			      (update-joystick-axis axis value))
      (:video-expose-event () (sdl:update-display))
      (:key-down-event (:key key :mod-key mod)
		       (let ((event (make-event key mod)))
			 (if *held-keys*
			     (when (null (gethash event *key-table*))
			       (message "Inserting event ~A" event)
			       (setf (gethash event *key-table*) 0))
			     (dispatch-event event))))
      (:key-up-event (:key key :mod-key mod)
		     (when *held-keys*
		       (let* ((event (make-event key mod))
			      (entry (gethash event *key-table*)))
			 (if (numberp entry)
			     (if (plusp entry)
				 (progn (message "Removing entry ~A:~A" event entry)
					(remhash event *key-table*))
				 (when (zerop entry)
				   ;; This event hasn't yet been sent,
				   ;; but the key release happened
				   ;; now. Mark this entry as pending
				   ;; deletion (by setting its value to -1)
				   (setf (gethash event *key-table*) -1)))
			     (break-events event)))))
      (:idle ()
	     (if *timer-p*
	       (if (zerop *clock*)
		   (progn 
		     (sdl:clear-display sdl:*black*)
		     ;; send held events
 		     (when *held-keys*
		       (send-held-events))
		     ;; send timer event
		     (dispatch-event *timer-event*)
		     ;; send any joystick button events
		     ;; (poll-all-buttons)
		     ;;  (generate-button-events)
		     ;; update display
		     (show-widgets)
		     (sdl:update-display)
		     (setf *clock* *timer-interval*))
		   (decf *clock*))
		 ;; clean this up. these two cases aren't that different.
		 (progn 
		   (sdl:clear-display sdl:*black*)
		   (when *held-keys* ;; TODO move this to do-physics?
		     (send-held-events))
		   (show-widgets) 
		   (setf *event* nil) ;; TODO where to do this? 
		   (sdl:update-display)))))))
		 

;;; The .xe2rc user init file

(defparameter *user-init-file-name* ".xe2rc")

(defvar *initialization-hook* nil 
"This hook is run after the XE2 console is initialized.
Set timer parameters and other settings here.")

(defun load-user-init-file ()
  (let ((file (merge-pathnames (make-pathname :name *user-init-file-name*)
			       (user-homedir-pathname))))
    (when (probe-file file)
      (load (merge-pathnames (make-pathname :name *user-init-file-name*)
			     (user-homedir-pathname))))))

(defparameter *user-keyboard-layout* :qwerty)

(defparameter *use-sound* t "Non-nil (the default) is to use sound. Nil disables sound.")

;;; PAK resource interchange files

(defparameter *pak-file-extension* ".pak"
"PAK is a simple Lisp data interchange file format readable and
writable by both Emacs Lisp and Common Lisp. A PAK file can contain
one or more data resources. A 'resource' is an image, sound, text,
font, lisp program, or other data whose interpretation is up to the
client.

A PAK resource can be either self-contained, or point to an
external file for its data.

The syntax of PAK files is a subset of the Common Lisp reader
syntax that is also acceptable to the GNU Emacs reader (reasonably
small decimal integers and floating-point numbers, strings, lists,
and symbols).

A 'resource record' defines a resource. A resource record is a
structure with the following elements:

 :NAME    A string; the name of the resource.
          The colon character : is reserved and used to specify 
          resource transformations; see below.
 :TYPE    A keyword symbol identifying the data type.
          Corresponding handlers are the responsibility of the client.
          See also `*resource-handlers*' and `load-resource'.

          The special type :pak is used to load the pak file
          specified in :FILE, from (optionally) another module
          whose name is given in :DATA.

          The special type :alias is used to provide multiple names
          for a resource. The :DATA field contains the name of the
          target resource.

 :PROPERTIES  Property list with extra data; for example :copyright,
              :license, :author. 
              The special property :AUTOLOAD, when non-nil causes
              the resource to be loaded automatically.

 :FILE    Name of file to load data from, if any. 
          Relative to directory of PAK file.
 :DATA    Lisp data encoding the resource itself, if any.

In memory, these will be represented by resource structs (see
below).  On disk, it's a property list printed as text. Unknown
keys will trigger an error. 

The string '()' is a valid .PAK file; it contains no resources.")

(defstruct resource 
  name type properties file data object)

;; The extra `object' field is not saved in .PAK files; it is used to
;; store driver-dependent loaded resources (i.e. SDL image surface
;; objects and so on). This is used in the resource table.

(defun resource-to-plist (res)
  "Convert the resource record RES into a property list.

This prepares it for printing as part of a PAK file."
  (list :name (resource-name res)
	:type (resource-type res)
	:properties (resource-properties res)
	:file (resource-file res)
	:data (resource-data res)
	:object nil))

;; First we need routines to read and write raw s-expressions to and
;; from text files.

(defun write-sexp-to-file (filename sexp)
  (with-open-file (file filename :direction :output 
			:if-exists :overwrite
			:if-does-not-exist :create)
    (format file "~S" sexp)))

(defun read-sexp-from-file (filename)
  (with-open-file (file filename :direction :input)
    (read file)))

;; Now tie it all together with routines that read and write
;; collections of records into PAK files.

(defun write-pak (filename resources)
  "Write the RESOURCES to the PAK file FILENAME."
  (write-sexp-to-file (mapcar #'resource-to-plist resources) filename))

(defun read-pak (filename)
  "Return a list of resources from the PAK file FILENAME."
  (mapcar #'(lambda (plist)
	      (apply #'make-resource plist))
	  (read-sexp-from-file filename)))

;;; Resources and modules

(defvar *resource-table* nil 
  "A hash table mapping resource names to resource records. All loaded
resources go in this one hash table.

The `resource table' maps resource names to their corresponding
records. `Indexing' a resource means that its resource record is
added to the resource table. `Loading' a resource means that any
associated driver-dependent object (SDL image surface, audio buffer
object, etc) is created. This value is stored into the OBJECT field
of the resource record upon loading; see `load-resource'.

The loading operation may be driver-dependent, so each resource
type (i.e. :image, :text, :sound) is handled by its own plugin
function (see `*resource-handlers*').

`Finding' a resource means looking up its record in the resource
table, and loading the resource if it hasn't been loaded already.
A lookup failure results in an error. See `find-resource'.

A `module' is a directory full of resource files. The name of the
module is the name of the directory. Each module must contain a
file called {module-name}.pak, which should contain an index of
all the module's resources. Multiple modules may be loaded at one
time. In addition the special resource .startup will be loaded;
if this is type :lisp, the startup code for your game can go in
that external lisp file.")

(defun initialize-resource-table ()
  "Create a new empty resource table."
   (setf *resource-table* (make-hash-table :test 'equal)))

(defun index-resource (resource)
  "Add the RESOURCE's record to the resource table.
If a record with that name already exists, it is replaced.  However,
if the resource is an :alias, just the string name of the target
resource is stored; see also `find-resource'."
  (let ((val (if (eq :alias (resource-type resource))
		 (resource-data resource)
		 resource)))
    (setf (gethash (resource-name resource)
		   *resource-table*) 
	  val)))

(defvar *executable* nil)

(defvar *module-directories* 
  (list (if *executable*
            (make-pathname :directory (pathname-directory (car #+sbcl sb-ext:*posix-argv*
                                                               #+clozure ccl:*command-line-argument-list*)))
	    (make-pathname :directory 
			   (pathname-directory 
			    (make-pathname
			     :host (pathname-host #.(or *compile-file-truename*
							*load-truename*))
			     :device (pathname-device #.(or *compile-file-truename*
							    *load-truename*))
			     :directory (pathname-directory #.(or *compile-file-truename*
								  *load-truename*)))))))
  "List of directories where XE2 will search for modules.
Directories are searched in list order.")
;; (load-time-value 
;; (or #.*compile-file-truename* *load-truename*))))

(defun find-module-path (module-name)
  "Search the `*module-directories*' path for a directory with the
name MODULE-NAME. Returns the pathname if found, otherwise nil."
  (let ((dirs *module-directories*))
    (message "Probing directories ~S..." dirs)
    (or 
     (loop 
       for dir in dirs for path
	 = (probe-file (make-pathname :directory 
				      (append (pathname-directory
					       dir) (list module-name))
			    :defaults dir))
       when path return path)
     (error "Cannot find module ~s in paths ~S. 
You must set the variable XE2:*MODULE-DIRECTORIES* in the configuration file ~~/.xe2rc
Please see the included file BINARY-README for instructions."
	    module-name dirs))))

(defun find-module-file (module-name file)
  "Make a pathname for FILE within the module MODULE-NAME."
  (merge-pathnames file (find-module-path module-name)))

(defun directory-is-module-p (dir)
  "Test whether a PAK index file exists in a directory."
  (let ((index-filename (concatenate 'string
				     (file-namestring dir)
				     *pak-file-extension*)))
    (probe-file (make-pathname :name index-filename
			       :directory (if (stringp dir)
					      dir
					      (namestring dir))))))

(defun find-modules-in-directory (dir)
  "Search DIR for modules and return a list of their names."
  (let ((dirnames (mapcar #'(lambda (s)
			      (subseq s 0 (1- (length s))))
			  (mapcar #'namestring
				  (directory (concatenate 'string (namestring dir) "/*/"))))))
    (remove-if-not #'directory-is-module-p dirnames)))

(defun find-all-modules ()
  (mapcar #'file-namestring
	  (mapcan #'find-modules-in-directory *module-directories*)))

(defvar *pending-autoload-resources* '())

(defun index-pak (module-name pak-file)
  "Add all the resources from the pak PAK-FILE to the resource
table. File names are relative to the module MODULE-NAME."
  (let ((resources (read-pak pak-file)))
    (dolist (res resources)
      (if (eq :pak (resource-type res))
	  ;; we're including another pak file. if :data is specified,
	  ;; take this as the name of the module where to look for
	  ;; that pak file and its resources.
	  (let ((include-module (or (resource-data res) 
				    module-name)))
	    (index-pak include-module (find-module-file include-module
							(resource-file res))))
	  ;; we're indexing a single resource.
	  (progn
	    (index-resource res)
	    ;; change the file field into a full pathname, for resources
	    ;; that need to load data from an external file later.
	    (when (resource-file res)
	      (setf (resource-file res)
		    (merge-pathnames (resource-file res)
				     (find-module-path module-name))))
	    ;; save the resource name for later autoloading, if needed
	    (when (getf (resource-properties res) :autoload)
	      (push res *pending-autoload-resources*)))))))


(defun index-module (module-name)
  "Add all the resources from the module MODULE-NAME to the resource
table."
  (let ((index-file (find-module-file module-name
				      (concatenate 'string module-name ".pak"))))
    (index-pak module-name index-file)))

;;; Standard resource names

(defvar *startup* ".startup")

(defvar *default-font* ".default-font")

;;; Driver-dependent resource object loading handlers

(defun load-image-resource (resource)
  ;; handle zooming
  (let ((image 
	 (sdl-image:load-image (namestring (resource-file resource)) 
			       :alpha 255)))
    (if (or (= 1 *zoom-factor*)
	    (not (is-zoomed-resource resource)))
	image
	(zoom-image image *zoom-factor*))))

(defun load-bitmap-font-resource (resource)
  (let ((props (resource-properties resource)))
    (if (null props)
	(error "Must set properties for bitmap font.")
	(destructuring-bind (&key width height character-map color-key) props
	  (sdl-gfx:initialise-font (make-instance 'SDL:simple-font-definition
						  :width width :height height
						  :character-map character-map
						  :color-key (apply #'sdl:color color-key)
						  :filename (resource-file resource)
						  :pad-x 0 :pad-y 0))))))
    
(defun load-text-resource (resource)
  (with-open-file (file (resource-file resource)
			:direction :input
			:if-does-not-exist nil)
    (loop for line = (read-line file nil)
	  while line collect line)))

(defun load-formatted-text-resource (resource)
  (read-sexp-from-file (resource-file resource)))
    
(defun load-lisp-resource (resource)
  (let* ((source (resource-file resource))
	 (fasl (compile-file-pathname source)))
    ;; do we need recompilation?
    (if (probe-file fasl)
    	(if (> (file-write-date source)
    	       (file-write-date fasl))
	    ;; recompile. 
    	    (load (compile-file source))
    	    ;; no, just load the fasl
    	    (load fasl))
	;; create the fasl for the first time. 
	(load (compile-file source)))))
	      
(defun load-canvas-resource (resource)
  (destructuring-bind (&key width height background)
      (resource-properties resource)
    (let ((canvas (create-canvas width height))
	  (background (resource-data resource)))
      (prog1 canvas
	(when background
	  (draw-box 0 0 width height 
		    ;; TODO support arbitrary rgb and other drawing commands
		    :color (concatenate 'string "." background)
		    :destination canvas))))))

(defun load-color-resource (resource)
  (destructuring-bind (red green blue)
      (resource-data resource)
    (sdl:color :r red :g green :b blue)))

(defun load-font-resource (resource)
  (let ((font-name (string-upcase (concatenate 'string 
					       "*font-" 
					       (resource-data resource)
					       "*"))))
    (sdl:initialise-font (symbol-value (intern font-name :lispbuilder-sdl)))))

(defun load-music-resource (resource)
  (when *use-sound*
    (sdl-mixer:load-music (namestring (resource-file resource)))))

(defun load-sample-resource (resource)
  (when *use-sound*
    (let ((chunk (sdl-mixer:load-sample (namestring (resource-file resource)))))
      (prog1 chunk
	(when (resource-properties resource)
	  (destructuring-bind (&key volume) (resource-properties resource)
	    (when (numberp volume)
	      (setf (sdl-mixer:sample-volume chunk) volume))))))))

(defvar *resource-handlers* (list :image #'load-image-resource
				  :lisp #'load-lisp-resource
				  :color #'load-color-resource
				  :music #'load-music-resource
				  :bitmap-font #'load-bitmap-font-resource
				  :text #'load-text-resource
				  :formatted-text #'load-formatted-text-resource
				  :sample #'load-sample-resource
				  :canvas #'load-canvas-resource
				  :font #'load-font-resource)
  "A property list mapping resource type keywords to handler functions.
Each function should accept one resource record, and return an
object (possibly driver-dependent). When a resource is loaded (with
`load-resource'), the appropriate handler is looked up, and invoked on
the resource record.  The return value is stored in the OBJECT field
of the record.")

;;; Functions to load, find, and transform resources

(defvar *resource-transformation-delimiter* #\:)

(defun is-transformable-resource (name)
  (eq (aref name 0)
      *resource-transformation-delimiter*))

(defun next-transformation (name)
  (assert (is-transformable-resource name))
  (let ((delimiter-pos (position *resource-transformation-delimiter* 
				 (subseq name 1))))
    (when delimiter-pos 
      (let* ((*read-eval* nil)
	     (xform-command (subseq name 1 (1+ delimiter-pos))))
	(read-from-string (concatenate 'string 
				       "(" 
				       xform-command
				       ")"))))))

(defun next-source (name)
  (assert (is-transformable-resource name))
  (let ((delimiter-pos (position *resource-transformation-delimiter*
				 (subseq name 1))))
    (if (numberp delimiter-pos)
	(subseq name (1+ delimiter-pos))
	(subseq name 1))))

(defun rotate-image (image degrees)
  (sdl:rotate-surface degrees :surface image))

;; (defun reflect-image (image direction)
;;   (sdl:reflect-surface 

(defvar *resource-transformations* 
  (list :rotate #'rotate-image))

(defun load-resource (resource)
  "Load the driver-dependent object of RESOURCE into the OBJECT field
so that it can be fed to the console."
  (message "Attempting to load resource ~S." (resource-name resource))
  (let ((handler (getf *resource-handlers* (resource-type resource))))
    (assert (functionp handler))
    ;; fill in the object field by invoking the handler
    (setf (resource-object resource)
	  (funcall handler resource))
    (if (null (resource-object resource))
	(error "Failed to load resource ~S." (resource-name resource))
	(message "Loaded resource ~S with result ~S." (resource-name resource)
		 (resource-object resource)))))

(defun find-resource (name &optional noerror)
  "Obtain the resource named NAME, performing any necessary loading
and/or transformations. Unless NOERROR is non-nil, signal an error
when NAME cannot be found."
  ;; can we find the resource straight off? 
  (let ((res (gethash name *resource-table*)))
    (cond ((resource-p res)
	   ;; yes, load-on-demand
	   (prog1 res
	     (when (null (resource-object res))
	       (load-resource res))))
	  ;; no, is it an alias?
	  ((stringp res)
	   ;; look up the real one and make the alias map to the real resource
	   (setf (gethash name *resource-table*) 
		 (find-resource res)))
	  ;; not found and not an alias. try to xform
	  ((null res)
	   (if (is-transformable-resource name)
	       ;; ok. let's xform and cache the result
	       (let ((xform (next-transformation name))
		     (source-name (next-source name)))
		 (setf (gethash name *resource-table*) 
		       (if (null xform)
			   (find-resource source-name)
			   (destructuring-bind (operation . arguments) xform
			     (let* ((xformer (getf *resource-transformations* 
						   (make-keyword operation)))
				    (source-res (find-resource source-name))
				    (source-type (resource-type source-res))
				    (source (resource-object source-res))
				    (xformed-resource (apply xformer source
							     arguments)))
			       (make-resource :name name 
					      :type source-type
					      :object xformed-resource))))))
	       ;; can't xform. 
	       (if noerror
		   nil
		   (error "Cannot find resource.")))))))

(defun find-resource-object (name)
  "Obtain the resource object named NAME, or signal an error if not
found."
  (resource-object (find-resource name)))

(defun find-resource-property (resource-name property)
  "Read the value of PROPERTY from the resource RESOURCE-NAME."
  (getf (resource-properties (find-resource resource-name))
	property))

;;; Loading modules as a whole and autoloading resources

(defvar *loaded-modules* nil 
"List of loaded modules.")

(defun load-module (module)
  "Load the module named MODULE."
  (setf *pending-autoload-resources* nil)
  (index-module module)
  (mapc #'load-resource (nreverse *pending-autoload-resources*))
  (setf *pending-autoload-resources* nil))

;;; Playing music and sound effects

(defun set-music-volume (number)
  "Set the mixer music volume between 0 (silent) and 255 (full volume)."
  (when *use-sound*
    (setf (sdl-mixer:music-volume) number)))

(defun play-music (music-name &rest args)
  "Begin playing the music resource MUSIC-NAME. If the resource
MUSIC-NAME has the property :volume, its value is used as the volume
of the music."
  (when *use-sound*
    (let ((resource (find-resource music-name))
	  (volume (find-resource-property music-name :volume)))
      (assert (eq :music (resource-type resource)))
      (set-music-volume (or volume 255))
      (apply #'sdl-mixer:play-music 
	     (resource-object resource)
	     args))))

(defun halt-music (fade-milliseconds)
  "Stop all music playing."
  (when *use-sound*
    (sdl-mixer:halt-music fade-milliseconds)))

;; TODO (defun seek-music 

(defun play-sample (sample-name &rest args)
  "When sound is enabled, play the sample resource SAMPLE-NAME."
  (when *use-sound*
    (let ((resource (find-resource sample-name)))
      (assert (eq :sample (resource-type resource)))
      (apply #'sdl-mixer:play-sample 
	     (resource-object resource)
	     args))))

(defun halt-sample (channel &rest args)
  (when *use-sound*
    (apply #'sdl-mixer:halt-sample :channel channel args)))

;;; Font operations

;; A PAK entry for a font looks like this: 

;; (:name ".default-font" 
;;        :type :font 
;;        :properties (:height 14 :width 7) 
;;        :data "7x14")

;; Right now you can only specify built-in LISPBUILDER-SDL fonts.
;; See also `load-font-resource'.

(defun font-height (font)
  (find-resource-property font :height))

(defun font-width (font)
  (find-resource-property font :width))

(defun font-text-extents (string font)
  (* (length string)
     (font-width font)))

(defun draw-string-solid (string x y 
			  &key destination (font *default-font*) (color ".white"))
  (sdl:draw-string-solid-* string x y :surface destination :font (find-resource-object font)
			   :color (find-resource-object color)))

(defun draw-string-shaded (string x y &optional (foreground ".white") (background ".black")
			  &key destination (font *default-font*))
  (sdl:draw-string-shaded-* string x y (find-resource-object foreground)
			    (find-resource-object background)
			    :surface destination :font (find-resource-object font)))

;;; Other primitives

(defun draw-line (x0 y0 x1 y1 
		     &key 
		     (color ".white")
		     destination)
  (sdl:draw-line-* x0 y0 x1 y1 :surface destination :color (find-resource-object color)))

(defun draw-pixel (x y &key 
		   (color ".white")
		   destination)
  (sdl:draw-pixel-* x y :surface destination :color (find-resource-object color)))

(defun draw-circle (x y radius &key 
		   (color ".white")
		    destination)
  (sdl:draw-circle-* x y radius :surface destination :color (find-resource-object color)))

;;; Standard colors

;; The X11 standard colors are loaded by default into the resource
;; table from the raw data in `*x11-color-data*'. See also rgb.lisp.

(defun initialize-colors ()
  "Load the X11 color data into the resource table."
  (dolist (color *x11-color-data*)
    (destructuring-bind (name red green blue) color
      (index-resource (make-resource :name (concatenate 'string "." name)
				     :type :color
				     :data (list red green blue))))))

;;; Icons

;; An icon is an image that corresponds to a cell method keyword. The
;; expression (icon-image :move) becomes the image ".move".
;; See cells.lisp for a list of keywords.

;; Standard icons for these are in the "standard" module. 

(defun icon-resource (key)
  "Return an icon resource for the key KEY.
The standard GEAR icon is used when no other applicable icon can be
found."
  (or (find-resource (concatenate 'string "."
				  (string-downcase (symbol-name key)))
		     :noerror)
      (find-resource ".gear")))

(defun icon-image (key)
  "Return an icon image name for KEY."
  (resource-name (icon-resource key)))

;;; Creating and displaying images

;; The "driver dependent objects" for XE2 images are just SDL:SURFACE
;; objects. (The situation is the same for XE2 colors, fonts, and so
;; on). So long as the clients treat the driver-dependent resource
;; objects as opaque, this thin wrapper is sufficient.

;; Below are some image handling functions.

(defun create-image (width height)
  "Create a new XE2 image of size (* WIDTH HEIGHT)."
  (sdl:create-surface width height))

(defun draw-image (image x y &key (destination sdl:*default-surface*))
  "Draw the IMAGE at offset (X Y) on the image DESTINATION.
The default destination is the main window."
  (sdl:draw-surface-at-* image x y :surface destination))

(defun draw-resource-image (name x y &key (destination sdl:*default-surface*))
  "Draw the image named by NAME at offset (X Y) on the image DESTINATION.
The default destination is the main window."
  (draw-image (find-resource-object name) x y :destination destination))

(defun image-height (image)
  "Return the height in pixels of IMAGE."
  (sdl:height (find-resource-object image)))

(defun image-width (image)
  "Return the width in pixels of IMAGE."
  (sdl:width (find-resource-object image)))

;;; Drawing shapes

(defun draw-box (x y width height		
		 &key (stroke-color ".white")
		 (color ".black")
		 destination)
  "Draw a filled rectangle at (X Y) of size (* WIDTH HEIGHT)."
  (sdl:draw-box-* x y width height :color (find-resource-object color)
		  :stroke-color (find-resource-object stroke-color)
		  :surface destination))

(defun draw-rectangle (x y width height
		       &key (color ".white")
		       destination)
  (sdl:draw-rectangle-* x y width height :color (find-resource-object color)
			:surface destination))

;;; Audio

(defvar *frequency* 44100)

(defvar *output-chunksize* 1024)

(defvar *output-channels* 2)

(defvar *sample-format* SDL-CFFI::AUDIO-S16LSB)

(defun cffi-sample-type (sdl-sample-type)
  (ecase sdl-sample-type
    (SDL-CFFI::AUDIO-U8 :uint8) ; Unsigned 8-bit samples
    (SDL-CFFI::AUDIO-S8 :int8) ; Signed 8-bit samples
    (SDL-CFFI::AUDIO-U16LSB :uint16) ; Unsigned 16-bit samples, in little-endian byte order
    (SDL-CFFI::AUDIO-S16LSB :int16) ; Signed 16-bit samples, in little-endian byte order
    ;; (SDL-CFFI::AUDIO-U16MSB nil) ; Unsigned 16-bit samples, in big-endian byte order
    ;; (SDL-CFFI::AUDIO-S16MSB nil) ; Signed 16-bit samples, in big-endian byte order
    (SDL-CFFI::AUDIO-U16 :uint16)  ; same as SDL(SDL-CFFI::AUDIO-U16LSB (for backwards compatability probably)
    (SDL-CFFI::AUDIO-S16 :int16) ; same as SDL(SDL-CFFI::AUDIO-S16LSB (for backwards compatability probably)
    (SDL-CFFI::AUDIO-U16SYS :uint16) ; Unsigned 16-bit samples, in system byte order
    (SDL-CFFI::AUDIO-S16SYS :int16) ; Signed 16-bit samples, in system byte order
    ))

(defun cffi-chunk-buffer (chunk)
  (sdl:fp chunk))

(defun convert-cffi-sample (chunk)
  (let* ((input-buffer (cffi-chunk-buffer chunk))
	 (type (cffi-sample-type *sample-format*))
	 (size (length (cffi:mem-ref input-buffer type))))
    (assert (eq *sample-format* SDL-CFFI::AUDIO-S16LSB)) ;; for now
    (let ((output-buffer (make-array size)))
	(prog1 output-buffer
	  (dotimes (n size)
	    (setf (aref output-buffer n)
		  (/ (float (cffi:mem-aref input-buffer type n))
		     32768.0)))))))

;(defun convert-internal-audio (input-buffer output-stream)

;; (REGISTER-MUSIC-MIXER 
;;     (lambda (user stream len)
;;       &#039;FILL-THE-AUDIO-OUTPUT-BUFFER))


;;; Engine status

(defun quit (&optional shutdown)
  (when shutdown 
    (setf *quitting* t))
  (setf *next-module* nil)
  (sdl:push-quit-event))

(defun reset (&optional (module-name "standard"))
  (setf *quitting* nil)
  (setf *next-module* module-name)
  (sdl:push-quit-event))

(defvar *copyright-text*
"XE2 Game Engine
Copyright (C) 2006, 2007, 2008, 2009, 2010 David O'Toole
<dto@gnu.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

This program uses libSDL 1.2 (Simple Direct Media Layer), which is
provided under the terms of the GNU Lesser General Public License. See
also the file LIBSDL-LICENSE for details.
")

(defvar *library-search-paths-setup-hook* nil)

(defun setup-library-search-paths ()
(run-hook '*library-search-paths-setup-hook*)
#+darwin (setf cffi:*foreign-library-directories*
	       (union cffi:*foreign-library-directories*
		      '(#P"/opt/local/lib" #P"/sw/lib/")
		      :test #'equal)))

(defun play (&optional (module-name "standard"))
  "This is the main entry point to XE2. MODULE-NAME is loaded 
and its .startup resource is loaded."
  (format t "~A" *copyright-text*)
  (setf *initialization-hook* nil)
  (setf *random-state* (make-random-state t))
  ;; override module to play?
  (setf *next-module* module-name)
  ;; add library search paths for Mac if needed
  (setup-library-search-paths)
 ;; now play modules until done
  (loop while (and (not *quitting*)
		   *next-module*)
     do (unwind-protect
	     ;; dynamically load libs (needed for GNU/Lpinux)
	     (progn (cffi:define-foreign-library sdl
		      (:darwin (:or (:framework "SDL")
				    (:default "libSDL")))
		      (:unix (:or "libSDL-1.2.so.0.7.2"
				  "libSDL-1.2.so.0"
				  "libSDL-1.2.so"
				  "libSDL.so"
				  "libSDL")))
		    (cffi:use-foreign-library sdl)
		    ;;
		    (cffi:define-foreign-library sdl-mixer
                      (:darwin (:or (:framework "SDL_mixer")
                                    (:default "libSDL_mixer")))
		      (:unix (:or "libSDL_mixer-1.2.so.0.7.2"
				  "libSDL_mixer-1.2.so.0"
				  "libSDL_mixer-1.2.so"
				  "libsdl_mixer-1.2.so.0.2.6" ;; eeebuntu?
				  "libSDL_mixer.so"
				  "libSDL_mixer")))
		    (cffi:use-foreign-library sdl-mixer)
		    ;;
		    (cffi:define-foreign-library sdl-gfx
                      (:darwin (:or (:framework "SDL_gfx")
                                    (:default "libSDL_gfx")))
		      (:unix (:or "libSDL_gfx-1.2.so.0.7.2"
				  "libSDL_gfx-1.2.so.0"
				  "libSDL_gfx-1.2.so"
				  "libSDL_gfx.so.4"
				  "libSDL_gfx.so.13"
				  "libSDL_gfx.so"
				  "libSDL_gfx")))
		    (cffi:use-foreign-library sdl-gfx)
		    ;;
		    (cffi:define-foreign-library sdl-image
                      (:darwin (:or (:framework "SDL_image")
                                    (:default "libSDL_image")))
		      (:unix (:or "libSDL_image-1.2.so.0.7.2"
				  "libSDL_image-1.2.so.0"
				  "libSDL_image-1.2.so.0.1.5" ;; eeebuntu?
				  "libSDL_image-1.2.so"
				  "libSDL_image.so"
				  "libSDL_image")))
		    (cffi:use-foreign-library sdl-image)
		    ;;
		    (sdl:with-init (sdl:SDL-INIT-VIDEO sdl:SDL-INIT-AUDIO sdl:SDL-INIT-JOYSTICK)
		      (load-user-init-file)	
		      (initialize-resource-table)
		      (initialize-colors)
		      (when *use-sound*
			;; try opening sound
			(when (null (sdl-mixer:open-audio :frequency *frequency*
							  :chunksize *output-chunksize*
							  :format *sample-format*
							  :channels *output-channels*))
			  ;; if that didn't work, disable effects/music
			  (message "Could not open audio driver. Disabling sound effects and music.")
			  (setf *use-sound* nil))
			;; set to mix lots of sounds
			(sdl-mixer:allocate-channels *channels*))
		      (index-module "standard") 
		      (load-module *next-module*)
		      (run-main-loop)))
	  ;; close audio if crash
	  (when *use-sound* 
	    (sdl-mixer:close-audio t)))
	  (setf *quitting* t))
	(setf *quitting* nil)
	(when *use-sound* 
    (sdl-mixer:close-audio t)))
  ;; ;; free audio
  ;; (maphash #'(lambda (name resource)
  ;; 	       (declare (ignore name))
  ;; 	       (when (eq :music (resource-type resource))
  ;; 		 (sdl-mixer:free (resource-object resource))))
  ;; 	   *resource-table*))


;;; Saving and loading data 
;;; Taking screenshots

;;; console.lisp ends here
