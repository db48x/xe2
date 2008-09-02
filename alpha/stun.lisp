;;; stun.lisp --- a graphical shell for the X Window System

;; Copyright (C) 2006, 2007, 2008  David O'Toole
;;
;; Author: David O'Toole <dto@gnu.org>
;; Keywords: multimedia, tools, lisp, panels, unix
;; Version: $Id: stun.lisp,v 1.2 2007/10/12 02:35:10 dto Exp $
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; STUN is a graphical shell for STUMPWM. Like most shells, STUN can
;; interact with the user, launch programs, and control external
;; processes. Unlike most shells, STUN is also an alternative
;; graphical workspace toolkit for Common Lisp.

;;; Code:

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (require :clx))

(defpackage stun
  (:documentation "A graphical shell for the X Window System.")
  (:use :common-lisp)
  (:export widget frame worksheet connection port dataflow listener toolbar
	   textbox template stun))

(in-package :stun)			 

(defvar *display* nil)

(defun initialize-display (&optional force)
  (when (or force (null *display*))
    (setf *display* (xlib:open-default-display))))

(defun message (control-string &rest args)
  (apply #'format t control-string args)
  (fresh-line))

;;; Widgets

;; Widgets are the things that STUN panels display and edit. This is
;; the base class for interactive graphical elements in STUN.

;; Widgets are user interface elements that represent objects in a
;; problem domain. Widgets may have child widgets, and so on. Widgets
;; have a position within the parent widget and a label to be used
;; when appearing in a composition of widgets.  

(defclass widget ()
  ((parent :accessor parent :initform nil :initarg :parent)
   (children :accessor children :initform nil :initarg :children)
   (label :accessor label :initform "()" :initarg :label)
   (position-x :accessor position-x :initform 0 :initarg :position-x)
   (position-y :accessor position-y :initform 0 :initarg :position-y)
   (height :accessor height :initform 0 :initarg :height)
   (width :accessor width :initform 0 :initarg :width)))

(defgeneric default-map-key (widget key-spec) 
  (:documentation "When a class keymap lookup on WIDGET fails, the
keybinding system calls this method before checking the parent
WIDGET's class keymap table. The default is to do nothing; return
non-nil if you handle the KEY-SPEC."))

(defmethod default-map-key ((w widget) key-spec)
  nil)

(defgeneric touch (widget x y)
  (:documentation "The widget should respond to being touched at 
position X,Y. The meaning of being touched is up to the subclass."))

(defmethod touch ((w widget) x y)
  nil)

(defgeneric cursor-key (widget)
  (:documentation "Return a keyword representing the cursor to be displayed on
mouseover. Examples are :cursor, :touch-cursor, :join-cursor
etc."))

(defmethod cursor-key ((w widget))
  :cursor)

(defgeneric join-widgets (source sink &optional x y)
  (:documentation "Operate on the SOURCE and SINK widgets. The actual
operation to occur is determined by the classes of SOURCE and SINK."))

(defmethod join-widgets ((source widget) (sink widget) &optional x y)
  nil)

(defgeneric adjoin-child (parent child)
  (:documentation "Add CHILD to the widget P's children."))

(defmethod adjoin-child ((p widget) (child widget))
  (setf (children p) (adjoin child (children p)))
  (setf (parent child) p))

(defgeneric remove-child (parent child)
  (:documentation "Remove CHILD from the widget P's children."))

(defmethod remove-child ((p widget) (child widget))
  (setf (children p) (remove child (children p))))

(defgeneric move (widget x y)
  (:documentation "Reposition the widget W within the worksheet."))

(defmethod move ((w widget) x y)
  (setf (position-x w) x)
  (setf (position-y w) y))

(defun within-extents (x y x0 y0 x1 y1)
  (and (>= x x0) 
       (<= x x1)
       (>= y y0)
       (<= y y1)))
  
(defgeneric hit-test (widget x y)
  (:documentation "Return W when the position (x,y) is within the
bounding rectangle for the widget W, nil
otherwise. Non-rectangular widgets or widgets with clickable
subcomponents should override this method."))

(defmethod hit-test ((w widget) x y)
  (with-slots (position-x position-y height width) w
    (if (within-extents x y 
			position-x position-y 
			(+ position-x width)
			(+ position-y height))
	w
	nil)))
    
(defun hit-widgets (widgets x y)
  (some #'(lambda (w)
	    (hit-test w x y))
	(reverse widgets)))

(defun hit-widgets-or-parent (widgets parent x y)
  (or (hit-widgets widgets x y) parent))

;;; Keymaps: binding keypresses to CLOS methods

;; A keypress is a triple of the form (key keysym modifiers).  
;;
;; A keymap is a hash table mapping normalized keypresses (see
;; `normalize-key') to functions accepting a single widget argument.
;; The function `map-key' looks in a widget's class keymap (and then
;; the keymaps of its parent widgets) in order to find a suitable
;; handler function. See also `define-key'.

(defvar *class->keymap* nil "Hash table mapping class names to keymaps.")

(defun initialize-keymap-table ()
  (setf *class->keymap* (make-hash-table)))

(defun class-keymap (class-name)
  (gethash class-name *class->keymap*))

(defun set-class-keymap (class-name keymap)
  (setf (gethash class-name *class->keymap*) keymap))

(defsetf class-keymap set-class-keymap)

(defun normalize-key (key-spec)
  (destructuring-bind (key keysym modifiers) key-spec
    (list key keysym (sort (remove-duplicates modifiers)
			   #'string<
			   :key #'symbol-name))))

(defun make-class-keymap ()
  (make-hash-table :test 'equal))
    
(defun class-key-binding (class-name key-spec)
  ;; create if necessary
  (when (null (class-keymap class-name))
    (setf (class-keymap class-name)
	  (make-class-keymap)))
  ;; look up the binding
  (gethash (normalize-key key-spec)
	   (class-keymap class-name)))
  
(defun set-class-key-binding (class-name key-spec func) 
  ;; create if necessary
  (when (null (class-keymap class-name))
    (setf (class-keymap class-name)
	  (make-class-keymap)))
  ;; change the binding
  (setf (gethash (normalize-key key-spec)
		 (class-keymap class-name))
	func))
    
(defsetf class-key-binding set-class-key-binding)

;; Main user functions for defining and looking up key bindings.

(defun define-key (class-name key-spec func)
  (destructuring-bind (&key key keysym modifiers) key-spec
    (setf (class-key-binding class-name (list key keysym modifiers)) func)))

(defun map-key (widget key-spec)
  (let ((binding (class-key-binding (class-name (class-of widget))
				    key-spec)))
    (if binding
	(funcall binding widget)
	(when (and (null (default-map-key widget key-spec))
		   (parent widget))
	  (map-key (parent widget) key-spec)))))

;;; Panels: X windows full of STUN widgets

;; A panel is an X window for viewing and interacting with widgets.
;;
;; A panel has one associated main widget. This widget's label is taken
;; as the title of the panel. The widget's children are displayed in
;; the panel for interaction purposes.
;;
;; Several user actions can occur. The user can drag a widget by
;; holding shift and the left mouse button. This causes the "move"
;; method to be invoked on the widget.
;;
;; The user can join two widgets by right-mouse-dragging one onto the
;; other. This causes the "join-widgets" method to be invoked with the
;; two widgets as the source and sink arguments.
;;
;; The user can click a widget with the left mouse button. This
;; causes the "touch" method to be invoked on the widget, and also
;; causes keyboard focus to move to the widget.
;;
;; The user can type with the keyboard into the focused widget. The
;; method name to be invoked is looked up in the widget's keymap.

(defparameter *panel-font* "7x14")
(defparameter *panel-height* 20)
(defparameter *panel-position* :bottom)
(defparameter *panel-padding* 1)

(defvar *window->panel* "Hash table mapping X window ID's to panel objects.")

(defun find-panel (window)
  (gethash window *window->panel*))

(defclass panel ()
  (;; the main widget   
   (widget :accessor widget :initform nil :initarg :widget)
   (location :accessor location :initform :bottom :initarg :location)
   ;; the widget being dragged, if any
   (dragging :accessor dragging :initform nil)
   ;; the widget being joined to another, if any
   (joining :accessor joining :initform nil)
   ;; the widget having keyboard focus, if any
   (focusing :accessor focusing :initform nil)
   ;; CLX-related resources
   (screen :accessor screen :initform nil)
   (colormap :accessor colormap :initform nil)
   (foreground :accessor foreground :initform nil)
   (background :accessor background :initform nil)
   (context :accessor context :initform nil :initarg :context)
   (highlight-context :accessor highlight-context :initform nil)
   (accent-context :accessor accent-context :initform nil)
   (border-context :accessor border-context :initform nil)
   (shadowed-context :accessor shadowed-context :initform nil)
   (active-context :accessor active-context :initform nil)
   (clear-context :accessor clear-context :initform nil)
   (cursor :accessor cursor :initform nil :initarg :cursor)
   (join-cursor :accessor join-cursor :initform nil :initarg :join-cursor)
   (touch-cursor :accessor touch-cursor :initform nil :initarg :touch-cursor)
   (font :accessor font :initform nil :initarg :font)
   (window :accessor window :initform nil :initarg :window)
   (canvas :accessor canvas :initform nil :initarg :canvas)))

;;; Loading some pre-defined X cursors

(defconstant arrow-cursor-id 132)
(defconstant circle-cursor-id 24)
(defconstant hand-cursor-id 60)

(defun X-predefined-cursor (panel cursor-id)
  "Load and return one of the predefined X cursors."
  (let ((font (xlib:open-font *display* "cursor")))
    (setf (xlib:window-cursor (window panel))
	  (xlib:create-glyph-cursor :source-font font
			       :source-char cursor-id
			       :mask-font font
			       :mask-char (1+ cursor-id)
			       :foreground 
			       (xlib:make-color :red 1.0 :green 1.0 :blue 1.0)
			       :background 
			       (xlib:make-color :red 0.0 :green 0.0 :blue 0.0)))))

;;; Stumpwm integration

(defmethod change-window-type ((p panel) window-type)
  (xlib:change-property (window p) 
			:_NET_WM_WINDOW_TYPE (vector (xlib:find-atom *display* window-type))
			:atom 32))

(defmethod set-sticky ((p panel))
  (xlib:change-property (window p) 
			:_NET_WM_STATE (vector (xlib:find-atom *display* "_NET_WM_STATE_STICKY"))
			:atom 32))

(defmethod init-stumpwm-commands ((p panel))
  ;; we want stumpwm responses from root window
  (setf (xlib:window-event-mask (xlib:screen-root (screen p)))
	'(:property-change)))

(defmethod stumpwm-command ((p panel) command)
  (xlib:change-property (xlib:screen-root (screen p))
			:stumpwm_command command :string 8
			:transform 'char-code))

(defmethod end-stumpwm-command ((p panel))
  (xlib:delete-property (xlib:screen-root (screen p))
			:stumpwm_command))

;;; Customizing colors

(defparameter *foreground-color* '(:red 0.8 :green 0.8 :blue 0.8))
(defparameter *background-color* '(:red 0.0 :green 0.0 :blue 0.0))
(defparameter *accent-color* '(:red 1.0 :green 1.0 :blue 1.0))
(defparameter *highlight-color* '(:red 0.92 :green 0.0 :blue 0.0))
(defparameter *shadowed-color* '(:red 0.7 :green 0.7 :blue 0.7))
(defparameter *active-color* '(:red 0.3 :green 0.7 :blue 0.8))
(defparameter *border-color* '(:red 0.4 :green 0.4 :blue 0.4))

(defun color-from-param (color-plist)
  (xlib:alloc-color (xlib:screen-default-colormap (xlib:display-default-screen *display*))
		    (apply #'xlib:make-color color-plist)))

;;; Creating panels

(defmethod initialize-instance :after ((f panel) &rest initargs)
  "Initialize a new panel on the default display."
  (with-slots (screen colormap foreground context canvas
		      cursor join-cursor touch-cursor
		      highlight-context accent-context border-context
		      shadowed-context active-context clear-context
		      background widget font window) f
    (setf screen (xlib:display-default-screen *display*))
    (setf colormap (xlib:screen-default-colormap screen))
    (setf foreground (color-from-param *foreground-color*))
    (setf background (color-from-param *background-color*))
    (setf window (xlib:create-window
		  :parent (xlib:screen-root screen)
		  :x 0
		  :y 0
		  :height *panel-height*
		  :width *panel-height*
		  :background background
		  :border foreground
		  :border-width 0
		  :backing-store :when-mapped
		  :colormap colormap
		  :bit-gravity :center
		  :depth (xlib:drawable-depth (xlib:screen-root screen))
		  :class :input-output
;		  :override-redirect :on
		  :event-mask '(:exposure :button-press :key-press
				:button-release :pointer-motion)))
    (message "Changing window type.")
    (change-window-type f "_NET_WM_WINDOW_TYPE_DOCK")
    (set-sticky f)
    (xlib:display-finish-output *display*)
    (init-stumpwm-commands f)
    (setf canvas (new-canvas f window))
    (setf font (xlib:open-font *display* *panel-font*))
    (setf context (xlib:create-gcontext :drawable canvas
				   :foreground foreground
				   :background background
				   :font font))
    (setf accent-context (xlib:create-gcontext :foreground (color-from-param *accent-color*)
					       :line-width 2
					  :background background
					  :drawable canvas
					  :font font))
    (setf border-context (xlib:create-gcontext :foreground (color-from-param *border-color*)
					       :line-width 1
					  :background background
					  :drawable canvas
					  :font font))
    (setf highlight-context (xlib:create-gcontext :foreground (color-from-param *highlight-color*)
					     :background background
					     :drawable canvas
					     :font font))
    (setf shadowed-context (xlib:create-gcontext :foreground (color-from-param *shadowed-color*)
					    :line-style :dash
					    :dashes '(2 2)
					    :background background
					    :drawable canvas
					    :font font))
    (setf active-context (xlib:create-gcontext :foreground (color-from-param *active-color*)
					  :background background
					  :drawable canvas
					  :font font))
    (setf clear-context (xlib:create-gcontext :foreground background
					 :background foreground
					 :drawable canvas
					 :font font))
    (setf cursor (X-predefined-cursor f arrow-cursor-id))
    (setf join-cursor (X-predefined-cursor f circle-cursor-id))
    (setf touch-cursor (X-predefined-cursor f hand-cursor-id))
    ;; set window properties
    (xlib:set-wm-properties window
		       :name 'hello-world
		       :icon-name "hello-world"
		       :resource-name "hello-world"
		       :resource-class 'hello-world
		       :initial-state :normal)
    ;; set type and map the window
    (xlib:map-window window)
    ;; save the panel so that we can look it up later
    (setf (gethash window *window->panel*) f)
    f))

(defparameter *widget-horizontal-margin* 4)
(defparameter *widget-vertical-margin* 2)
(defparameter *widget-minimum-width* 40)

(defun X-default-render-widget (widget drawable context font)
  "Render the WIDGET with default X appearance to DRAWABLE with
gcontext CONTEXT and font FONT."
  (message "X-default-render-widget")
  (with-slots (position-x position-y label height width) widget
    ;; calculate size of widget based on font
    (setf width (max *widget-minimum-width*
		     (+ (* 2 *widget-horizontal-margin* )
			(xlib:text-extents font label))))
    (setf height (+ (* 2 *widget-vertical-margin*)
		    (xlib:font-ascent font) (xlib:font-descent font)))
    ;; now draw
    (xlib:draw-rectangle drawable context 
		    position-x position-y
		    width height)
    (xlib:draw-glyphs drawable context 
		 (+ 2 position-x)
		 (+ 2 (xlib:font-ascent font) position-y)
		 label)))
    
(defgeneric render-widget (panel widget)
  (:documentation "Render the widget to the panel with default
appearance. Different widget subclasses that need different
appearances should override this method."))

(defmethod render-widget ((f panel) (w widget))
  (with-slots (context canvas font) f
    (X-default-render-widget w canvas context font)))

(defgeneric render (panel)
  (:documentation "Redraw the widgets in the panel to the panel's
associated window."))

(defmethod render ((f panel))
  (format t "Rendering panel...")
  (fresh-line)
  (with-slots (canvas widget context clear-context window) f
    (format t "Clearing background.")
    (fresh-line)
    ;; clear background
    (xlib:draw-rectangle canvas clear-context 0 0 
			 (xlib:drawable-width window)
			 (xlib:drawable-height window)
			 :fill)
    (format t "Rendering widgets.")
    (fresh-line)
    ;; render widgets 
    (dolist (child (children (widget f)))
      (format t " * ")
      (render-widget f child))
    (fresh-line)
    (format t "Copying to screen.")
    ;; copy back buffer to window
    (xlib:copy-area canvas context 0 0 
		    (xlib:drawable-width window)
		    (xlib:drawable-height window)
		    window 0 0)))

(defmethod new-canvas ((p panel) window &optional 
			  (height (xlib:drawable-height (xlib:screen-root (screen p))))
			  (width (xlib:drawable-width (xlib:screen-root (screen p)))))
  (xlib:create-pixmap :width width
		      :height height
		      :depth (xlib:drawable-depth (xlib:screen-root (screen p)))
		      :drawable window))

(defmethod auto-resize ((p panel))
  (with-slots (window canvas location screen) p
    ;; TODO support other locations besides :bottom
	(let* ((head-width (xlib:screen-width screen))
	       (head-height (xlib:screen-height screen))
	       (head-y 0)
	       (head-x 0)
	       (panel-height *panel-height*))
	  (setf (xlib:drawable-width window) head-width
		(xlib:drawable-height window) panel-height
		(xlib:drawable-x window) head-x
		(xlib:drawable-y window) (- (+ head-y head-height) panel-height))
	  (setf canvas (new-canvas p window head-height head-width)))))

;;; User actions

(defgeneric click (panel x y)
  (:documentation "Respond to a mouse click from the user at
point X,Y. The default action is to 'touch' the widget at that
position."))

(defmethod click ((f panel) x y)
  (let* ((widgets (children (widget f)))
	 (widget (hit-widgets-or-parent widgets (widget f) x y)))
    (when widget
      (setf (focusing f) widget)
      (touch widget x y))))

(defgeneric start-dragging (panel x y)
  (:documentation "Begin dragging the selected widget."))

(defmethod start-dragging ((f panel) x y)
  (let* ((widgets (children (widget f)))
	 (widget (hit-widgets widgets x y)))
    (setf (dragging f) widget)
    (setf (focusing f) widget)))

(defgeneric stop-dragging (panel)
  (:documentation "Stop dragging the selected widget."))

(defmethod stop-dragging ((f panel))
  (setf (dragging f) nil))

(defgeneric start-joining (panel x y)
  (:documentation "Begin joining two widgets."))

(defmethod start-joining ((f panel) x y)
  (let ((widgets (children (widget f))))
    (setf (joining f) (hit-widgets widgets x y))))

(defgeneric stop-joining (panel x y)
  (:documentation "Join the selected widgets."))

(defmethod stop-joining ((f panel) x y)
  (let ((source (joining f))
	(sink (hit-widgets-or-parent (children (widget f)) (widget f) x y)))
    (when (and source sink)
      (join-widgets source sink x y))
    (setf (joining f) nil)))

(defgeneric set-cursor (panel cursor)
  (:documentation "Set the cursor type for the given panel."))

(defmethod set-cursor ((f panel) cursor)
  (setf (window-cursor (window f)) cursor))

;;; The X event loop

(defun run-panels ()
  (unwind-protect 
       (xlib:event-case (*display* :discard-p t :force-output-p t)
	 (exposure
	  (window)
	  (message "Running panels...")
	  (let ((panel (find-panel window)))
	    (when panel
	      (render panel)
	      nil)))
	 ;;
	 (button-release 
	  (window state)
	  (let ((panel (find-panel window))
		(state-keys (xlib:make-state-keys state)))
	    (when panel
	      (multiple-value-bind (x y)
		  (xlib:pointer-position window)
		(cond 
		  ((member :button-1 state-keys)
		   (stop-dragging panel)
		   (render panel))
		  ((member :button-3 state-keys)
		   (stop-joining panel x y)
		   (render panel))))))
	  nil)
	 ;;
	 (button-press
	  (window)
	  (multiple-value-bind (x y s c state) 
	      (xlib:query-pointer window)
	    (let ((panel (find-panel window))
		  (state-keys (xlib:make-state-keys state)))
	      (when panel
		(multiple-value-bind (x y)
		    (xlib:pointer-position window)
		  (cond 
		    ((subsetp '(:shift :button-1) state-keys)
		     (click panel x y))
		    ((member :button-1 state-keys)
		     (start-dragging panel x y))
		    ((member :button-3 state-keys)
		     (start-joining panel x y)))))))
	  nil)
	 ;;
	 (key-press
	  (window code state)
	  (let* ((panel (find-panel window))
		 (state-keys (xlib:make-state-keys state))
		 (widget (or (focusing panel) (widget panel)))
		 (keysym (xlib:keycode->keysym *display* 
					       code (if (member :shift state-keys)
							1
							0)))
		 (key (xlib:keysym->character *display* keysym)))
	    (when widget
	      (map-key widget key keysym state-keys)
	      (render panel)))
	  nil)
	 ;;
	 (motion-notify
	  (window button)
	  (multiple-value-bind (x y)
	      (xlib:pointer-position window)
	    (let* ((panel (find-panel window))
		   (widgets (children (widget panel)))
		   (dragged-widget (dragging panel))
		   (joined-widget (joining panel)))
	      (cond
		((and dragged-widget panel)
		 (move dragged-widget x y)
		 (render panel))
		;;
		((and joined-widget panel)
		 nil))
	      ;; hit-test to see what cursor we should use. 
	      (let ((cursor 
		     (let ((widget (hit-widgets widgets x y)))
		       (if widget
			   (case (cursor-key widget)
			     (:cursor (cursor panel))
			     (:join-cursor (join-cursor panel))
			     (:touch-cursor (touch-cursor panel)))
			   (cursor panel)))))
		(setf (xlib:window-cursor window) cursor))))
 	  nil))
    (xlib:close-display *display*)))

;;; Textboxes
                                           
(defvar *textbox-margin* 4 "Default onscreen margin of a textbox.")

(defclass textbox (widget) 
  ((buffer :accessor buffer :initform nil :initarg :buffer)
   (point-row :accessor point-row :initform 0 :initarg :point-row)
   (point-column :accessor point-column :initform 0 :initarg :point-column)))

(defmethod render-widget ((f panel) (box textbox))
  (with-slots (window canvas context font highlight-context focusing) f
    (with-slots (position-x position-y height width 
			    buffer point-row point-column) box
      (let* ((font-height (+ 2 (xlib:font-ascent font) (xlib:font-descent font))))
	;; update textbox geometry
	(let ((line-lengths (mapcar (lambda (s)
				      (xlib:text-extents font s))
				    buffer)))
	  (setf width (max *widget-minimum-width*
			   (+ (* 2 *textbox-margin*)
			      (if (null line-lengths)
				  0 (apply #'max line-lengths))))))				      
	(setf height (+ (* 2 *textbox-margin*)
			(* font-height (max 1 (length buffer)))))
	;; draw border
	(xlib:draw-rectangle canvas context 
			     position-x position-y
			     width height)
	;; draw buffer
	(let ((x (+ position-x *textbox-margin*))
	      (y (+ -2 position-y *textbox-margin*)))
	  (dolist (line buffer)
	    (incf y font-height)
	    (xlib:draw-glyphs canvas context x y line)))
	;; draw cursor
	(when (eq focusing box)
	  (let* ((line (nth point-row buffer))
		 (cursor-width (xlib:text-extents font " "))
		 (x (+ position-x *textbox-margin*
		       (xlib:text-extents font (subseq line 0 point-column))))
		 (y (+ 2 position-y *textbox-margin*
		       (* font-height point-row))))
	    (xlib:draw-rectangle canvas highlight-context 
				 x y cursor-width font-height t)))))))

(defmethod default-map-key ((box textbox) key-spec)
  (destructuring-bind (key keysym modifiers) key-spec
    (when (typep key 'standard-char)
      (insert-key box key))
    ;; return true to notify keymapper that we've handled the event
    t))

(defmethod forward-char ((box textbox))
  (with-slots (buffer point-row point-column) box
    (setf point-column (min (1+ point-column)
			    (length (nth point-row buffer))))))

(defmethod backward-char ((box textbox))
  (with-slots (buffer point-row point-column) box
    (setf point-column (max 0 (1- point-column)))))

(defmethod next-line ((box textbox))
  (with-slots (buffer point-row point-column) box
    (setf point-row (min (1+ point-row)
			 (1- (length buffer))))
    (setf point-column (min point-column 
			    (length (nth point-row buffer))))))

(defmethod previous-line ((box textbox))
  (with-slots (buffer point-row point-column) box
    (setf point-row (max 0 (1- point-row)))
    (setf point-column (min point-column
			    (length (nth point-row buffer))))))

(defmethod move-end-of-line ((box textbox))
  (with-slots (buffer point-row point-column) box
    (setf point-column (length (nth point-row buffer)))))

(defmethod move-beginning-of-line ((box textbox))
  (setf (point-column box) 0))

(defmethod newline ((box textbox))       
  (with-slots (buffer point-row point-column) box
    ;;  insert line break
    (let* ((line (nth point-row buffer))
	   (line-remainder (subseq line point-column))
	   (buffer-remainder (nthcdr (1+ point-row) buffer)))
      ;; truncate current line
      (setf (nth point-row buffer) 
	    (subseq line 0 point-column))
      ;; insert new line
      (if (= 0 point-row)
	  (setf (cdr buffer)
		(cons line-remainder (cdr buffer)))
	  (setf (cdr (nthcdr (- point-row 1) buffer))
		(cons (nth point-row buffer)
		      (cons line-remainder buffer-remainder))))
      ;;
      (incf point-row)			
      (setf point-column 0))))

(defmethod backward-delete-char ((box textbox))       
  (with-slots (buffer point-row point-column) box
    (if (and (= 0 point-column) (/= 0 point-row))
	(progn 
	  ;;
	  ;; we need to remove a line break.
	  (let ((line (nth (- point-row 1) buffer))
		(next-line (nth (+ point-row 1) buffer)))
	    (setf (nth (- point-row 1) buffer)
		  (concatenate 'string line (nth point-row buffer)))
	    (setf (cdr (nthcdr (- point-row 1) buffer))
		  (nth (+ point-row 1) buffer))
	    ;; move cursor too
	    (decf point-row)
	    (setf point-column (length line))))
	(progn
	  ;; otherwise, delete within current line.
	  (when (/= 0 point-column)
	    (let* ((line (nth point-row buffer))
		   (remainder (subseq line point-column)))
	      (setf (nth point-row buffer)
		    (concatenate 'string 
				 (subseq line 0 (- point-column 1))
				 remainder))
	      (decf point-column)))))))

(defmethod insert-key ((box textbox) key)       
  (with-slots (buffer point-row point-column) box
    (if (null buffer)
	(progn
	  (push (string key) buffer)
	  (incf point-column))
	(progn
	  (let* ((line (nth point-row buffer))
		 (remainder (subseq line point-column)))
	    (setf (nth point-row buffer)
		  (concatenate 'string
			       (subseq line 0 point-column)
			       (string key)
			       remainder)))
	  (incf point-column)))))

;;; Buttons

;; A button evaluates the lisp expression inside when you click on it.

(defclass button (textbox) ())

(defmethod render-widget ((f panel) (b button))
  (with-slots (context canvas font) f
    (X-default-render-widget b canvas context font)))

(defmethod touch ((b button) x y)
  (with-slots (label) b
    (handler-case
	(eval (read-from-string label))
      ;; print any errors to standard output for now
      (condition (c) (format t "~S" c)))))

;;; Templates

;; A template allows you to create new objects within a worksheet.

(defclass template (widget) ())

(defmethod render-widget ((f panel) (tmp template))
  (with-slots (shadowed-context canvas font) f
    (X-default-render-widget tmp canvas shadowed-context font)))

(defmethod cursor-key ((tem template))
  :join-cursor)

;;; Worksheets

;; Worksheets are used to organize widgets into a page.

(defclass worksheet (widget) ())

(defmethod join-widgets ((tmp template) (wrk worksheet) &optional x y)
  "Create a new widget of the class indicated by template TMP
in worksheet WRK at location X Y."
  (let* ((class-symbol (intern (string-upcase (label tmp))))
	 (widget (make-instance class-symbol 
				:label (concatenate 'string
						    "*new " 
						    (label tmp)
						    "*")
				:position-x x
				:position-y y
				:parent wrk)))
    (adjoin-child wrk widget)))

;;; Toolbars

;; A toolbar full of widgets is displayed across the top of the panel.

(defclass toolbar (widget) ())

(defparameter *toolbar-margin* 2)

(defmethod render-widget ((f panel) (b toolbar))
  (format t "Rendering widgets...")
  (with-slots (window canvas accent-context font) f
    (let ((toolbar-height (+ 4
			     (* 2 *toolbar-margin*) 
			     (* 2 *widget-vertical-margin*)
			     (xlib:font-ascent font)
			     (xlib:font-descent font))))
      ;; update toolbar geometry 
      (with-slots (position-x position-y height width) b
	(setf position-x 0)
	(setf position-y 0)
	(setf height toolbar-height)
	(setf width (xlib:drawable-width window)))
      ;;
      ;; draw toolbar border
      (xlib:draw-line canvas accent-context
		      0 toolbar-height
		      (xlib:drawable-width window) toolbar-height)
      ;;
      ;; position and render children
      (let ((x *toolbar-margin*))
	(dolist (child (children b))
	  (setf (position-x child) x)
	  (setf (position-y child) *toolbar-margin*)
	  (render-widget f child)
	  (incf x (+ *toolbar-margin* (width child))))))))

(defmethod hit-test ((b toolbar) x y)
  (hit-widgets (children b) x y))

;;; Lisp Listener

;; A listener gives you the read-eval-print loop at the bottom of the panel.

(defparameter *listener-lines* 5 "Number of lines to display in listener.")
(defparameter *listener-margin* 5 "Size of margins in listener.") 

(defclass listener (textbox)
  ((history-position :accessor history-position :initform 0 
		     :initarg :history-position)))

(defmethod add-listener ((f panel))
  (let ((listener (make-instance 'listener)))
    (adjoin-child (widget f) listener)))

(defmethod render-widget ((f panel) (L listener))
  (with-slots (window canvas highlight-context accent-context font focusing) f
    (xlib:with-state (window)
      (with-slots (position-x position-y height width 
			      buffer point-row point-column) L
	(let* ((font-height (+ 2 (xlib:font-ascent font) (xlib:font-descent font)))
	       (font-width (xlib:text-extents font "a"))
	       (listener-height (+ 4 
				   (* 2 *listener-margin*)
				   (* *listener-lines* font-height))))
	  ;;
	  ;; update listener geometry
	  (setf position-y (- (xlib:drawable-height window)
			      listener-height))
	  (setf position-x 0)
	  (setf width (xlib:drawable-width window))
	  (setf height listener-height)
	  ;;
	  ;; draw border
	  (xlib:draw-line canvas accent-context 
			  position-x position-y 
			  (xlib:drawable-width window) position-y)
	  ;;
	  ;; draw text lines
	  (let ((y (- (xlib:drawable-height window)
		      *listener-margin*
		      )))
	    (dotimes (i *listener-lines*)
	      (xlib:draw-glyphs canvas accent-context 
				*listener-margin* y
				(nth i buffer))
	      (decf y font-height)))
	  ;;
	  ;; draw cursor
	  (when (eq focusing L)
	    (xlib:draw-rectangle canvas highlight-context 
				 (+ *listener-margin*
				    (* point-column font-width))
				 (- (xlib:drawable-height window)
				    *listener-margin*
				    font-height)
				 font-width font-height t)))))))

(defmethod evaluate ((L listener))
  (with-slots (buffer point-row point-column history-position) L
    (setf point-row 0)
    (setf history-position 0)
    (setf point-column 0)
    (push (concatenate 'string " "
		       (handler-case 
			   (prin1-to-string 
			    (eval (read-from-string (car buffer))))
			 (condition (c) (format nil "~S" c))))
	  buffer)
    (push "" buffer)))

(defmethod previous-history ((L listener))
  (with-slots (buffer history-position point-column) L
    (setf history-position (min (1+ history-position)
				(length buffer)))
    (setf (car buffer) (copy-seq (nth history-position buffer)))
    (setf point-column (length (car buffer)))))

(defmethod next-history ((L listener))
  (with-slots (buffer history-position point-row point-column) L
    (setf history-position (max 0 (1- history-position)))
    (setf (car buffer) (copy-seq (nth history-position buffer)))
    (setf point-column (length (car buffer)))))

;;; Dataflow

;; These subclasses implement an abstract dataflow interface.

;; Dataflow widgets contain inlet ports and outlet ports.
;; Ports can connect to one or more connections.
;; Connections have a source port and a sink port. 

;;;; Connections link together two ports 

(defclass connection (widget)
  ((source :accessor source :initform nil :initarg :source)
   (sink :accessor sink :initform nil :initarg :sink)
   (handle :accessor handle :initform nil :initarg :handle)))

(defmethod disconnect ((c connection))
  (with-slots (source sink) c
    (let ((parent (parent (parent source))))
      (remove-connection source c)
      (remove-connection sink c)
      (remove-child parent c))))

(defmethod endpoints ((c connection))
  (with-slots (source sink) c
    (let ((x0 (port-extents-x (parent source) 
			      (port-number source) 
			      (num-outlets (parent source))))
	  (y0 (port-extents-y (parent source) :outlet-p))
	  (x1 (port-extents-x (parent sink) 
			      (port-number sink) 
			      (num-inlets (parent sink))))
	  (y1 (+ *port-height* (port-extents-y (parent sink)))))
      (values x0 y0 x1 y1))))

(defparameter *handle-radius* 5 "Default on-screen radius of a connection handle.")

(defmethod handle-extents ((c connection))
  (multiple-value-bind (x0 y0 x1 y1) (endpoints c)
    (let* ((mid-x (truncate (/ (+ x0 x1) 2)))
	   (mid-y (truncate (/ (+ y0 y1) 2)))
	   (hx0 (- mid-x *handle-radius*))
	   (hx1 (+ mid-x *handle-radius*))
	   (hy0 (- mid-y *handle-radius*))
	   (hy1 (+ mid-y *handle-radius*)))
      (values hx0 hy0 hx1 hy1))))

(defmethod render-widget ((f panel) (c connection))
  (with-slots (context canvas) f
    (multiple-value-bind (x0 y0 x1 y1) (endpoints c)
      (xlib:draw-line canvas context x0 y0 x1 y1))
    (multiple-value-bind (x0 y0 x1 y1) (handle-extents c)
      (xlib:draw-arc canvas context x0 y0 (- x1 x0) (- y1 y0)
		     0.0 (* 2.0 3.14159)))))

(defmethod hit-test ((c connection) x y)
  (multiple-value-bind (x0 y0 x1 y1) (handle-extents c)
    (if (within-extents x y x0 y0 x1 y1)
	(handle c)
	nil)))

;;;; Connection handles 

(defclass connection-handle (widget) ())

(defmethod touch ((h connection-handle) x y)
  (disconnect (parent h)))

;;;; Ports are the components of a dataflow where connections attach

(defclass port (widget)
  ((port-number :accessor port-number :initform 0 :initarg :port-number)
   (connections :accessor connections :initform nil :initarg :connections)
   (port-type :accessor port-type :initform :inlet :initarg :port-type)))

(defmethod adjoin-connection ((p port) connection)
  (setf (connections p) (adjoin connection (connections p))))

(defmethod remove-connection ((p port) connection)
  (setf (connections p) (remove connection (connections p))))

(defmethod join-widgets ((source port) (sink port) &optional x y)
  (when (not (eq source sink))
    (connect-ports source sink)))

(defmethod connect-ports ((source port) (sink port))
  (let* ((parent (parent (parent source)))
	 (connection (make-instance 'connection
				    :source source
				    :sink sink
				    :parent parent))
	 (handle (make-instance 'connection-handle)))
    ;;
    (setf (handle connection) handle)
    (setf (parent handle) connection)
    ;;
    (adjoin-connection source connection)
    (adjoin-connection sink connection)
    ;; save new connection in parent widget
    (adjoin-child parent connection)))

(defmethod cursor-key ((p port))
  :join-cursor)

;;;; Dataflow widgets are sources and sinks of data with attached ports. 

(defclass dataflow (textbox)
  ((num-inlets :accessor num-inlets :initform 0 :initarg :num-inlets)
   (inlets :accessor inlets :initform nil)
   (num-outlets :accessor num-outlets :initform 0 :initarg :num-outlets)
   (outlets :accessor outlets :initform nil)))

(defmethod replace-port ((d dataflow) (p port) port-type)
  (case port-type
    (:inlet 
     (setf (aref (inlets d) (port-number p)) p))
    (:outlet
     (setf (aref (outlets d) (port-number p)) p))))

(defmethod port-extents-x ((self dataflow) nth-port num-ports)
  "Return the x-coordinates of the left and right edges of the port NTH-PORT
in SELF."
  (with-slots (position-x width) self
    (let ((left (+ position-x (* nth-port (/ width num-ports)))))
      (values left (+ left *port-width*)))))

(defmethod port-extents-y ((self dataflow) &optional outlet-p)
  "Return the y-coordinates of the top and bottom edges of the
inlets for widget SELF. If outlet-p is non-nil, return the outlet
coordinates instead."
  (with-slots (position-y height) self
    (let ((top (if outlet-p
		   (+ position-y height)
		   (- position-y *port-height*))))
      (values top (+ top *port-height*)))))

(defparameter *port-width* 8 "Default onscreen width of a data port.")
(defparameter *port-height* 8 "Default hit-test height of a data port.")

(defmethod render-widget ((f panel) (w dataflow))
  (with-slots (context accent-context canvas) f
    (with-slots (position-x position-y height width
			    num-inlets num-outlets) w
      ;; draw default appearance
      (call-next-method)
      ;; decorate it with ports
      (dotimes (n num-inlets)
	(multiple-value-bind (x0 x1) (port-extents-x w n num-inlets)
	  (multiple-value-bind (ignore y) (port-extents-y w)
	    (xlib:draw-line canvas accent-context x0 y x1 y))))
      (dotimes (n num-outlets)
	(multiple-value-bind (x0 x1) (port-extents-x w n num-outlets)
	  (multiple-value-bind (y ignore) (port-extents-y w :outlet-p)
	    (xlib:draw-line canvas accent-context x0 y x1 y)))))))

(defmethod hit-test ((d dataflow) x y)
  "Return the widget (either D or one of its ports) when
hit-testing succeeds, nil otherwise."
  (with-slots (inlets outlets num-inlets num-outlets) d
    (labels ((hit-port (p n outlet-p)
	       (multiple-value-bind (x0 x1) 
		   (port-extents-x (parent p) (port-number p) n)
		 (multiple-value-bind (y0 y1) 
		     (port-extents-y (parent p) outlet-p)
		   (if (and (>= x x0) (<= x x1)
			    (>= y y0) (<= y y1))
		       p
		       nil))))
	     (hit-inlet (p n)
	       (hit-port p n nil))
	     (hit-outlet (p n)
	       (hit-port p n t)))
      (or (some #'(lambda (p)
		    (hit-inlet p num-inlets))
		inlets)
	  (some #'(lambda (p)
		    (hit-outlet p num-outlets))
		outlets)
	  ;; none of the inlets or outlets were hit. 
	  (call-next-method)))))

(defmethod cursor-key ((d dataflow))
  :touch-cursor)

;;;; Initializing STUN

(defun initialize-stun ()
  "Get the stun library ready to go."
  (initialize-display)
  (initialize-keymap-table)
  (setf *window->panel* (make-hash-table :test #'equal))
  (setf *class->keymap* (make-hash-table :test #'equal))
  ;;
  ;; define initial keymaps
  (define-key 'textbox '(:modifiers (:control) :key #\f) #'forward-char)
  (define-key 'textbox '(:modifiers (:control) :key #\b) #'backward-char)
  (define-key 'textbox '(:modifiers (:control) :key #\n) #'next-line)
  (define-key 'textbox '(:modifiers (:control) :key #\p) #'previous-line)
  (define-key 'textbox '(:keysym 65363) #'forward-char)
  (define-key 'textbox '(:keysym 65361) #'backward-char)
  (define-key 'textbox '(:keysym 65364) #'next-line)
  (define-key 'textbox '(:keysym 65362) #'previous-line)
  (define-key 'textbox '(:modifiers (:control) :key #\e) #'move-end-of-line)
  (define-key 'textbox '(:modifiers (:control) :key #\a) #'move-beginning-of-line)
  (define-key 'textbox '(:key #\Return) #'newline)
  (define-key 'textbox '(:key #\Backspace) #'backward-delete-char)
  ;;
  (define-key 'dataflow '(:modifiers (:control) :key #\f) #'forward-char)
  (define-key 'dataflow '(:modifiers (:control) :key #\b) #'backward-char)
  (define-key 'dataflow '(:modifiers (:control) :key #\n) #'next-line)
  (define-key 'dataflow '(:modifiers (:control) :key #\p) #'previous-line)
  (define-key 'dataflow '(:keysym 65363) #'forward-char)
  (define-key 'dataflow '(:keysym 65361) #'backward-char)
  (define-key 'dataflow '(:keysym 65364) #'next-line)
  (define-key 'dataflow '(:keysym 65362) #'previous-line)
  (define-key 'dataflow '(:modifiers (:control) :key #\e) #'move-end-of-line)
  (define-key 'dataflow '(:modifiers (:control) :key #\a) #'move-beginning-of-line)
  (define-key 'dataflow '(:key #\Return) #'newline)
  (define-key 'dataflow '(:key #\Backspace) #'backward-delete-char)
  ;;
  (define-key 'listener '(:modifiers (:control) :key #\f) #'forward-char)
  (define-key 'listener '(:modifiers (:control) :key #\b) #'backward-char)
  (define-key 'listener '(:modifiers (:control) :key #\n) #'next-history)
  (define-key 'listener '(:modifiers (:control) :key #\p) #'previous-history)
  (define-key 'listener '(:keysym 65363) #'forward-char)
  (define-key 'listener '(:keysym 65361) #'backward-char)
  (define-key 'listener '(:keysym 65364) #'next-history)
  (define-key 'listener '(:keysym 65362) #'previous-history)
  (define-key 'listener '(:modifiers (:control) :key #\e) #'move-end-of-line)
  (define-key 'listener '(:modifiers (:control) :key #\a) #'move-beginning-of-line)
  (define-key 'listener '(:key #\Return) #'evaluate)
  (define-key 'listener '(:key #\Backspace) #'backward-delete-char))

(defparameter *user-init-file-name* ".stunrc")

(defvar *initialization-hook* nil)

(defun load-user-init-file ()
  (load (merge-pathnames (make-pathname :name *user-init-file-name*)
			 (user-homedir-pathname))))

(defun stun ()
  (setf *display* nil)
  (initialize-stun)
  (load-user-init-file)
  (let ((panel (make-instance 'panel)))

    (message "Created panel window.")
    (xlib:display-finish-output *display*)

    (message "Resizing panel window.")
    (auto-resize panel)
    (xlib:display-finish-output *display*)

    (message "Adding widgets.")
    (let* ((worksheet (make-instance 'worksheet))
	   (toolbar (make-instance 'toolbar))
	   (textbox (make-instance 'textbox)))
      (setf (widget panel) worksheet)
      (dotimes (i 4)
	(let ((box (make-instance 'button
				  :parent toolbar
				  :label (nth i 
					      '("mount" "browse" "properties" "<< back")))))
	  (adjoin-child toolbar box)))
      (adjoin-child worksheet toolbar)
      (adjoin-child worksheet textbox)
      
      (xlib:display-finish-output *display*)
      
      (message "Preparing to run panel.")
      ;; now get going
      (run-panels))))

;;; stun.lisp ends here
