;;; widgets.lisp --- interactive graphical elements with offscreen drawing

;; Copyright (C) 2008  David O'Toole

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

;;; Commentary:

;; A game can draw directly to the screen if it wants. This file
;; defines a reusable "widget" object where you draw to an offscreen
;; image. Widgets are also designed to receive input events via the
;; `handle-key' method; `define-key' and `undefine-key' can be used to
;; manage keybindings.

;; The main RLX loop is set up to dispatch event messages to
;; widgets. After the events have been processed and the widgets have
;; drawn their images to their respective offscreen buffers, the
;; engine copies the buffers to the screen. (see console.lisp)

;; This file contains the basic widget code and some standard widgets,
;; including an output formatter and a configurable command prompt. 

;;; Code:

(in-package :rlx)

(define-prototype widget
    (:documentation "A basic widget that renders to an offscreen image.")
  (keymap :documentation "A hash table mapping keylists to lambdas.")
  (image :documentation "The offscreen image buffer containing the widget's rendered output.")
  (width :documentation "The current allocated image width of the widget, in pixels.")
  (height :documentation "The current allocated image height of the widget, in pixels.")
  (x :documentation "The screen x-coordinate of the left side of the widget's display area.")
  (y :documentation "The screen y-coordinate of the top of the widget's display area."))

(define-method initialize widget ()
  (setf <keymap> (make-hash-table :test 'equal)))

(define-method resize widget (&key height width)
  "Allocate an image buffer of HEIGHT by WIDTH pixels."
  (setf <width> width 
	<height> height)
  (setf <image> (create-image width height)))

(define-method move widget (&key x y)
  (setf <x> x <y> y))

(define-method render widget ()
  "Render the widget to its image."
  ;; The default implementation leaves the image blank.
  nil)

(define-method get-image widget ()
  <image>)

(define-method clear widget (&optional (color ".black"))
  (draw-box 0 0 <width> <height> :color color :stroke-color color :destination <image>))

(define-method define-key widget (key-name modifiers func)
  "Bind the described keypress to invoke FUNC.
KEY-NAME is a string giving the key name; MODIFIERS is a list of
keywords like :control, :alt, and so on."
  (setf (gethash (normalize-event (cons key-name modifiers))
		 <keymap>)
	func))

(define-method undefine-key widget (key-name modifiers)
  "Remove the described keybinding."
  (remhash (normalize-event (cons key-name modifiers))
	   <keymap>))

(define-method handle-key widget (keylist)
  "Look up and invoke the function (if any) bound to KEYLIST. Return t
if a binding was found, nil otherwise."
  (let ((func (gethash keylist <keymap>)))
    (when func
      (prog1 t
	(funcall func)))))

;;; Hit testing for mouse cursor support

(define-method hit widget (x y)
  "Find a widget by screen position. The default implementation
returns itself when the widget's own onscreen image is clicked.  Child
implementations may do subsequent hit-testing on child widgets, and
possibly return one of them."
  (let ((x0 <x>)
	(y0 <y>)) self
    (when (within-extents x y 
			  x0 y0 
			  (+ x0 <width>)
			  (+ y0 <height>))
      (message "~S" (object-name (object-parent self)))
      self)))

;;; Stack layout

(define-prototype stack 
    (:parent =widget= :documentation "Stack all the child widgets on top of one another in a column.")
  (children :initform nil :documentation "The widgets in the stack."))

(define-method initialize stack (&rest children)
  (setf <children> children))

(define-method render stack ()
  (let ((y <y>)
	(x <x>))
    (dolist (widget <children>)
      [move widget :x x :y y]
      (incf y (field-value :height widget)))))

(define-method set-children stack (children)
  (setf <children> children))

(define-method hit stack (x y)
  (hit-widgets x y <children>))

;;; Formatted display widget

;; This section implements a simple output formatting widget for the
;; presentation of messages and other in-game data. Foreground and
;; background colors are supported, as well as displaying images
;; in-line with text of different fonts.

;; A formatted line is a list of formatted strings. A formatted 
;; string is a cons of (STRING . PROPERTIES), where the keys in
;; PROPERTIES are chosen from:

;;   :FOREGROUND       Foreground color. A color resource name.
;;   :BACKGROUND       Background color. A color resource name.
;;   :IMAGE            Image to be displayed instead of STRING.
;;                     If this is a string, the corresponding resource image is 
;;                     found and displayed. If this is an image object, the image 
;;                     itself is displayed.
;;   :FONT             Font name. Defaults to *default-font*.

;; First the utility functions for rendering individual formatted
;; strings and lists (lines) of such strings. 

(defvar *default-formatter-scrollback-size* 1000)

(defun formatted-string-height (S)
  (destructuring-bind (string &key image (font *default-font*) &allow-other-keys) S
    (declare (ignore string))
    (if image
	(image-height image)
	(font-height font))))

(defun formatted-string-width (S)
  (destructuring-bind (string &key image (font *default-font*) &allow-other-keys) S
    (if image 
	(image-width image)
	(* (font-width font) (length string)))))

(defun formatted-line-height (line)
  (apply #'max (mapcar #'formatted-string-height line)))

(defun formatted-line-width (line)
  (apply #'+ (mapcar #'formatted-string-width line)))

(defun render-formatted-string (formatted-string x y &key (text-offset 0) destination)
  "Render the FORMATTED-STRING to position X,Y on the image DESTINATION.
If TEXT-OFFSET is provided, add that many pixels to the Y coordinate
for rendered text in the line. (This is used to make text align with
inline images that are larger than the text height---see also
`render-formatted-line')."
  (destructuring-bind (string &key (foreground ".white") 
			      (font *default-font*)
			      background image)
      formatted-string
    (if image
	(draw-image (typecase image
		      (string (find-resource-object image))
		      (otherwise image))
		    x y :destination destination)
	(if (null string)
	    nil ;; WARNING
	    ;; draw the text.
	    (if background
		(draw-string-shaded string x (+ text-offset y)
				    foreground background
				    :destination destination
				    :font font)
		(draw-string-solid string x (+ text-offset y) :font font
				   :color foreground :destination destination))))))

(defun render-formatted-line (line x y &key destination (font *default-font*))
  "Render the formatted LINE at position X,Y on the image DESTINATION.
Return the height of the rendered line."
  (let* ((line-height (formatted-line-height line))
	 (default-font-height (font-height font))
	 (text-offset (if (> line-height default-font-height)
			  (truncate (/ (- line-height 
					  default-font-height)
				       2))
			  0))
	 (current-x x))
    (dolist (string line)
      (when string
	(render-formatted-string string current-x y :text-offset text-offset 
				 :destination destination)
	(incf current-x (formatted-string-width string)))
      line-height)))
  
;; Next comes the CLON formatter widget that uses the utility
;; functions just defined.

(define-prototype formatter 
    (:parent =widget= :documentation "Output formatter.")
  (lines :documentation "Vector of lines.")
  (current-line :documentation "Formatted line currently being composed."))

(define-method print formatter (string &rest keys &key image foreground background font)
  "Add a formatted STRING to the end of the current line.
Example: [print my-formatter \"hello\" :foreground \"red\"]"
  (vector-push-extend (cons string keys) <current-line>))

(define-method print-formatted-string formatter (formatted-string)
  (vector-push-extend formatted-string <current-line>))	       

(define-method print-image formatter (image)
  [print self nil :image image])

(define-method println formatter (&rest args)
  (apply #'rlx:send self :print self args)
  [newline self])

(define-method space formatter ()
  [print self " "])

(define-method newline formatter ()
  "Add the current line to the display, and start a fresh offscreen
line."
  (when (and (vectorp <current-line>)
	     (> (fill-pointer <current-line>) 0))
    (vector-push-extend (coerce <current-line> 'list) <lines>))
  (setf <current-line> (make-array 10 :adjustable t :fill-pointer 0)))

(define-method reset-lines formatter ()
  (setf <lines> (make-array 10 :adjustable t :fill-pointer 0)))

(define-method delete-line formatter (&optional (num-lines 1))
  (dotimes (n num-lines)
    (vector-pop <lines>)))

(define-method delete-all-lines formatter ()
  [delete-line self (fill-pointer <lines>)])

(define-method initialize formatter ()
  [reset-lines self]
  [newline self])

(define-method update formatter ()
  "Invoked before each render. Replace this method for custom
auto-updated displays."  
  nil)

(defun get-some-object-name (ob)
  (if (stringp (field-value :name ob))
      (field-value :name ob)
      (let ((str (symbol-name (object-name (object-parent ob)))))
	(string-capitalize (subseq str 1 (search "=" str :from-end t))))))

(define-method print-object-tag formatter (ob)
  [print-image self (field-value :tile ob)]
  [space self]
  [print self (get-some-object-name ob)]
  [space self])

(define-method print-separator formatter ()
  [print self "  :  " :foreground ".gray20"])

(define-method render formatter ()
  [clear self]
  [update self]
  (let ((y <height>) (n 0)
	line
	(lines <lines>)
	(image <image>))
    (setf n (fill-pointer lines))
    (when (plusp n)
      (loop do
	   (progn 
	     (setf line (aref lines (- n 1)))
	     (decf y (formatted-line-height line))
	     (render-formatted-line line 0 y :destination image)
	     (decf n))
	 ;; reached top of output image?
	 while (and (plusp y) 
		    ;; ran out of lines to display?
		    (not (zerop n)))))))
	   
;;; Command prompt widget

;; The command prompt widget is a text input area with Emacs-like
;; keybindings. It is used to send messages to objects. (For ease of
;; use, prompt commands may also be bound to single keystrokes.)
;;
;;  The command syntax is:
;; 
;;   command-name arg1 arg2 ...
;;
;; All tokens must be Lisp-readable symbols, strings, or numbers.
;;
;; The command prompt will change its commands into message sends, and
;; send them to a designated command receiver:
;; 
;;   yes             -->   [yes <receiver>]
;;   move :north     -->   [move <receiver> :north]
;;   attack :west :with :left-hand  --> [attack <receiver> :west 
;;                                              :with :left-hand]
;;
;; So the commands are just the receiver's methods. The command
;; line's HELP system is just a method documentation browser
;; (i.e. SLOT-DESCRIPTORS.) 

;; The prompt can bind single keystrokes (i.e. one or more modifiers
;; and a keypress code) to the insertion of an arbitrary string at
;; point in the prompt. A string that ends in a period is a
;; "terminating" keybinding; a terminating keybinding also completes
;; the command input, causing the resulting command to be executed.

;; Examples: 

;;    <up>      -->    move :north .
;;   shift-<up> -->    push :north .
;;     C-q      -->    quaff         ;; also shows potion list as output
;;     M-1      -->    choose 1 .    ;; choose option 1 from output

;; The prompt has two input modes; direct mode and forward mode. In
;; direct mode, the prompt widget's own keymap is used. In forward
;; mode, all keypresses (except for the mode escape key) are rejected
;; by returning `nil' from `handle-key'.

;; In the typical setup, the first widget to receive the keypress
;; would be the default command prompt; a customized prompt, with
;; game-specific keybindings, would come second. During play, the
;; command prompt would reject all keypresses, which would pass on to
;; the next widget in the frame (the customized prompt.) To "escape"
;; this and enter commands, hit ESCAPE (and again to return to forward
;; mode.)

;; The modes can be toggled with the ESCAPE key.

;; The modes have different prompt strings:

(defparameter *direct-prompt-string* "COMMAND> ")
(defparameter *forward-prompt-string* "> ")

(defparameter *default-prompt-margin* 4)

(defparameter *default-prompt-history-size* 100)
(defparameter *default-cursor-width* 2)

(defvar *lowercase-alpha-characters* "abcdefghijklmnopqrstuvwxyz")
(defvar *uppercase-alpha-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defvar *numeric-characters* "0123456789")

(define-prototype prompt
    (:parent rlx:=widget= :documentation "A command prompt.")
  (mode :documentation "Either :direct or :forward." :initform :direct)
  (default-keybindings :documentation "Default keybindings bound during initialization.
These are the arguments to `bind-key-to-prompt-insertion', which see.")
  (visible :documentation "When non-nil, the prompt is drawn." :initform t)
  (receiver :documentation "The object to send command messages to when in :forward mode.")
  (point :initform 0 :documentation "Integer index of cursor within prompt line.")
  (line :initform "" :documentation "Currently edited command line.")
  (history :initform (make-queue :max *default-prompt-history-size*)
	   :documentation "A queue of strings containing the command history.")
  (history-position :initform 0))

(defun bind-key-to-prompt-insertion (p key modifiers &optional (insertion key))
  [define-key p (string-upcase key) modifiers
	      #'(lambda ()
		  [insert p insertion])])

(defun bind-key-to-method (p key modifiers method-keyword)
  [define-key p (string-upcase key) modifiers
	      #'(lambda ()
		  (send nil method-keyword p))])

(define-method handle-key prompt (keylist)
  "Reject all keypresses when in :forward mode; otherwise handle them
normally."
  (ecase <mode>
    ;; returning t stops the frame from trying other widgets
    (:direct (prog1 t (let ((func (gethash keylist <keymap>)))
			(when func
			  (funcall func)))))
    (:forward (when (string= "ESCAPE" (first keylist))
		(setf <mode> :direct)))))

(define-method escape prompt ()
  (setf <mode> :forward))

(define-method set-mode prompt (mode)
  (setf <mode> mode))

(define-method install-default-keybindings prompt ()
  (dolist (k <default-keybindings>)
    (apply #'bind-key-to-prompt-insertion self k)))
		     
(define-method install-keybindings prompt ()
  ;; install basic keybindings
  (bind-key-to-method self "A" '(:control) :move-beginning-of-line)
  (bind-key-to-method self "E" '(:control) :move-end-of-line)
  (bind-key-to-method self "F" '(:control) :forward-char)
  (bind-key-to-method self "B" '(:control) :backward-char)
  (bind-key-to-method self "K" '(:control) :clear)
  (bind-key-to-method self "BACKSPACE" nil :backward-delete-char)
  (bind-key-to-method self "RETURN" nil :execute)
  (bind-key-to-method self "ESCAPE" nil :escape)
  (bind-key-to-method self "P" '(:alt) :backward-history)
  (bind-key-to-method self "N" '(:alt) :forward-history)  
  ;; install keybindings for self-inserting characters
  (map nil #'(lambda (char)
	       (bind-key-to-prompt-insertion self (string char) nil
					     (string-downcase char)))
       *lowercase-alpha-characters*)
  (map nil #'(lambda (char)
	       (bind-key-to-prompt-insertion self (string char) '(:shift)))
       *uppercase-alpha-characters*)
  (map nil #'(lambda (char)
	       (bind-key-to-prompt-insertion self (string char) nil))
       *numeric-characters*)
  ;; other characters
  (bind-key-to-prompt-insertion self "MINUS" nil "-")
  (bind-key-to-prompt-insertion self "SEMICOLON" nil ";")
  (bind-key-to-prompt-insertion self "SEMICOLON" '(:shift) ":")
  (bind-key-to-prompt-insertion self "SPACE" nil " ")
  (bind-key-to-prompt-insertion self "QUOTE" nil "'")
  (bind-key-to-prompt-insertion self "QUOTE" '(:shift) "\""))

(define-method initialize prompt ()
  [parent>>initialize self]
  [install-default-keybindings self])

(define-method forward-char prompt ()
  (setf <point> (min (1+ <point>)
		     (length <line>))))

(define-method backward-char prompt ()
  (setf <point> (max 0 (1- <point>))))

(define-method insert prompt (string)
  (setf <line> (concatenate 'string
			    (subseq <line> 0 <point>)
			    string
			    (subseq <line> <point>)))
  (incf <point> (length string))
  ;; if the insertion ends with a period, also execute the command
  ;; line.
  (when (string= "." (subseq string (1- (length string))))
    (setf <line> (subseq string 0 (1- (length string))))
    [execute self]))

(define-method backward-delete-char prompt ()
  (when (< 0 <point>) 
    (setf <line> (concatenate 'string
			      (subseq <line> 0 (1- <point>))
			      (subseq <line> <point>)))
    (decf <point>)))

(define-method execute prompt ()
  (let* ((*read-eval* nil)
	 (sexp (handler-case 
		   (read-from-string (concatenate 'string "(" <line> ")"))
		 ((or end-of-file reader-error) () nil))))
    (when sexp 
      (apply #'send nil (make-keyword (car sexp)) <receiver> (cdr sexp)))
    (queue <line> <history>)
    [clear self]))

(define-method history-item prompt (n)
  (nth (- (queue-count <history>) n)
       (queue-head <history>)))

(define-method forward-history prompt ()
  (when (> <history-position> 0)
    (setf <line> [history-item self (decf <history-position>)])))

(define-method backward-history prompt ()
  (when (< <history-position> (queue-count <history>))
    (setf <line> [history-item self (incf <history-position>)])))

(define-method set-receiver prompt (receiver)
  (setf <receiver> receiver))

(define-method clear prompt ()
  (setf <line> "")
  (setf <point> 0))

(define-method move-end-of-line prompt ()
  (setf <point> (length <line>)))

(define-method move-beginning-of-line prompt ()
  (setf <point> 0))

(define-method show prompt ()
  (setf <visible> t))

(define-method hide prompt ()
  (setf <visible> nil))

(define-method render prompt ()
  (when <visible>
    (let* ((image <image>)
	   (font-height (font-height *default-font*))
	   (font-width (font-width *default-font*))
	   (prompt-height (+ (* 2 *default-prompt-margin*)
			     font-height))
	   (strings-y *default-prompt-margin*)
	   (prompt-string (ecase <mode>
			    (:direct *direct-prompt-string*)
			    (:forward *forward-prompt-string*))))
      (draw-box 0 0 <width> prompt-height :color ".black" :stroke-color ".black"
		:destination image)
      ;; draw prompt 
      (draw-string-solid prompt-string
			 *default-prompt-margin*
			 strings-y
			 :destination image)
      ;; draw current command line text
      (draw-string-solid <line>
			 (+ *default-prompt-margin* 
			    (* font-width (length prompt-string)))
			 strings-y
			 :destination image)
      ;; draw cursor
      (draw-box (* (+ (length prompt-string) 1 <point>)
		   font-width)
		strings-y
		*default-cursor-width*
		font-height
		:color ".yellow"
		:stroke-color ".yellow"
		:destination image))))

;;; Text display and edit control

;; No fancy formatting, but editable and supports scrolling.

(defparameter *textbox-margin* 4 "Default onscreen margin (in pixels) of a textbox.")

(defparameter *textbox-minimum-width* 10) 

(define-prototype textbox (:parent =widget=)
  (font :initform ".default-font")
  (buffer :initform nil)
  (max-displayed-rows :initform nil :documentation "An integer when scrolling is enabled.")
  (max-displayed-columns :initform nil)
  (background-color :initform ".blue")
  (foreground-color :initform ".white")
  (point-row :initform 0)
  (point-column :initform 0)
  (visible :initform t))

(define-method show textbox ()
  (setf <visible> t))

(define-method hide textbox ()
  (setf <visible> nil))

(define-method set-buffer textbox (buffer)
  (setf <buffer> buffer))

(defparameter *next-screen-context-lines* 3)

(define-method page-up textbox (buffer)
  "Scroll up one page, only when <max-displayed-rows> is set."
  (clon:with-field-values (max-displayed-rows) self
    (when (integerp max-displayed-rows)
      (setf <point-row> (max 0
			   (- <point-row> (- max-displayed-rows
					     *next-screen-context-lines*)))))))

(define-method page-down textbox (buffer)
  "Scroll down one page, only when <max-displayed-rows> is set."
  (clon:with-field-values (max-displayed-rows) self
    (when (integerp max-displayed-rows)
      (setf <point-row> (min (- (length <buffer>) max-displayed-rows)
			     (+ <point-row> (- max-displayed-rows
					     *next-screen-context-lines*)))))))

(define-method auto-center textbox ()
  "Automatically center the textbox on the screen."
  (clon:with-field-values (x y width height) self
    (let ((center-x (truncate (/ *screen-width* 2)))
	  (center-y (truncate (/ *screen-height* 2))))
      (setf <x> (- center-x (truncate (/ width 2)))
	    <y> (- center-y (truncate (/ height 2)))))))

(define-method resize-to-scroll textbox (&key width height)
  "Resize the textbox to WIDTH * HEIGHT and enable scrolling of contents.
This method allocates a new SDL surface."
  (assert (and (numberp width) (numberp height)))
  [resize self :height height :width width]
  (setf <max-displayed-rows> (truncate (/ height (font-height <font>)))))

(define-method resize-to-fit textbox ()
  "Automatically resize the textbox to fit the text, and disable scrolling.
This method allocates a new SDL surface when necessary."
  ;; disable scrolling
  (setf <max-displayed-rows> nil)
  ;; measure text
  (let* ((buffer <buffer>)
	 (line-height (font-height <font>))
	 (line-lengths (mapcar #'(lambda (s)
				   (font-text-extents s <font>))
			       buffer)))
    ;; update geometry
    (let ((width0 (max *textbox-minimum-width*
		       (+ (* 2 *textbox-margin*)
			  (if (null line-lengths)
			      0 
			      (apply #'max line-lengths)))))
	  (height0 (+ (* 2 *textbox-margin*)
		      (* line-height (max 1 (length buffer))))))
      (when (or (not (equal <width> width0))
		(not (equal <height> height0)))
	(message "resizing textbox H:~S W:~S" height0 width0)
	[resize self :height height0 :width width0]))))

(define-method render textbox ()
  (when <visible>
    [clear self]
    (with-fields (buffer x y width height) self
      (with-field-values (font image) self
	;; measure text
	(let* ((line-height (font-height font))
	       (line-lengths (mapcar #'(lambda (s)
					 (font-text-extents s font))
				     buffer)))
	  [resize-to-fit self]
	  ;; draw background
	  (draw-box x y width height :destination image
		    :stroke-color <foreground-color>
		    :color <background-color>)
	  ;; draw text
	  (let ((x0 (+ x *textbox-margin*))
		(y0 (+ y *textbox-margin*)))
	    (dolist (line (nthcdr <point-row> buffer))
	      (draw-string-solid line x0 y0 :destination image
				 :font font :color <foreground-color>)
	      (incf y0 line-height))
	    ;; draw cursor
	    ;; TODO fix <point-row> to be drawn relative pos in scrolling
	    (let* ((current-line (nth <point-row> buffer))
		   (cursor-width (font-width font))
		   (x1 (+ x *textbox-margin*
			  (font-text-extents (subseq current-line 0 <point-column>)
					     font)))
		   (y1 (+ 2 y *textbox-margin*
			  (* line-height <point-row>))))
	      (draw-rectangle x1 y1 cursor-width line-height 
			      :color ".yellow"
			      :destination image))))))))

;;; The pager switches between different visible groups of widgets

(define-prototype pager (:parent =widget=)
  (pages :initform nil)
  (current-page :initform nil
		:documentation "Keyword name of current page.")
  (pager-height :initform 20
		:documentation "Height in pixels of the pager")
  (background-color :initform ".gray18")
  (style :initform '(:foreground ".gray60")
	 :documentation "Text style properties for pager display")
  (prefix-string :initform " F")
  (number-separator-string :initform ": ")
  (separator-string :initform "  ")
  (highlighted-style :initform '(:foreground ".gray20" :background ".white")))

(define-method initialize pager ()
  [parent>>initialize self]
  [auto-position self]
  (labels ((s1 () [select self 1])
	   (s2 () [select self 2])
	   (s3 () [select self 3]))
    [define-key self "F1" nil #'s1]
    [define-key self "F2" nil #'s2]
    [define-key self "F3" nil #'s3]))

(define-method select pager (page)
  (let ((newpage (etypecase page
		   (number (car (nth (- page 1) <pages>)))
		   (keyword page))))
    (if (null newpage)
	(error "Cannot find page.")
	(progn 
	  (setf <current-page> newpage)
	  ;; insert self always as first widget
	  (apply #'rlx:install-widgets self (cdr (assoc newpage <pages>)))))))

(define-method auto-position pager ()
  [resize self :width rlx:*screen-width* :height <pager-height>]
  [move self :x 0 :y (- rlx:*screen-height* <pager-height>)])

(define-method add-page pager (keyword &rest widgets)
  (push (cons keyword widgets) <pages>))

(define-method get-page-names pager ()
  (remove-duplicates (mapcar #'car <pages>)))

(define-method render pager ()
  ;; calculate geometry. always draw
  [clear self <background-color>]
  (let ((n 1)
	(line '()))
    (dolist (page <pages>)
      (let ((page-name (car page)))
	;; build a list of formatted strings
	(push (cons (concatenate 'string 
				 <prefix-string>
				 (format nil "~D" n)
				 <number-separator-string>
				 (symbol-name page-name)
				 <separator-string>)
		    ;; highlight current page
		    (if (eq page-name <current-page>)
			<highlighted-style> <style>))
	      line))
      (incf n))
    ;; draw the string
    (render-formatted-line (nreverse line) 0 0 :destination <image>)))
    
    
				  

;;; widgets.lisp ends here
