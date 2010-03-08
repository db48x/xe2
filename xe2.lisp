;;; xe2.lisp --- a next-generation tile-based virtual home video-game console

;; __  _______ ____  
;; \ \/ / ____|___ \ 
;;  \  /|  _|   __) |
;;  /  \| |___ / __/ 
;; /_/\_\_____|_____|
               
;; Copyright (C) 2006, 2007, 2008, 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: multimedia, games
;; Version: 2.0

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

;; This program is dedicated to our beloved Yogi, who died 2006-10-06.

;;; Commentary:

;;  - Single-player, turn-based action in an object-oriented world
;;  - Graphical tile map display with transparency
;;  - Command processor with customizable keybindings
;;  - Formatting engine for messages and data displays
;;  - Saving/restoring game sessions
;;  - Music and sound effects support
;;  - Line-of-sight and light/shadow engine
;;  - Terrain generation and pattern-based map synthesis
;;  - Pathfinding and A.I. support

;;; Requirements:

;; This program requires a Common Lisp implementation and the
;; LISPBUILDER-SDL packages. These are available from:
;; http://lispbuilder.sourceforge.net/lispbuilder-sdl.html

;; ASDF users can do the following:
;;
;;   (require :asdf)
;;   (require :asdf-install)
;;   (asdf-install:install :lispbuilder-sdl) 
;;   (asdf-install:install :lispbuilder-sdl-image)

;;; Code:

(defpackage :xiomacs-engine-2 
  (:documentation "A graphical roguelike game engine for Common Lisp.")  
  (:nicknames :xe2)
  (:use :common-lisp :clon) 
  (:export *default-frame-width* *default-frame-height* =viewport=
	   *frequency* *output-chunksize* *output-channels*
  *dt* *physics-function* =equipment= *default-world-axis-size*
  *default-world-z-size* =browser= install-widgets
  *initialization-hook* initialize-engine split-string-on-lines
  message *screen-width* transform-method-body roll-under make-stat
  =formatter= initialize-colors *standard-categories*
  *default-action-points* =world= roll bind-key-to-method *colors*
  get-color =prompt= =menu-item= =direction-chooser= define-method
  *default-font* *startup* field-value set-field-value object-fields
  dispatch-event run *user-init-file-name* distance icon-resource
  icon-image *compass-directions* *compass-opposites*
  find-resource-property compose-blank-fields font-width font-height
  *browser* browser set-browser transform-field-reference
  *screen-height* =inventory= formatted-line-width
  formatted-line-height formatted-string-height formatted-string-width
  get-color create-image draw-image play define-prototype has-field
  defcell *choose-direction-menu* set-field-options field-option-value
  index-resource find-module-path index-module load-image-resource
  load-lisp-resource *executable* *resource-handlers* load-resource
  find-resource find-resource-object *colors* *world*
  load-user-init-file *module-directories* resource-to-plist
  make-event =widget= *active-widgets* bind-key-to-prompt-insertion
  make-field-initializer clone make-field-initializer-body
  make-key-modifier-symbol make-key-string normalize-event
  make-keyword make-object queue-head queue-max queue-count *sender*
  make-special-variable-name field-reference-p null-parent
  *message-send-symbol-suffix* *x11-color-data* object-name
  object-parent send send-super send-queue self opposite-direction
  object-address-string object step-in-direction direction-to =cell=
  plasma-rect subdivide-rect render-plasma add-hook run-hook
  queue-tail make-queue queue unqueue queue-message queued-messages-p
  unqueue-message send-queue field-value random-direction
  load-font-resource draw-string-solid read-pak *resource-table*
  initialize-resource-table percent-of-time render-formatted-paragraph
  make-formatted-string draw-string-shaded render-formatted-string
  render-formatted-line resource font-text-extents write-sexp-to-file
  with-message-sender *message-sender* =textbox= read-sexp-from-file
  write-pak *grammar* one-of left-hand-side right-hand-side expansions
  generate send-event-to-widgets play-music halt-music seek-music
  *joystick-mapping* *generic-joystick-mapping* *ps3-joystick-mapping*
  draw-resource-image *event-handler-function* *use-sound*
  trace-rectangle trace-row trace-column trace-octagon trace-line
  midpoint =asterisk= =gray-asterisk= self *module-widgets* defsprite
  =sprite= get-some-object-name transform-declaration-field-descriptor
  show-widgets no-such-field =narrator= find-modules-in-directory goal
  =mission= =gateway= =launchpad= =environment= directory-is-module-p
  find-all-modules *next-module* transform-tree stat-value draw-line
  *default-message-verbosities* *message-verbosities* add-overlay
  set-message-verbosities operation-symbol message-symbol play-sample
  set-music-volume add-message-verbosities with-message-queue
  =minimap= draw-pixel *user-keyboard-layout* *fullscreen* draw-circle
  set-field-option-value =pager= =pager-prompt= load-module
  field-options world set-frame-rate *frame-rate* =stack=
  *pak-file-extension* *window-title* set-timer-interval
  *message-logging* overlay poll-joystick-axis reset-joystick
  set-screen-width =universe= *universe* set-screen-height genseq
  *zoom-factor* zoom-image is-zoomed-resource *timer-interval*
  enable-timer disable-timer while enable-held-keys disable-held-keys
  do-cells draw-box draw-rectangle *quitting* quit reset))

(in-package :xe2)

;;; Emacs Lisp compatibilty macro 

(defmacro while (test &body body)
  `(loop while ,test do ,@body))

;;; xe2.lisp ends here
