;;; rlx.lisp --- a next-generation tile-based virtual home video-game console
;;  ____  _    __  __
;; |  _ \| |   \ \/ /
;; | |_) | |    \  /
;; |  _ <| |___ /  \
;; |_| \_\_____/_/\_\

;; Copyright (C) 2006, 2007, 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: multimedia, games
;; Version: 0.903

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

(eval-when (:execute :load-toplevel :compile-toplevel) 
  (require :lispbuilder-sdl)
  (require :lispbuilder-sdl-image))

;;; Code:

(defpackage :rlx 
  (:documentation "A graphical roguelike game engine for Common Lisp.")  
  (:use :common-lisp :clon) 
  (:export *default-frame-width* *default-frame-height* =viewport=
  =equipment= *default-world-axis-size* *default-world-z-size*
  =browser= install-widgets *initialization-hook* initialize-engine
  message *screen-width* transform-method-body make-stat =formatter=
  initialize-colors *standard-categories* *default-action-points*
  =world= roll bind-key-to-method *colors* get-color =prompt=
  define-method *default-font* *startup* field-value set-field-value
  object-fields dispatch-event run *user-init-file-name* distance
  icon-resource icon-image *compass-directions* *compass-opposites*
  find-resource-property compose-blank-fields font-width font-height
  *browser* browser set-browser transform-field-reference
  *screen-height* =inventory= formatted-string-height
  formatted-string-width get-color create-image draw-image play
  define-prototype has-field set-field-options field-option-value
  index-resource find-module-path index-module load-image-resource
  load-lisp-resource *resource-handlers* load-resource find-resource
  find-resource-object *colors* *active-world* load-user-init-file
  *module-directories* resource-to-plist make-event =widget=
  *active-widgets* bind-key-to-prompt-insertion make-field-initializer
  clone make-field-initializer-body make-key-modifier-symbol
  make-key-string normalize-event make-keyword make-object queue-head
  queue-max queue-count *sender* make-special-variable-name
  field-reference-p null-parent *message-send-symbol-suffix*
  *x11-color-data* object-name object-parent send send-super
  send-queue self opposite-direction object-address-string object
  step-in-direction direction-to =cell= plasma-rect subdivide-rect
  render-plasma add-hook run-hook queue-tail make-queue queue unqueue
  queue-message queued-messages-p unqueue-message send-queue
  field-value random-direction load-font-resource draw-string-solid
  read-pak *resource-table* initialize-resource-table
  render-formatted-paragraph make-formatted-string draw-string-shaded
  render-formatted-string render-formatted-line resource
  write-sexp-to-file with-message-sender *message-sender*
  read-sexp-from-file write-pak send-event-to-widgets
  draw-resource-image *event-handler-function* trace-rectangle
  trace-octagon trace-line midpoint =asterisk= =gray-asterisk= self
  *module-widgets* transform-declaration-field-descriptor
  no-such-field =narrator= find-modules-in-directory =environment=
  directory-is-module-p find-all-modules *next-module* transform-tree
  stat-value operation-symbol message-symbol with-message-queue
  *user-keyboard-layout* set-field-option-value field-options
  *quitting* quit reset))

(in-package :rlx)

;;; rlx.lisp ends here
