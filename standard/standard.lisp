;;; standard.lisp --- default startup splash screen and menu

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

;;; Code:

(eval-when (:execute :load-toplevel :compile-toplevel) 
  (require :rlx))

(defpackage :rlx-standard
  (:documentation "A default startup splash screen and menu for RLX.")
  (:use :rlx :common-lisp)
  (:export rlx-standard))

(in-package :rlx-standard)

;;; Menu items

(define-prototype module-launcher ()
  module-name)

(define-method initialize module-launcher (&optional module)
  (setf <module-name> module))

(define-method open module-launcher ()
  (setf *next-module* <module-name>)
  (sdl:quit-sdl))

;;; The splash screen widget

(define-prototype splash (:parent =widget=))

(defparameter *splash-width* 160)

(defparameter *splash-height* 100)
  
(define-method render splash ()
  (rlx:draw-resource-image ".splash-screen"
		       (- (/ <width> 2) *splash-width*)
		       (- (/ <height> 2) *splash-height*)
		       :destination <image>))

(defun show-default-splash-screen ()
  (let ((splash (clone =splash=))
	(browser (clone rlx:=browser=)))
    [resize splash 
	    :height *screen-height* 
	    :width *screen-width*]
    [move splash :x 0 :y 0]
    [resize browser :height 100 :width 200]
    [move browser :x 0 :y 0]
    (install-widgets (list splash browser))))
    
(defun rlx-standard ()
  (setf rlx:*screen-height* 600)
  (setf rlx:*screen-width* 800)
  (show-default-splash-screen))

(rlx-standard)

;;; standard.lisp ends here
