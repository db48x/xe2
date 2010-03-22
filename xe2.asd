;;; -*- Mode: Lisp; -*-

;; ASDF Manual: http://constantly.at/lisp/asdf/

(defpackage :xe2-asd)

(in-package :xe2-asd)

(asdf:defsystem xe2
  :name "xe2"
  :version "1.9"
  :maintainer "David O'Toole"
  :author "David O'Toole"
  :license "General Public License (GPL) Version 3"
  :description "An object-oriented graphical 2D game engine."
  :serial t
  :depends-on (:trivial-features :trivial-features-tests 
				 :lispbuilder-sdl :lispbuilder-sdl-image 
				 :lispbuilder-sdl-gfx
				 :lispbuilder-sdl-mixer
				 :clon)
  :components ((:file "xe2")
	       (:file "math")
	       (:file "rgb")
	       (:file "console")
	       (:file "widgets")
	       (:file "viewport")
	       (:file "cells")
	       (:file "narration")
	       (:file "browser")
	       (:file "worlds")
	       (:file "mission")
	       (:file "path")
	       (:file "util")
	       (:file "forms")))
	       
	       
