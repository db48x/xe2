;;; -*- Mode: Lisp; -*-

;; ASDF Manual: http://constantly.at/lisp/asdf/

(defpackage :rlx-asd)

(in-package :rlx-asd)

(asdf:defsystem rlx
  :name "RLX"
  :version "0.12"
  :maintainer "David O'Toole"
  :author "David O'Toole"
  :license "General Public License (GPL) Version 3"
  :description "An object-oriented graphical roguelike game engine."
  :serial t
  :depends-on (:trivial-features :trivial-features-tests 
				 :lispbuilder-sdl :lispbuilder-sdl-image 
				 :lispbuilder-sdl-gfx
				 :lispbuilder-sdl-mixer
				 :clon)
  :components ((:file "rlx")
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
	       (:file "path")))
	       
	       
