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
  :depends-on (:lispbuilder-sdl :lispbuilder-sdl-image)
  :components ((:file "rlx")
	       (:file "clon")
	       (:file "math")
	       (:file "rgb")
	       (:file "console")
	       (:file "cells")
	       (:file "widgets")
	       (:file "browser")
	       (:file "worlds")))
