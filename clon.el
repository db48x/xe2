;;; clon.el --- Emacs development support for the CLON object system

;; Copyright (C) 2006, 2007, 2008 David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: lisp, oop, extensions
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides support for developing applications with the Clon
;; object system for Common Lisp. Features include:

;; - Font-locking of CLON constructs
;; - Delimiter matching tweaks for the [foo bar] message send syntax
;; - TODO object browser with SLIME integration?

;; clon.el requires GNU Emacs, version 22 or later.

;;; Code:

(require 'cl)
(require 'rx)

;;; Font-locking

;; Put this in your emacs initialization file to get the highlighting:
;; (add-hook 'emacs-lisp-mode-hook #'clon-do-font-lock)

(defvar clon-font-lock-keywords
  `((,(rx (sequence "(" (group "define-method")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face) ;; this still doesn't work
				      ;; properly.
     (3 font-lock-type-face))
    (,(rx (sequence "(" (group "define-prototype")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
;    ("\\<\\(\<[^<>]*\>\\)\\>" (1 font-lock-preprocessor-face))
    ("(.*\\(\>\>\\>\\)" (1 font-lock-type-face))))

(defun clon-do-font-lock ()
  (interactive)
  "Highlight the keywords used in prototype-oriented programming."
  (font-lock-add-keywords nil clon-font-lock-keywords))

;;; Bracket matching in Common Lisp mode

;; Matching square brackets are turned off by default in Common Lisp
;; mode. This will turn them back on, which makes it work with
;; paredit, show-paren, and highlight-parentheses modes.

(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)

(provide 'clon)
;;; clon.el ends here
