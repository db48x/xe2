;;; grammar.lisp --- context-free grammar sentence generator

;; Copyright (C) 2009  David O'Toole

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

;; http://en.wikipedia.org/wiki/Context-free_grammar

;;; Code:

(in-package :rlx)

(defparameter *test-grammar* 
  '((mission >> (at location please goal+ in exchange for reward))
    (location >> mars zeta-base nebula-m corva-3)
    (goal+ >> goal (goal and goal+))
    (goal >> (defeat foe) (defend friend) (activate button) (retrieve documents)
     (collect mineral+))
    (mineral+ >> mineral (mineral and mineral+))
    (mineral >> endurium technetium molybdenum francium iodine)
    (foe >> scanner biclops unique)
    (friend >> transport skiff soldier scientist)
    (unique >> zx-90 xioblade)
		   (reward >> money part)
    (money >> 10000 20000 30000 40000 50000)
    (part >> muon-pistol lepton-cannon ion-shield-belt)))


(defvar *grammar* *test-grammar*
  "The current context-free grammar used for sentence generation.
This is an association list of the form:

    ((VARIABLE . EXPANSIONS)
     (VARIABLE . EXPANSIONS)
     ...)

Where EXPANSIONS is a list of alternatives, which may be either (1)
single symbols or (2) a list of symbols, representing concatenation.")

(defun one-of (set)
  (list (nth (random (length set)) set)))

(defun left-hand-side (rule)
  (first rule))

(defun right-hand-side (rule)
  (rest (rest rule)))

(defun expansions (variable)
  (right-hand-side (assoc variable *grammar*)))

(defun generate (phrase)
  "Generate a random phrase using the grammar in `*grammar*'."
  (cond ((listp phrase)
	 (apply #'append (mapcar #'generate phrase)))
	((expansions phrase)
	 (generate (one-of (expansions phrase))))
	(t (list phrase))))

;; TODO allow :keywords which become (symbol-value :keyword) during expansion

(provide 'grammar)
;;; grammar.lisp ends here
