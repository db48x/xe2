;;; uuidgen.el --- generate uuid's using an external program

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: processes

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

;; This file is a simple wrapper around the command-line UUID
;; generator "uuidgen." This is available on Debian in the package
;; "e2fsprogs".

;; More about uuids: http://www.ietf.org/rfc/rfc4122.txt

;;; Code:

(defvar uuidgen-program "uuidgen"
  "Name of the external program used to generate uuid's.")

(defun uuidgen-generate ()
  "Generate a UUID string by invoking the external program
`uuidgen-program'."
  (with-temp-buffer
    (if (= 0 (call-process uuidgen-program nil t))
	(buffer-substring-no-properties (point-min)
					;; remove trailing newline
					(1- (point-max)))
	(error "Cannot generate uuid."))))

(defvar uuidgen-generator-function #'uuidgen-generate
  "The function called to actually generate UUID's.
The default is to use the external program `uuidgen', which is
included in the `e2fsprogs' package. (Debian users can type:
apt-get install e2fsprogs).")

(defsubst make-uuid ()
  "Return an RFC 4122 UUID string."
  (funcall uuidgen-generator-function))

(provide 'uuidgen)
;;; uuidgen.el ends here
