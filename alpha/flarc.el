;;; flarc.el --- non-destructive, transcoding, annotating audio archiver

;; Copyright (C) 2007  David O'Toole

;; Author: David O'Toole <dto@monad.lab>
;; Keywords: multimedia

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is FLARC!

;; I own a lot of recording equipment, and I generate lots and lots of
;; audio files in various formats---ranging from less than a second to
;; several hours in length. Furthermore, I work with several other
;; musicians, with whom I constantly exchange recordings. We only want
;; to keep about ten percent of this stuff, but we have so much stuff
;; now that we need help to organize the work of reviewing and
;; annotating the material.

;; The purpose of FLARC is to help you:

;;  - extract audio files from USB Mass Storage devices (for example zoom.el)
;;    or from CD-Audio / WAV files / etc
;;  - create an archive from extracted audio files, or add to an existing archive
;;  - store archive contents as FLAC
;;  - review, rename, annotate, and slice audio files
;;  - create an org-mode file index for each archive with  review file foo.bar
;;  - define audio tags and tag your files with auto-completion of tag names
;;  - search annotations/tags across multiple archives to find relevant audio
;;  - carefully delete unwanted recordings
;;  - create ogg vorbis files upon request
;;  - burn CD's?
;;  - tag regions of files? (or just split files, then tag the pieces...)

;; You can also use FLARC to organize backups of your purchased CD's; for
;; example, when ripping to FLAC from A Better CD Encoder ("abcde").

;; <:6d3aaf1e-85c4-4799-a948-8b80fc9d65fe:>

;;; Code:

(require 'usb)
(require 'radio)

;;; Basics

(defvar flarc-temp-directory "/tmp")

;;; Archives and Annotations

(defvar flarc-archives ()
  "List of directory names containing frequently-used flarc archives.
This is used for auto-completion.")

(defvar flarc-selected-archive nil
  "The archive to which operations will apply when no archive is
specified.")

(defun flarc-choose-archive ()
  (interactive)
  (completing-read "Choose flarc archive: " flarc-archives))

(defun* flarc-select-archive (&optional (archive (flarc-choose-archive)))
  (interactive)
  (setf flarc-selected-archive archive))

;;; Auditioning files

(defun* flarc-audition (file)

;;; Tags

(defvar flarc-tags ()
  "List of string tag names used for autocompletion when tagging.

Example:

 (setf flarc-tags '(\":studio:\" \":field:\" \":fender-mustang:\" \":wildlife:\"
                    \":michael:\" \":dto:\"))

You could use these tags to annotate:

     a studio recording, 
     a field recording (i.e. with a portable recorder),
     a recording of a Fender Mustang guitar, 
     a recording of bird calls or other wildlife sounds,
     a recording of Michael, and
     a recording of myself (dto).

Later on, you can use `flarc-search' to find audio files of
Michael playing the Fender, or to find all field recordings from
a given month, and so on.

There is no special syntax to tag a recording---the appearance of
the word in the recording's associated annotation (i.e. text
file) is enough, and the tag search is really just a call to
grep. You can just type 

  \"dto field wildlife eastern-screech-owl\"

or 

  \"michael studio acoustic-guitar\"

when you annotate the file. See also
`flarc-audition-and-annotate'.

")

;; TODO (defun flarc-tag-search

;; TODO interactively prompt the user for an annotation

;;; Extracting files from various sources

;; An extractor function accepts a string identifying the resource or
;; device from which it should attempt to extract audio files. It
;; returns a shell command that FLARC can execute asynchronously
;; in order to extract the files.

;; Each extraction is called a "batch". 

;;; FLAC Compression

;; TODO asynchronous background job?

(defvar flarc-flac-command-string 
  "nohup flac --silent --decode-through-errors --best")

;; TODO remove wavs after flac encoding

(defun flarc-flac-encode (wave-file)
  (let ((command (concat flarc-flac-command-string wave-file "&")))
    (message "Running ..." command)
    (shell-command command)))
			 
;; (defun flarc-flac-decode
;; 


;;; Listen to files and annotate them

;;; Searching the annotations




(provide 'flarc)
;;; flarc.el ends here
