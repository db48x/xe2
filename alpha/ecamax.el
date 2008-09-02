;;; ecamax.el --- enhanced ecasound tools for emacs

;; Copyright (C) 2007  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: multimedia
;; Version: 0.1
;; $Id: ecamax.el,v 1.12 2007/01/26 22:19:02 dto Exp dto $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;;; Status: 

;; Fragmentary and unstable. Use at your own risk.

;;; Commentary:

;; ECAMAX is an experimental object-oriented frontend to Kai
;; Vehmanen's ecasound and some related programs (Mario Lang's audio
;; code for Emacs). 

;; Planned feature list:
;;  - Basic playback and transcoding tasks
;;  - Basic jackd/aconnect configuration GUI
;;  - Run jackd and ecasound as daemons
;;  - Network audio with jack.udp and jack.plumbing
;;  - Interactive audition/annotation/marking/extraction
;;  - Non-destructive recording (i.e. "takes") by default.
;;  - Automatic storage management and archiving
;;  - extract audio files from USB Mass Storage devices
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

;; Terminology comes first. Like a lot of audio software, we take the
;; term `session' to mean (a bit confusingly) a single multi-track
;; project (typically a song) that will in reality be recorded and
;; edited over the course of many studio "sessions". 

;; In ECAMAX, a session consists of a set of `tracks' and some audio
;; configuration data (sampling rate, etc.)  A track consists of an
;; ecasound input specifier (or `iospec'), an ecasound output iospec,
;; a set of `operations' to be performed, and finally a collection of
;; `takes'. A take identifies an individual audio file in a series of
;; recordings. A `take-set' is the set of new takes created when the
;; engine is started in record-enabled mode (this will be the null set
;; if there aren't any record-enabled tracks). The audio files for the
;; takes are stored in the session directory, which serves as the name
;; of the session. A session may be renamed by simply renaming the
;; directory it lives in.

;; Behind the scenes, ECAMAX configures ecasound and jackd to match
;; the specified recording/playback/routing setup, manages the files
;; for various takes, and so on.

;;; Dependencies

;; The communication with ecasound is via Mario Lang's ecasound.el,
;; which is included with ecasound itself. However, ECAMAX requires a
;; development version of ecasound.el. You can get that from
;; http://dtoweb.no-ip.org/e/ecasound-modified.el Mario Lang's jack.el
;; is also required.  http://delysid.org/emacs/jack.el (We are
;; planning to put this all into http://tromey.com/elpa )

;; You can download ecasound from http://eca.cx. Debian users may
;; install the "ecasound" package.

;;; Code:

(require 'ecasound)
(require 'radio)
(require 'usb)
(require 'jack)
(require 'eon)
;; (require 'cell)

;;; Specifying inputs and outputs 

;; An `iospec' is an expression designating a source or sink of audio
;; data. Examples:
;; 
;; (:jack "alsa_pcm" "capture_3")
;; ((:jack "alsa_pcm" "playback_1") (:jack "alsa_pcm" "playback_1"))
;; (:file "~/wildlife/nightbird.wav")
;; (:master)

;; Right now only file and JACK i/o are supported.

;; When an iospec is a list with multiple inputs or outputs (as in the
;; second example above) then that track will have the corresponding
;; number of channels.

(defun ecamax-iospec-to-string (iospec prefix)
  "Transform IOSPEC into a list of ecasound-formatted specifier strings."
  (cond 
   ;; it's a plain ecasound string. pass it through
   ((stringp iospec) (list iospec))
   ;; it's a list of iospecs
   ((and (listp iospec) (listp (car-safe iospec)))
    (mapcar (lambda (io)
	      (ecamax-iospec-to-string io prefix))
	    iospec))
   ;; it's a single iospec
   ((listp iospec) 
    (case (car iospec)
      (:jack (format "jack_generic,%s" prefix))
      (:file (expand-file-name (second iospec)))
      (:loop (error "loop devices not handled yet"))
      (:master "jack_alsa")))))
	    
;;; The ecamax audio engine

(defvar *ecamax-engine*)

(defobject+ :ecamax-engine 
    "An ecamax audio engine."
  record-enabled-p ; when non-nil, any record-enabled tracks will record nondestructively
  ecasound-buffer ; the buffer we use to communicate with ecasound
  session-directory ; directory where audio data is stored
  tracks ; the :ecamax-track objects
  jack-queued-connections
  jack-port-suffix-number
  marks 
  selected-track ; integer or nil
  ;; audio configuration
  sample-rate 
  period-size 
  device 
  )

(defmacro with-ecasound (&rest body)
  (declare (indent defun))
  `(with-current-buffer (@ :ecasound-buffer self)
     ,@body))

(defmethod+ :initialize :ecamax-engine (session-directory)
  "Start up ecasound daemon if necessary."
  (when (null @ecasound-buffer)
    (ignore-errors (eci-init))
    (setf @ecasound-buffer (get-buffer "*eci-ecasound*"))
    (with-ecasound (eci-cs-disconnect))
    (setf @record-enabled-p nil
	  @session-directory session-directory
	  @tracks nil
	  @jack-queued-connections nil
	  @jack-port-suffix-number 1
	  @marks nil
	  @selected-track nil
	  @sample-rate jack-sample-rate
	  @period-size jack-period-size
	  @device jack-alsa-device)))

(defmethod+ :state :ecamax-engine ()
  "Get the state of the ecasound engine."
  (with-ecasound (eci-engine-status)))

(defmethod+ :start :ecamax-engine ()
  "Start the engine."
  (with-ecasound (eci-start)))

(defmethod+ :stop :ecamax-engine ()
  "Stop the engine."
  (with-ecasound (eci-stop)))

(defmethod+ :position :ecamax-engine ()
  "Get the play position of the engine."
  (with-ecasound (eci-cs-get-length-samples)))

(defmethod+ :seek :ecamax-engine (seconds)
  "Set the play position of the engine, in seconds."
  (with-ecasound (eci-cs-set-position seconds)))	

(defmethod+ :seek-samples :ecamax-engine (samples)
  "Set the play position of the engine, in samples."
  (with-ecasound (eci-cs-set-position-samples samples)))   

(defmethod+ :add-track :ecamax-engine (&optional track)
  "Add TRACK to the track list, or a new track if TRACK is nil."
  (let ((new (or track (clone-object :ecamax-track self))))
    (push new @tracks)))

(defmethod+ :session-file :ecamax-engine (file-name)
  "Return the expanded file name for a file in this session."
  (expand-file-name file-name @session-directory))

(defmethod+ :load-session :ecamax-engine (session)
  "Load a session."
  FIXME)

(defmethod+ :configure-ecasound :ecamax-engine ()
  "Build current configuration as a chainsetup in ecasound."
  (setf @jack-queued-connections nil)
  (setf @jack-port-suffix-number 1)
  (with-ecasound
      ;; create new chainsetup
      (eci-cs-remove)
    (eci-cs-add "ecamax")
    (dolist (track @tracks)
      (>> :configure-ecasound track))
    (>>configure-jack)))
      
(defmethod+ :configure-jack :ecamax-engine ()
  (mapc #'jack-connect @jack-queued-connections))
  
(defun ecamax-find-session (session-directory)
  "Load session from the directory SESSION-DIRECTORY.
If no such session exists, create it. Return the engine
initialized with the session data."
  (interactive "sSession directory: ")
  (let* ((engine *ecamax-engine*)
	 (dir (@ :session-directory engine)))
    (if (file-exists-p dir)
	(if (file-directory-p dir)
	    ;; open existing session
	    nil ;; <:0bbea2b7-82e9-4dcf-9496-a463a6985289:>
	    (error "File exists, but is not a directory."))
	;; create new session
	(make-directory dir)
      (message "Created session in %s" dir))
    (>> :initialize engine session-directory)))
  
(defun ecamax-find-timestamped-session (name)
  (interactive "sSession name after timestamp: ")
  (let ((stamp (format-time-string "%Y-%m-%d-%H%M" (current-time))))
    (ecamax-find-session (concat stamp "." name))))

(defun ecamax-session-finish-take ()
  (interactive)
  (dolist (track (ecamax-session-tracks ecamax-selected-session))
    ;;
    ;; did we recently record anything? 
    (when (and ecamax-record-enabled-p (ecamax-track-record-p track))
      ;;
      ;; set update flag so that the cell sheet updates
      (setf (ecamax-track-update-p track) t)
      ;; 
      ;; select the take we just recorded for playback
      (setf (ecamax-track-selected-take track) (ecamax-track-next-take track))
      ;;
      ;; prepare for next take
      (incf (ecamax-track-next-take track)))))

(defmethod+ :queue-jack-connection :ecamax-engine (a b c d)
  (push (list a b c d) @jack-queued-connections)
  (incf @jack-port-suffix-number))

;;; Tracks for the audio engine to play and record
  
(defobject+ :ecamax-track
    "An audio track within an :ecamax-engine object."
  name ; string name of track. used in filenames, so don't use a slash
  engine ;; the engine that holds this track
  num-channels ; how many channels? usually 1 or 2
  input output ; sexps that are mapped to ecasound i/o strings
  operators ; list of strings in ecasound chainop syntax
  record-p ; whether to create a new take when recording
  play-p ; whether to hear the selected take (or current recording, when record-p is non-nil)
  selected-take ; which take plays when you listen back
  next-take ; number of next take to be recorded
  update-p ; whether any caches/widgets should be re-initialized from track data
  )
  
(defmethod+ :take-file :ecamax-track (&optional take-number)
  "Given a TRACK and an optional TAKE-NUMBER, produce the name of
the file where audio will be recorded for this take. If TAKE is
not given, assume the next take number."
  (>> :session-file @engine (format "%s.take-%d.wav"
				    (@ :name track)
				    (or take-number @next-take))))

(defmethod+ :initialize :ecamax-track (engine)
  "Create an empty track."	    
  (setf @name (object-uuid self)
	@engine engine
	@play-p t
	@next-take 1
	@num-channels 1
	@selected-take 0))

(defmethod+ :connect-input :ecamax-track (prefix iospec &optional mono-p)
  "Add an iospec input."
    (setf @input iospec)
    ;; handle any special connections
    (case (car iospec)
      (:file (when (not (file-exists-p (second iospec)))
	       (message "Warning: track input file %s does not exist." (second iospec))))
      (:jack
       (let* ((in-client (second iospec))
	      (in-port (third iospec))
	      (out-client "ecasound")
	      (out-port (concat prefix (format "_%d" @jack-port-suffix-number))))
	 (>> :queue-jack-connection @engine in-client in-port out-client out-port)))))

(defmethod+ :connect-output :ecamax-track (prefix iospec)
  "Add an iospec output."
  (setf @output iospec)
  ;; handle jack connections
  (when (eq :jack (car iospec))
    (let ((in-client "ecasound")
	  (in-port (concat prefix (format "_%d" @jack-port-suffix-number)))
	  (out-port (third iospec))
	  (out-client (second iospec)))
      (push (list in-client in-port out-client out-port)
	    (@ :jack-queued-connections @engine))
	 (incf (@ :jack-port-suffix-number @engine)))))
	   
(defun* ecamax-audio-format-string (num-channels &optional
						 (sample-rate jack-sample-rate))
    "Format a string suitable for telling ecasound what audio format to use."
    (format "f32_le,%d,%d,i" num-channels sample-rate))

(defmethod+ :configure-ecasound :ecamax-track ()
  (with-ecasound
      ;; set track format
      (eci-cs-set-audio-format (ecamax-audio-format-string @num-channels))
      ;; 
      ;; are we monitoring?
      (when @play-p
	(eci-c-add (concat @name "-monitor"))
	;; decide what to monitor: input or selected take? 
	(if (and @record-p 
		 (@ :record-enabled-p @engine))
	    ;; monitoring the live input. this will need jack connections.
	    (progn 
	      (eci-ai-add (ecamax-iospec-to-string @input @name))
	      (dotimes (channel @num-channels)
		(>> :queue-jack-connection @engine @name
		    (nth channel @input)
		    (= 1 channels))))
	    ;;
	    ;; monitoring the selected take.
	    (eci-ai-add (>>take-file @selected-take))))
	;;
	;; send to output
	(eci-ao-add (ecamax-iospec-to-string @output @name)
    ;;
    ;; are we recording? 
    (when (and @record-p (@ :record-enabled-p @engine))
      (eci-c-add (concat @name "-recording"))
      (eci-ai-add (ecamax-iospec-to-string (car @input) @name))
      (dotimes (channel @num-channels)
	(>> :queue-jack-connection (ecamax-track-name track) ;<:e001eaf0-f6dd-4463-a31b-5ecc4e217405:>
				 (nth channel inputs)
				 (= 1 channels)))
      (eci-ao-add (ecamax-track-take-file track))))))

(defun ecamax-start ()
  "Begin a new take. If ecamax-record-enabled-p is non-nil,
then record all tracks that are also record enabled."
  (interactive)
  (with-ecasound 
   (eci-start)
   ;;
   ;; FIXME: there has to be a better way of waiting until the engine is running
   (while (not (string= "running" (eci-engine-status)))
     (sit-for 0.1))
   (ecamax-session-to-jack)
   (setf ecamax-state :running)))

(defun ecamax-stop ()
  "Complete the current take. "
  (interactive)
  (with-ecasound (eci-cs-disconnect))
  (setf ecamax-state :stopped)
  ;;
  ;; turn off global record enable. this means you must explicitly
  ;; enable recording for each take.
  (setf ecamax-record-enabled-p nil)
  ;;
  ;; see (@> "sessions")
  (ecamax-session-finish-take))


;;; %%%%%%%%%%%%%%%%

(setf *ecamax-engine* (clone-object :ecamax-engine))
(with-object *ecamax-engine*
  (message (format "At position %S" (>>position)))
  (let ((track (>>add-track)))
    (setf (@ :input track) '(file "~/osb-1/audio/das-mystiche.wav")
	  (@ :output track) '(master))))
   
;;; ecaradio
;;                               _ _       
;;   ___  ___ __ _ _ __ __ _  __| (_) ___  
;;  / _ \/ __/ _` | '__/ _` |/ _` | |/ _ \ 
;; |  __/ (_| (_| | | | (_| | (_| | | (_) |
;;  \___|\___\__,_|_|  \__,_|\__,_|_|\___/ 
;;
;; concatenative synthesis with large files
;; (programmable crossfade and static effects)                                        


;; (@* "quickie functions to set up a marking session")


(defvar ecaradio-interactive-mark-number 1)


(defun ecaradio-interactive-mark ()
  (interactive)
  (ecasound-set-mark (format "%d" ecaradio-interactive-mark-number))
  (incf ecaradio-interactive-mark-number))
		     

;(global-set-key [(f6)] 'ecaradio-interactive-mark)

(defun ecaradio-interactive-mark-go (file)
  (interactive "fFilename: ")
  (ecasound)
  (setf ecaradio-interactive-mark-number 1)
  (eci-cs-disconnect)
  (eci-cs-add "ecasound")
  (eci-c-add "player")
  (eci-ai-add file)
  (eci-ao-add "jack_alsa")
  (ecaradio-interactive-mark)
  (eci-start))


(defvar ecaradio-internal-markers nil)

(defun ecaradio-markers ()
  ecaradio-internal-markers)

(defun ecaradio-grab-markers ()
  (interactive)
  (setf ecaradio-internal-markers
	(mapcar 'cdr (reverse
		      (cdr (assoc nil ecasound-markers))))))


(defun ecaradio-get-marker-pos (marker-number)
  (nth marker-number (ecaradio-markers)))


;(insert (format "%S" (ecaradio-markers) ))
; (setf my-album-markers '(0.0 30.092 258.216 260.805 406.439 519.549 537.722 593.464  693.169 727.801 852.317 876.724 906.3 979.913 1013.109 1135.827 1151.123 1244.131 1326.129 1333.292 1462.999 1486.181 1510.487 1688.187 1862.909 2014.364 2019.953 2035.345 2171.151 2199.828 2306.156 2363.387 2387.963 2494.098 2577.641 2717.237 2822.172 2914.746 3264.947 3352.404))


;; (@* "splitting a file at its markers")


(defvar ecaradio-split-output-directory "/tmp")


(defun ecaradio-split-file-on-markers (file &optional markers)
  (interactive "FFilename: ")
  (let* ((marker-list (or markers (ecaradio-markers)))
	 (num-markers (- ecaradio-interactive-mark-number 1))
	 (current-marker 1))
    (while (< current-marker num-markers)
      (let ((begin (ecaradio-get-marker-pos current-marker))
	    (end (or (ecaradio-get-marker-pos (1+ current-marker)) nil)))
	(ecaradio-split-on-position begin end current-marker file))
      (incf current-marker))))


(defun ecaradio-split-on-position (begin end split-number file)
  (message "Ecaradio-split-on-position: %S %S %S %S" begin end split-number file)
  (eci-cs-disconnect)
  (eci-cs-add (format "ecaradio-split-%d" split-number))
  (eci-c-add "player")
  (eci-ai-add file)
  (eci-ao-add (concat (file-name-as-directory ecaradio-split-output-directory)
		      "ecaradio-split-"
		      (number-to-string split-number)
		      ".wav"))
  (eci-ai-index-select 1)
  (eci-ai-set-position begin)
  (when end (eci-cs-set-length (- end begin)))
  (eci-start)
  (sit-for 3)
  (while (string= (eci-engine-status) "running")
    (sit-for 1)))


;; (@* "rendering a crossfaded ecaradio session")
  

(defvar ecaradio-crossfade-length 10.0)


(defvar ecaradio-static-source nil "String filename of static source.")
(defvar ecaradio-static-amp-percentage 60)
(defvar ecaradio-use-static t)

(defun ecaradio-make-ewf (outfile &rest args)
  (destructuring-bind (&key source offset start-position length looping &allow-other-keys)
      args
    (with-temp-buffer 
      (when source (insert (format "source = %s\n" source)))
      (when offset (insert (format "offset = %f\n" offset)))
      (when start-position (insert (format "start-position = %f\n" start-position)))
      (when length (insert (format "length = %f\n" length)))
      (when looping (insert "looping = true"))
      (write-file outfile))))
      

(defun ecaradio-measure-files (tracklist)
  (eci-cs-disconnect)
  (eci-cs-add "ecaradio-measure")
  (let ((counter 1)
	(track-files (mapcar (lambda (track)
			       (getf track :file))
			     tracklist))
	(lengths nil))
    (dolist (file track-files)
      (eci-c-add (format "%d" counter))
      (incf counter)
      (eci-ai-add file)
      (eci-ao-add "null"))
    (eci-start)
    (sit-for 5)
    (dolist (file track-files)
      (eci-ai-select file)
      (push (eci-ai-get-length) lengths))
    (reverse lengths)))
    

      
(defun ecaradio-render (outfile tracklist &optional measurements)
  (let ((counter 0)
	(position 0.0)
	(xfade-position 0.0)
	(track-files (mapcar (lambda (track)
			       (getf track :file))
			     tracklist))
	(lengths (or measurements (ecaradio-measure-files tracklist))))
    (eci-cs-disconnect)
    (eci-cs-add "ecaradio-render")
    (dolist (file track-files)
      (let ((ewf-file (concat file ".ewf")))
	;;
	;; overlap tracks by half the crossfade length
	(setf xfade-position (- position (/ ecaradio-crossfade-length 2.0)))
	(ecaradio-make-ewf ewf-file :source file :offset (if (< xfade-position 0)
							     position
							   xfade-position))
	(eci-c-add (format "%d" counter))
	(eci-ai-add ewf-file)
	(eci-ao-add "null")
	;; do cross-fade
	(eci-cop-add "-ea:0")
	(let* ((fadein-start xfade-position)
	       (fadein-end (+ ecaradio-crossfade-length fadein-start))
	       (fadeout-start (+ xfade-position (- (nth counter lengths) ecaradio-crossfade-length)))
	       (fadeout-end (+ fadeout-start ecaradio-crossfade-length))
	       (static-ewf-file (format "%s-%d.ewf" ecaradio-static-source counter)))
	  (eci-ctrl-add (format "-klg:1,0,100,4,%f,0,%f,1.0,%f,1.0,%f,0.0" 
				fadein-start fadein-end fadeout-start fadeout-end))
	  ;; add static
	  (when ecaradio-use-static
	    (ecaradio-make-ewf static-ewf-file :source ecaradio-static-source :offset fadeout-start
			       :start-position (random 40)
			       :length ecaradio-crossfade-length)
	    (eci-c-add (format "static-%d" counter))
	    (eci-ai-add static-ewf-file)
	    (eci-ao-add "null")
	    (eci-cop-add "-erc:1,2")
	    (eci-cop-add "-ea:0")
	    (eci-ctrl-add (format "-klg:1,0,%f,4,%f,0,%f,1.0,%f,1.0,%f,0.0"
				  ecaradio-static-amp-percentage
				  fadeout-start (+ fadeout-start 2.5)
				  (+ fadeout-start 5.0) (+ fadeout-start 10.0)))
	  ))
	;;
	(setf position (- (+ position (nth counter lengths)) (/ ecaradio-crossfade-length 2.0)))
	(incf counter)))
    (eci-c-select-all)
    (eci-ao-add outfile)
    (eci-ao-select outfile)
    (eci-ao-attach)
    (eci-cs-option "-z:mixmode,sum")))
      		      
;;; Cell-mode UI

;; (mapcar 'cell-register-prototype 
;; 	'(("track" :compute %ecamax-track :face ecamax-track-face)
;; 	  ("record-p" :compute %ecamax-record-p :face ecamax-record-p-face)
;; 	  ("monitor-p" :compute %ecamax-monitor-p :face ecamax-monitor-p-face)
;; 	  ("selected-take" :compute %ecamax-selected-take :face ecamax-selected-take-face)
;; 	  ("next-take" :compute %ecamax-next-take :face ecamax-next-take-face)
;; 	  ("input" :compute %ecamax-input :face ecamax-io-face)
;; 	  ("channels" :compute %ecamax-channels :face ecamax-track-face)
;; 	  ("output" :compute %ecamax-output :face ecamax-io-face)))

;; (defun ecamax-session-update-sheet ()
;;   (interactive)
;;   (with-current-cell-sheet
;;    (mapcar (lambda (cell)
;; 	     (funcall (cell-compute cell) cell :model))
;; 	   (cell-sheet-list sheet))
;;    ;; clear update flags on tracks
;;    (dolist (track (ecamax-session-tracks ecamax-selected-session))
;;      (setf (ecamax-track-update-p track) nil))
;;    ;; update the screen
;;    (cell-sheet-update)))

;; (defun ecamax-go ()
;;   (interactive)
;;   (ecamax-session-update-sheet)
;;   (ecamax-session-to-ecasound))


;; ; (global-set-key [(f11)] 'ecamax-go)

;; (defun %ecamax-track (cell message)
;;   (with-cell cell
;; 	     (when (null state)
;; 	       (setf label "No track name!"))
;; 	     (case message
;; 	       (:edit 
;; 		(setf state (read-from-minibuffer "Track name: " state))
;; 		(setf label state))
;; 	       (:model 
;; 		;; select the track
;; 		(ecamax-select-track state)
;; 		;;
;; 		;; create track if needed
;; 		(when (not (ecamax-track-get state))
;; 		  (ecamax-track-add state))))))
		      

;; (defun %ecamax-record-p (cell message)
;;   (with-cell cell
;; 	     (if state 
;; 		 (setf label "record")
;; 	       (setf label " -- "))
;; 	     (case message
;; 	       (:model (setf (ecamax-track-record-p ecamax-selected-track)
;; 			     state))
;; 	       (:click (setf state (if state nil t))))))


;; (defun %ecamax-monitor-p (cell message)
;;   (with-cell cell
;; 	     (if state 
;; 		 (setf label "monitor")
;; 	       (setf label " -- "))
;; 	     (case message
;; 	       (:model (setf (ecamax-track-monitor-p ecamax-selected-track)
;; 			     state))
;; 	       (:click (setf state (if state nil t))))))


;; (defun %ecamax-selected-take (cell message)
;;   (with-cell cell
;; 	     (if state
;; 		 (setf label (format "take %d" state))
;; 	       (setf state 1)
;; 	       (setf label "1"))
;; 	     (case message
;; 	       (:edit 
;; 		(let ((entry (read-from-minibuffer "Select take number: " (format "%d" state) nil :read)))
;; 		  (when (numberp entry)
;; 		    (setf state entry))))
;; 	       (:model 
;; 		;;
;; 		;; should we set the selected track, or update the widget when the selected track
;; 		;; has been changed automatically? 
;; 		(if (ecamax-track-update-p ecamax-selected-track)
;; 		    (setf state (ecamax-track-selected-take ecamax-selected-track))
;; 		  (setf (ecamax-track-selected-take ecamax-selected-track) state))))))
	       

;; (defun %ecamax-next-take (cell message)
;;   (with-cell cell
;; 	     (if state
;; 		 (setf label (format "take %d" state))
;; 	       (setf label "no next take?"))
;; 	     (case message
;; 	       (:model (setf state (ecamax-track-next-take ecamax-selected-track))))))


;; (defun %ecamax-input (cell message)
;;   (with-cell cell 
;; 	     (case message
;; 	       (:edit
;; 		(setf state (read-from-minibuffer "Lisp expression: " (format "%S" state) nil :read)))
;; 	       (:model 
;; 		(setf (ecamax-track-input ecamax-selected-track) state)))	     
;; 	     (setf label (format "%S" state))))


;; (defun %ecamax-output (cell message)
;;   (with-cell cell 
;; 	     (case message
;; 	       (:edit
;; 		(setf state (read-from-minibuffer "Lisp expression: " (format "%S" state) nil :read)))
;; 	       (:model
;; 		(setf (ecamax-track-output ecamax-selected-track) state)))
;; 	     (setf label (format "%S" state))))


;; (defun %ecamax-channels (cell message)
;;   (with-cell cell
;; 	     (when (null state)
;; 	       (setf state 1))
;; 	     (setf label (format "%d" state))
;; 	     (case message
;; 	       (:edit 
;; 		(setf state (read-from-minibuffer "Number of channels: " 
;; 						  (format "%S" state) nil :read)))
;; 	       (:model 
;; 		(setf (ecamax-track-num-channels ecamax-selected-track) state)))))	      		
	       	  	     

;; (defface ecamax-track-face '((t (:foreground "gray50" :background "white")))
;;   "Face for ecamax controls.")

;; (defface ecamax-record-p-face '((t (:foreground "yellow" :background "red")))
;;   "Face for ecamax controls.")

;; (defface ecamax-monitor-p-face '((t (:foreground "yellow" :background "forestgreen")))
;;   "Face for ecamax controls.")

;; (defface ecamax-selected-take-face '((t (:foreground "white" :background "gray20")))
;;   "Face for ecamax controls.")

;; (defface ecamax-next-take-face '((t (:foreground "red" :background "white")))
;;   "Face for ecamax controls.")

;; (defface ecamax-io-face '((t (:foreground "cyan" :background "navyblue")))
;;   "Face for ecamax controls.")

					  
					  @tracks)

;; (@ :ecasound-buffer *ecamax-engine*)
;; (>> :position *ecamax-engine*)
;; (>> :add-track *ecamax-engine*)
;; (@ :tracks *ecamax-engine*) 

;;; %%%%%%%%%%%%%%%%

;; (defun ecamax-track-remove (track-name)
;;   (interactive "sTrack name: ")
;;   (setf (ecamax-session-tracks ecamax-selected-session)
;; 	(remove-if (lambda (track)
;; 		     (string= track-name (ecamax-track-name track)))
;; 		   (ecamax-session-tracks ecamax-selected-session))))

;; (defun ecamax-track-get (track-name)
;;   (interactive)
;;   (find-if (lambda (track)
;; 	     (string= track-name (ecamax-track-name track))) 
;; 	   (ecamax-session-tracks ecamax-selected-session)))

;; (defun ecamax-select-track (track-name)
;;   (setf ecamax-selected-tack-name track-name)
;;   (setf ecamax-selected-track (ecamax-track-get track-name)))


;;; $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

         


(provide 'ecamax)
;;; ecamax.el ends here
