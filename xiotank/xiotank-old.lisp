
(defconstant +ticks-per-minute+ 60000 "Each tick is one millisecond.")

(defvar *beats-per-minute* 120)

(defun ticks-per-beat (bpm)
  (float (/ +ticks-per-minute+ bpm)))

(defvar *position* 0.0 "Song position in ticks. Fractional ticks are allowed.")

(defun position-seconds ()
  (float (/ *position* 1000)))

(defvar *wall-ticks* 0)

;;; Events

;; Events are lists the form (start-time &rest parameters-plist). For
;; events in a sequence, START-TIME (in ticks) is relative to the beginning of
;; the sequence's start time.

;;; Sequences

;; Sequences are sorted lists of events for a particular channel.

;;; Patterns 

;; Patterns are groups of sequences that play simultaneously on
;; multiple channels. A pattern is a vector of event sequences, with
;; the nth element being the sequence for channel N.

;; When a pattern is triggered, 

;;; Channels

;; A channel is a single synthesizer voice. The default channel type
;; triggers samples.

(define-prototype channel ()
  channel-number ;; integer
  )

(define-method play channel (&key sample volume)
  


