;;;
;;; timer.lisp -- Frame-timing routines.
;;;
;;; Barebones.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :vgdev-iso-cl)

(defvar *frame-end*)

(defun sync-start-frame (&optional (frame-length 20))
  "function SYNC-START-FRAME &optional frame-length

Mark the start of a frame.  Default length is 20 miliseconds."
  (setf *frame-end* (+ (sdl:get-ticks) frame-length)))

;; XXX should return time difference, as well.
(defun sync-end-frame ()
  "function SYNC-END-FRAME

End one frame, and wait an appropriate amount of time to keep
consistent FPS.  Returns NIL for overrun, T if everything's cool."
  (let ((now (sdl:get-ticks)))
    (cond ((> now *frame-end*) nil)
	  (t (sdl:delay (- *frame-end* now))
	     t))))