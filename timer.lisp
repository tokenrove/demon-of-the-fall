
(in-package :vgdev-iso-cl)

(defvar *frame-end*)

(defun sync-start-frame (&optional (frame-length 20))
  "Mark the start of a frame.  Default length is 20 miliseconds."
  (setf *frame-end* (+ (sdl:get-ticks) frame-length)))

(defun sync-end-frame ()
  "End one frame, and wait an appropriate amount of time to keep
consistent FPS.  Returns nil for overrun, t if everything's cool."
  (let ((now (sdl:get-ticks)))
    (cond ((> now *frame-end*) nil)
	  (t (sdl:delay (- *frame-end* now))
	     t))))