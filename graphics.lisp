;;;
;;; graphics.lisp -- Basic graphics primitives.
;;;
;;; As-simple-as-possible SDL implementations, generally.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :vgdev-iso-cl)

(defparameter *desired-width* 320)
(defparameter *desired-height* 240)
(defparameter *desired-bpp* 8)

(defvar *video-surface* nil
  "Actual video display surface.")
(defvar *vbuffer* nil
  "Double-buffer.  Where everything should be drawn; flipped onto
*VIDEO-SURFACE* when REFRESH-DISPLAY is called.  Might be equivalent to
*VIDEO-SURFACE*.")

(defun create-similar-surface (surface)
  "Convenience function."
  (let* ((format (sdl:surface-format surface)))
    (sdl:create-rgb-surface sdl:+swsurface+
			    (sdl:surface-w surface)
			    (sdl:surface-h surface)
			    (sdl:pixel-format-bits-per-pixel format)
			    (sdl:pixel-format-r-mask format)
			    (sdl:pixel-format-g-mask format)
			    (sdl:pixel-format-b-mask format)
			    (sdl:pixel-format-a-mask format))))

(defun create-display (&optional (fullscreen-p nil))
  "function CREATE-DISPLAY &optional fullscreen-p

Initialize the display, optionally fullscreen (beware setting this
option on undebugged code!)."
  (sdl:init (logior sdl:+init-video+ sdl:+init-timer+))
  (let ((mode-flags (logior sdl:+hwsurface+ sdl:+swsurface+
			    sdl:+doublebuf+ sdl:+hwpalette+
			    (if fullscreen-p
				sdl:+fullscreen+ 0))))
    (setf *video-surface*
	  (sdl:set-video-mode *desired-width* *desired-height*
			      *desired-bpp* mode-flags))
    (setf *vbuffer* *video-surface*))
  (sdl:show-cursor 0)

  (unless (logtest (sdl:surface-flags *video-surface*) sdl:+doublebuf+)
    (setf *vbuffer* (create-similar-surface *video-surface*)))

  t)

(defun new-image-buffer (w h)
  (let* ((format (sdl:surface-format *vbuffer*))
	 (surface (sdl:create-rgb-surface sdl:+swsurface+
					  w h
					  (sdl:pixel-format-bits-per-pixel format)
					  (sdl:pixel-format-r-mask format)
					  (sdl:pixel-format-g-mask format)
					  (sdl:pixel-format-b-mask format)
					  (sdl:pixel-format-a-mask format)))
	 (palette (sdl:pixel-format-palette format)))
    (flet ((set-colors (sface)
	     (sdl:set-colors sface
			     (get-slot-value palette (:struct-pointer color)
					     'sdl::colors)
			     0
			     (get-slot-value palette int 'sdl::ncolors))))
      (set-colors surface)
      (sdl:set-color-key surface (logior sdl:+srccolorkey+ sdl:+rleaccel+)
			 0)
      surface)))

(defun destroy-display ()
  "Shut down the display and free the double buffer.  Doesn't fully
deinitialize SDL due to some bad interaction with CMUCL."
  (when (not (eql *video-surface* *vbuffer*))
    (sdl:free-surface *vbuffer*))
  (sdl:quit-subsystem sdl:+init-video+))

(defun refresh-display ()
  "Flip double buffer, update current display."
  (unless (logtest (sdl:surface-flags *video-surface*) sdl:+doublebuf+)
    (sdl:blit-surface *vbuffer* nil *video-surface* nil))
  (sdl:flip *video-surface*))

(defun display-width () (sdl:surface-w *video-surface*))
(defun display-height () (sdl:surface-h *video-surface*))

(defun fill-background (color &optional (destination *vbuffer*))
  "Fill the screen with a solid color."
  (sdl:fill-rect destination nil color))

(defun rectangle-set (rect x y w h)
  "Convenience function to set an sdl:rect's members in one swoop."
  (setf (sdl:rect-x rect) x
	(sdl:rect-y rect) y
	(sdl:rect-w rect) w
	(sdl:rect-h rect) h))

(defun load-image (filename &optional (colorkeyp nil))
  "function LOAD-IMAGE filename &optional colorkeyp => image

Load an image (in pretty much any sane format), optionally with color
zero flagged as transparent (when colorkeyp)."
  (let ((image (and filename (sdl-img:load filename))))
    (when (null-pointer-p image) (setf image nil))
    (when (and image colorkeyp)
      (sdl:set-color-key image (logior sdl:+srccolorkey+ sdl:+rleaccel+)
			 0))
    image))

(defun free-image (image)
  (sdl:free-surface image))

(defun use-image-palette (image)
  "function USE-IMAGE-PALETTE image

Sets the current display palette to whatever image is carrying around
with it."
  (let ((palette (sdl:pixel-format-palette
		  (sdl:surface-format image))))
    (flet ((set-colors (sface)
	     (sdl:set-colors sface
			     (get-slot-value palette (:struct-pointer color)
					     'sdl::colors)
			     0
			     (get-slot-value palette int 'sdl::ncolors))))
      (set-colors *video-surface*)
      (set-colors *vbuffer*))))

(defvar *scratch-rectangle* (sgum:with-foreign-objects ((dst-rect sdl:rect))
			      dst-rect))

(defun blit-image (img src-rect x y &optional (destination *vbuffer*))
    "function BLIT-IMAGE image src-rect x y

Blits the supplied image (with transparency if it's color-keyed) to
the double buffer, at point (X, Y).  Note that this routine hasn't
fully stabilized its interface; in particular, the origin point may
not mean ``from the upper-left corner of the sprite''."
    (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;;; XXX fix types
    (rectangle-set *scratch-rectangle* x (- y (if src-rect
						  (sdl:rect-h src-rect)
						  (sdl:surface-h img))) 0 0)
    (sdl:blit-surface img src-rect destination *scratch-rectangle*))

;;; Y'know, in Double Draggin', I implemented Wu-Rokne-Wyvill line
;;; drawing, because I was sick of Bresenham all the time.  After that
;;; I realized it totally wasn't worth it.
(defun draw-line (p-x p-y q-x q-y color &key (surface *vbuffer*))
  "Draw a line between points P and Q (stored as complex), in color
COLOR.

XXX: Don't use this at the moment as it is ponderously slow.  (due to
not making direct pixel access, I suspect.)  Use SDL:DRAW-LINE
instead."
  (sdl:lock-surface surface)
  (let* ((delta-x (- q-x p-x))
	 (delta-y (- q-y p-y))
	 (ax (* 2 (abs delta-x)))
	 (ay (* 2 (abs delta-y)))
	 (sign-x (if (> delta-x 0) 1 -1))
	 (sign-y (if (> delta-y 0) 1 -1)))

    (cond ((> ax ay)			; X dominant
	   (let ((d (/ (- ay ax) 2)))
	     (loop
	      (sdl:faster-draw-pixel surface p-x p-y color)
	      (when (= p-x q-x) (return))
	      (when (>= d 0)
		(incf p-y sign-y)
		(decf d ax))
	      (incf p-x sign-x)
	      (incf d ay))))

	  (t				; Y dominant
	   (let ((d (/ (- ay ax) 2)))
	     (loop
	      (sdl:faster-draw-pixel surface p-x p-y color)
	      (when (= p-y q-y) (return))
	      (when (>= d 0)
		(incf p-x sign-x)
		(decf d ay))
	      (incf p-y sign-y)
	      (incf d ax))))))
  (sdl:unlock-surface surface))
