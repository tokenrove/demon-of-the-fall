
;;; base graphics

(in-package :vgdev-iso-cl)

(defparameter *desired-width* 320)
(defparameter *desired-height* 240)
(defparameter *desired-bpp* 8)

(defvar *vsurface*)
(defvar *vbuffer*)

(defun create-display (&optional (fullscreen-p nil))
  (sdl:init (logior sdl:+init-video+ sdl:+init-timer+))
  (let ((mode-flags (logior sdl:+hwsurface+ sdl:+swsurface+
			    sdl:+doublebuf+ sdl:+hwpalette+
			    (if fullscreen-p
				sdl:+fullscreen+ 0))))
    (setf *vsurface*
	  (sdl:set-video-mode *desired-width* *desired-height*
			      *desired-bpp* mode-flags))
    (setf *vbuffer* *vsurface*))
  (sdl:show-cursor 0)

  (unless (logtest (sdl:surface-flags *vsurface*) sdl:+doublebuf+)
    (let ((vsf (sdl:surface-format *vsurface*)))
      (setf *vbuffer*
	    (sdl:create-rgb-surface sdl:+swsurface+
				    (sdl:surface-w *vsurface*)
				    (sdl:surface-h *vsurface*)
				    (sdl:pixel-format-bits-per-pixel vsf)
				    (sdl:pixel-format-r-mask vsf)
				    (sdl:pixel-format-g-mask vsf)
				    (sdl:pixel-format-b-mask vsf)
				    (sdl:pixel-format-a-mask vsf)))))

  t)

(defun destroy-display ()
  (when (not (eql *vsurface* *vbuffer*))
    (sdl:free-surface *vbuffer*))
  (sdl:quit))

(defun refresh-display ()
  (unless (logtest (sdl:surface-flags *vsurface*) sdl:+doublebuf+)
    (sdl:blit-surface *vbuffer* nil *vsurface* nil))
  (sdl:flip *vsurface*))

(defun display-width () (sdl:surface-w *vsurface*))
(defun display-height () (sdl:surface-h *vsurface*))

(defun fill-background (color)
  (sdl:fill-rect *vbuffer* nil color))

(defun rectangle-set (rect x y w h)
  (setf (sdl:rect-x rect) x
	(sdl:rect-y rect) y
	(sdl:rect-w rect) w
	(sdl:rect-h rect) h))

(defun load-image (filename &optional (colorkeyp nil))
  (let ((image (sdl-img:load filename)))
    (when (null-pointer-p image) (setf image nil))
    (when (and image colorkeyp)
      (sdl:set-color-key image (logior sdl:+srccolorkey+ sdl:+rleaccel+)
			 0))
    image))

(defun free-image (image)
  (sdl:free-surface image))

(defun use-image-palette (image)
  (let ((palette (sdl:pixel-format-palette
		  (sdl:surface-format image))))
    (sdl:set-colors *vsurface*
		    (get-slot-value palette (:struct-pointer color)
				    'sdl::colors)
		    0
		    (get-slot-value palette int 'sdl::ncolors))
    (sdl:set-colors *vbuffer*
		    (get-slot-value palette (:struct-pointer color)
				    'sdl::colors)
		    0
		    (get-slot-value palette int 'sdl::ncolors))))

(defun blit-image (img src-rect x y)
  (sgum:with-foreign-objects ((dst-rect sdl:rect))
    (rectangle-set dst-rect x (- y (sdl:surface-h img)) 0 0)
    (sdl:blit-surface img src-rect *vbuffer* dst-rect)))

;;; Y'know, in Double Draggin', I implemented Wu-Rokne-Wyvill line
;;; drawing, because I was sick of Bresenham all the time.  After that
;;; I realized it totally wasn't worth it.
(defun draw-line (p-x p-y q-x q-y color &key (surface *vbuffer*))
  "Draw a line between points P and Q (stored as complex), in color
COLOR.  Don't use this at the moment as it is ponderously slow.  (due
to not making direct pixel access, I suspect.)"
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

