;;;
;;; graphics.lisp -- Basic graphics primitives.
;;;
;;; As-simple-as-possible SDL implementations, generally.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :game-fetus-alpha)

(defparameter *desired-width* 320)
(defparameter *desired-height* 240)
(defparameter *desired-bpp* 8)

(defvar *vbuffer* nil
  "Pointer to video double buffer.")


(defun create-display (&optional (fullscreen-p nil))
  "function CREATE-DISPLAY &optional fullscreen-p

Initialize the display, optionally fullscreen (beware setting this
option on undebugged code!)."
  (setf *vbuffer*
	(maybe-null->nil
	 (ll-gfx-init (bool->int fullscreen-p)
		      *desired-width* *desired-height* *desired-bpp*)))
  (assert *vbuffer*)
  t)

(defun new-image-buffer (w h)
  (maybe-null->nil (ll-gfx-new-image-buffer w h)))

(defun destroy-display ()
  "Shut down the display and free the double buffer."
  (ll-gfx-shutdown *vbuffer*))

(defun refresh-display ()
  "Flip double buffer, update current display."
  (ll-gfx-refresh-display *vbuffer*))

(defun surface-w (sface) (ll-gfx-surface-w sface))
(defun surface-h (sface) (ll-gfx-surface-h sface))
(defun display-width () (surface-w *vbuffer*))
(defun display-height () (surface-h *vbuffer*))

(defun fill-background (color &optional (destination *vbuffer*))
  "Fill the screen with a solid color."
  (ll-gfx-fill-rect-stub destination 0 0 -1 0 color))

#-openmcl (defun rectangle-set (rect x y w h)
  "Convenience function to set a gfx-rect's members in one swoop."
  (setf (get-slot-value rect 'gfx-rect 'x) x
	(get-slot-value rect 'gfx-rect 'y) y
	(get-slot-value rect 'gfx-rect 'w) w
	(get-slot-value rect 'gfx-rect 'h) h))

(defun load-image (filename &optional (colorkeyp nil))
  "function LOAD-IMAGE filename &optional colorkeyp => image

Load an image (in pretty much any sane format), optionally with color
zero flagged as transparent (when colorkeyp)."
  (with-cstring (name filename)
    (and filename
	 (maybe-null->nil (ll-gfx-load-image name (bool->int colorkeyp))))))

(defun free-image (image) (when image (ll-gfx-free-surface image)))

(defun use-image-palette (image)
  "function USE-IMAGE-PALETTE image

Sets the current display palette to whatever image is carrying around
with it."
  (when image (ll-gfx-use-image-palette image *vbuffer*)))

(defun blit-image (img x y &key (src-rect nil) (destination *vbuffer*))
  "function BLIT-IMAGE image x y

Blits the supplied image (with transparency if it's color-keyed) to
the double buffer, at point (X, Y)."
  (when img
    (decf y (if src-rect (fourth src-rect) (surface-h img)))
    (if src-rect
      (ll-gfx-blit-surface-stub img (first src-rect) (second src-rect)
                                (third src-rect) (fourth src-rect)
                                destination x y)
      (ll-gfx-blit-surface-stub img 0 0 -1 0 destination x y))
    t))

;;; Y'know, in Double Draggin', I implemented Wu-Rokne-Wyvill line
;;; drawing, because I was sick of Bresenham all the time.  After that
;;; I realized it totally wasn't worth it.
(defun draw-line (p-x p-y q-x q-y color &optional (surface *vbuffer*))
  "Draw a line between points P and Q, in color COLOR."
  (declare (optimize speed)
	   (type fixnum p-x p-y q-x q-y))
  (ll-gfx-lock-surface surface)
  (let* ((delta-x (- q-x p-x))
	 (delta-y (- q-y p-y))
	 (ax (the fixnum (* 2 (abs delta-x))))
	 (ay (the fixnum (* 2 (abs delta-y))))
	 (sign-x (if (> delta-x 0) 1 -1))
	 (sign-y (if (> delta-y 0) 1 -1))
	 (d 0))
    (declare (type fixnum d))

    (cond ((> ax ay)			; X dominant
	   (setf d (the fixnum (ash (- ay ax) -1)))
	   (loop
	    (when (and (<= 0 p-x (surface-w surface))
		       (<= 0 p-y (surface-h surface)))
	      (ll-gfx-draw-pixel surface p-x p-y color))
	    (when (= p-x q-x) (return))
	    (when (>= d 0)
	      (incf p-y sign-y)
	      (decf d ax))
	    (incf p-x sign-x)
	    (incf d ay)))

	  (t				; Y dominant
	   (setf d (the fixnum (ash (- ay ax) -1)))
	   (loop
	    (when (and (<= 0 p-x (surface-w surface))
		       (<= 0 p-y (surface-h surface)))
	      (ll-gfx-draw-pixel surface p-x p-y color))
	    (when (= p-y q-y) (return))
	    (when (>= d 0)
	      (incf p-x sign-x)
	      (decf d ay))
	    (incf p-y sign-y)
	    (incf d ax)))))
  (ll-gfx-unlock-surface surface))


(defun draw-rectangle (x y w h color &optional (surface *vbuffer*))
  (draw-line x y (+ x w) y color surface)
  (draw-line x y x (+ y h) color surface)
  (draw-line x (+ y h) (+ x w) (+ y h) color surface)
  (draw-line (+ x w) y (+ x w) (+ y h) color surface))

(defun draw-filled-rectangle (x y w h color &optional (surface *vbuffer*))
  (ll-gfx-fill-rect-stub surface x y w h color))


(defun draw-triangle (xs ys color &optional (surface *vbuffer*))
  (mapcar (lambda (opa opb)
	    (draw-line (funcall opa xs) (funcall opa ys)
		       (funcall opb xs) (funcall opb ys)
		       color surface))
	  (list #'first #'second #'third)
	  (list #'second #'third #'first)))

(defun draw-filled-triangle (xs ys color &optional (surface *vbuffer*))
  (let ((list (list)))
    (mapcar (lambda (opa opb)
	      (setf list
		    (helper-draw-line (funcall opa xs) (funcall opa ys)
				      (funcall opb xs) (funcall opb ys)
				      list (surface-h surface)))
	      nil)
	    (list #'first #'second #'third)
	    (list #'second #'third #'first))
    (dolist (raster list)
      (draw-filled-rectangle (second raster) (first raster)
			     (- (third raster) (second raster))
			     1 color surface))))

(defun helper-draw-line (p-x p-y q-x q-y list y-max)
  (declare (optimize speed)
	   (type fixnum p-x p-y q-x q-y)
	   (type list list))
  (let* ((delta-x (- q-x p-x))
	 (delta-y (- q-y p-y))
	 (ax (the fixnum (* 2 (abs delta-x))))
	 (ay (the fixnum (* 2 (abs delta-y))))
	 (sign-x (if (> delta-x 0) 1 -1))
	 (sign-y (if (> delta-y 0) 1 -1))
	 (d 0))
    (flet ((listplot (x y)
	     (aif (find y list :key #'first)
		  (if (< x (second it))
		      (setf (second it) x)
		      (when (> x (third it))
			(setf (third it) x)))
		  (push (list y x x) list))))
      (declare (type fixnum d))

      (cond ((> ax ay)			; X dominant
	     (setf d (the fixnum (ash (- ay ax) -1)))
	     (loop
	      (when (<= 0 p-y y-max)
		(listplot p-x p-y))
	      (when (= p-x q-x) (return))
	      (when (>= d 0)
		(incf p-y sign-y)
		(decf d ax))
	      (incf p-x sign-x)
	      (incf d ay)))

	    (t				; Y dominant
	     (setf d (the fixnum (ash (- ay ax) -1)))
	     (loop
	      (when (<= 0 p-y y-max)
		(listplot p-x p-y))
	      (when (= p-y q-y) (return))
	      (when (>= d 0)
		(incf p-x sign-x)
		(decf d ay))
	      (incf p-y sign-y)
	      (incf d ax))))))
  list)

