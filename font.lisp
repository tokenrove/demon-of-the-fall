;;;
;;; font.lisp -- Simple font routines.
;;;
;;; Uses SDL-TTF to do very primitive font things.  This is all pretty
;;; ugly, but who really cares about fonts?
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :vgdev-iso-cl)

(defvar *default-font* nil
  "The font used by PAINT-STRING, loaded via LOAD-DEFAULT-FONT.")

(defun font-init ()
  "This must be called before any other font routines are used."
  (ll-font-init))

(defun text-size (string)
  #+nil(cl-sdl-ttf:size-text *default-font* string))

(defun paint-string (string x y r g b)
  "Paints STRING on *VBUFFER* using *DEFAULT-FONT*, at position (X,Y)."
  (let ((sface (maybe-null->nil
		(ll-font-render-solid *default-font* string r g b))))
    (when sface
      (blit-image sface x (+ y (surface-h sface)))
      (free-image sface))))

(defun paint-blended-string (string x y r g b)
  "Paints STRING on *VBUFFER* using *DEFAULT-FONT*, at position (X,Y)."
  (let ((sface (maybe-null->nil
		(ll-font-render-blended *default-font* string r g b))))
    (when sface
      (blit-image sface x (+ y (surface-h sface)))
      (free-image sface))))

(defun paint-shaded-string (string x y r1 g1 b1 r2 g2 b2)
  "Paints STRING on *VBUFFER* using *DEFAULT-FONT*, at position (X,Y)."
  (let ((sface (maybe-null->nil
		(ll-font-render-shaded *default-font* string r1 g1 b1
				       r2 g2 b2))))
    (when sface
      (blit-image sface x (+ y (surface-h sface)))
      (free-image sface))))

(defun destroy-font ()
  "Frees *DEFAULT-FONT* appropriately."
  (unless (null *default-font*)
    (ll-font-close *default-font*))
  (setf *default-font* nil))

(defun load-default-font (file &optional (ptsize 12))
  "Loads FILE as *DEFAULT-FONT*."
  (setf *default-font* (maybe-null->nil (ll-font-open file ptsize))))
