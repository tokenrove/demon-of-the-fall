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
  (sdl-ttf:init))

(defun paint-string (string x y r g b)
  "Paints STRING on *VBUFFER* using *DEFAULT-FONT*, at position (X,Y)."
  (cl-sdl-ttf:with-solid-text (sface *default-font* string r g b)
    ;;; XXX needs type fix
    (sgum:with-foreign-objects ((rect 'sdl:rect))
      (setf (sdl:rect-x rect) x
	    (sdl:rect-y rect) y)
      (sdl:blit-surface sface nil *vbuffer* rect))))

(defun destroy-font ()
  "Frees *DEFAULT-FONT* appropriately."
  (unless (or (null *default-font*) (sgum:null-pointer-p *default-font*))
    (sdl-ttf:close-font *default-font*))
  (setf *default-font* nil))

(defun load-default-font (file &optional (ptsize 12))
  "Loads FILE as *DEFAULT-FONT*."
  (setf *default-font* (sdl-ttf:open-font file ptsize)))
