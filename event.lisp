;;; event.lisp -- Event handler code.
;;;
;;; Very basic SDL event handling, mapping some keys to symbols.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :vgdev-iso-cl)

;; XXX it would be nice if this table used keysym names, rather than
;; hardcoded constants.
(defparameter *xlate-symbol->map-idx*
  '((27 :quit)				; ESC -> :quit
    (113 :quit)				; q -> :quit
    (273 :up)				; up -> :up
    (274 :down)				; down -> :down
    (276 :left)				; left -> :left
    (275 :right)			; right -> :right
    (122 :jump)				; z -> :jump
    )
  "Translates an SDL KeySym value to a symbolic *event-map* key.")

(defvar *event-map* (make-hash-table)
  "Hash which tracks whether or not a button is being pressed.")

(defmacro with-symbol->map-idx ((var sym) &body body)
  "macro WITH-SYMBOL->MAP-IDX (var sym) &body body

Binds var to the map index corresponding with the SDL KeySym passed in
sym, and executes body unless var is NIL."
  `(let ((,var (cadr (find ,sym *xlate-symbol->map-idx* :key #'car))))
    (when ,var ,@body)))

(defun event-update ()
  "function EVENT-UPDATE

Refresh the state of *EVENT-MAP*."
  ;;; XXX we need some kind of type fix here
  (with-foreign-object (event 'sdl:event)
    (do ((rv #1=(sdl:poll-event event) #1#))
	((= rv 0))
      (let ((type (sdl:event-type event)))
	(cond ((= type sdl:+key-down+)
	       (with-symbol->map-idx (sym (sdl:event-key-symbol event))
		 (setf (gethash sym *event-map*) t)))
	      ((= type sdl:+key-up+)
	       (with-symbol->map-idx (sym (sdl:event-key-symbol event))
		 (setf (gethash sym *event-map*) nil))))))))

(defun event-pressedp (ev)
  "Returns true if the given button is pressed."
  (gethash ev *event-map*))
