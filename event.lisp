
(in-package :vgdev-iso-cl)

(defparameter *xlate-symbol->map-idx*
  '((27 :quit)				; ESC -> :quit
    (113 :quit)				; q -> :quit
    (273 :up)				; up -> :up
    (274 :down)				; down -> :down
    (276 :left)				; left -> :left
    (275 :right)			; right -> :right
    (122 :jump)				; z -> :jump
    ))
(defvar *event-map* (make-hash-table))

(defun event-update ()
  (sgum:with-foreign-objects ((event sdl:event))
    (do ((rv #1=(sdl:poll-event event) #1#))
	((= rv 0))
      (let ((type (sdl:event-type event)))
	(cond ((= type sdl:+key-down+)
	       (let ((ev-sym (cadr (find (sdl:event-key-symbol event)
					 *xlate-symbol->map-idx*
					 :key #'car))))
		 (when ev-sym
		   (setf (gethash ev-sym *event-map*) t))))
	      ((= type sdl:+key-up+)
	       (let ((ev-sym (cadr (find (sdl:event-key-symbol event)
					 *xlate-symbol->map-idx*
					 :key #'car))))
		 (when ev-sym
		   (setf (gethash ev-sym *event-map*) nil)))))))))

(defun event-pressedp (ev) (gethash ev *event-map*))
