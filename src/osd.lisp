
(in-package :demon-of-the-fall)

(let ((debug-expression (make-array '(40) :element-type 'base-char
				    :fill-pointer t)))
  (defun paint-osd (font)
    ;; blended strings look good here, but slow things down a lot.
    (fetus:paint-string font "Equinox-ish..." 10 10 255 255 255)
    (setf (fill-pointer debug-expression) 0)
    (format debug-expression "~A" (equinox::room-name equinox::*current-room*))
    (fetus:paint-string font (coerce debug-expression 'simple-base-string)
			10 30 255 255 255)))
