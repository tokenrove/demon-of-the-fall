
(in-package :vgdev-iso-cl)

(let ((debug-expression (make-array '(40) :element-type 'base-char
				    :fill-pointer t)))
  (defun paint-osd ()
    (paint-string "Equinox-ish..." 10 10 255 255 255)
    (setf (fill-pointer debug-expression) 0)
    (format debug-expression "~A"
	    (actor-position (gethash 0 *actor-map*)))
    (paint-string (coerce debug-expression 'simple-base-string)
		  10 30 255 255 255)))
