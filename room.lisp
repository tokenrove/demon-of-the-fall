;;;
;;; room.lisp -- Room management for Equinox-ish demo.
;;;
;;; Just floor drawing routines at the moment.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :vgdev-iso-cl)

;;;; FLOORS

(defconstant +tile-size+ 64)

(defvar *default-floor*
  #(#(0 0 0 1 0 0 0)
    #(0 0 1 1 1 0 0)
    #(0 1 1 1 1 1 0)
    #(0 0 1 1 1 0 0)
    #(0 0 1 1 1 0 0)
    #(1 1 1 1 1 1 1)
    #(1 1 1 1 1 1 1)
    #(0 0 1 1 1 0 0)
    #(0 0 1 1 1 0 0)))

(defun paint-floor (floor-img &optional (floor-slice *default-floor*))
  "function PAINT-FLOOR floor-tile-img &optional floor-slice => NIL

Paint floor tiles according to floor-slice.  Assumes floor-tile-img is
setup appropriately to tile at +TILE-SIZE+ intervals; paints from back
to front."

  ;; Draw order is from bottom-right of the array (furthest away from
  ;; the camera).
  (let ((h-extent (array-dimension (aref floor-slice 0) 0))
	(v-extent (array-dimension floor-slice 0)))
    (do ((z 0 (1+ z)))
	((>= z v-extent))
      (do ((x (1- h-extent) (1- x)))
	  ((< x 0))
	(let ((tile (aref (aref floor-slice z) x)))
	  (when (= tile 1)
	    (multiple-value-bind (u v)
		(iso-project-point (make-iso-point :x (* +tile-size+ x)
						   :y 0
						   :z (* +tile-size+ z)))
	      (blit-image floor-img nil u v))))))))


;;;; BLOCKS
