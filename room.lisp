
(in-package :vgdev-iso-cl)

;;;; FLOORS

(defconstant +tile-size+ 64)

(defvar *floor-description*
  #(#(0 0 1 1 1 0 0)
    #(0 0 1 1 1 0 0)
    #(1 1 1 1 1 1 1)
    #(1 1 1 1 1 1 1)
    #(0 0 1 1 1 0 0)
    #(0 0 1 1 1 0 0)))

(defun paint-floor (floor-img)
  "Paint floor tiles according to *floor-description*.  Assumes
FLOOR-IMG is setup appropriately to tile at +TILE-SIZE+ intervals;
paints from back to front."
  (let ((h-extent (array-dimension (aref *floor-description* 0) 0))
	(v-extent (array-dimension *floor-description* 0)))
    (do ((z (1- v-extent) (1- z)))
	((< z 0))
      (do ((x (1- h-extent) (1- x)))
	  ((< x 0))
	(let ((tile (aref (aref *floor-description* z) x)))
	  (when (= tile 1)
	    (multiple-value-bind (u v)
		(iso-project-point (make-iso-point :x (* +tile-size+ x)
						   :y 0
						   :z (* (- +tile-size+) z)))
	      (blit-image floor-img nil u v))))))))


;;;; BLOCKS