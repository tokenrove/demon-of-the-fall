
(in-package :vgdev-iso-cl)

;;;; FLOORS

(defvar *floor-description*
  #(#(0 0 1 1 1 0 0)
    #(0 0 1 1 1 0 0)
    #(1 1 1 1 1 1 1)
    #(1 1 1 1 1 1 1)
    #(0 0 1 1 1 0 0)
    #(0 0 1 1 1 0 0)))

(defun paint-floor (floor-img)
  "Paint floor tiles according to *floor-description*."
  (let ((h-extent (array-dimension (aref *floor-description* 0) 0))
	(v-extent (array-dimension *floor-description* 0)))
    (do ((y (1- v-extent) (1- y)))
	((< y 0))
      (do ((x (1- h-extent) (1- x)))
	  ((< x 0))
	;; lowest tile is x=0, y=1- v-extent
	;; first painted tile is x=1- h-extent, y=0, with world coords
	;;    (64*x, 0, -64*y)
	(let ((tile (aref (aref *floor-description* y) x)))
	  (when (= tile 1)
	    (multiple-value-bind (u v)
		(iso-project-point (* 64 x) 0 (* -64 y))
	      (blit-image floor-img nil u v))))))))


;;;; BLOCKS