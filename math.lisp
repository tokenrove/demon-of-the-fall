
(in-package :vgdev-iso-cl)

;;;; Isometric fu

(defstruct iso-point
  (x 0) (y 0) (z 0))

(defstruct box
  position dimensions)

(defun iso-project-point (p)
  "Project a world coordinate (3D) point onto screen coordinates.
Returns two values, X and Y in screen coordinates."
  (let ((sx (+ (ash (iso-point-x p) -1) (ash (iso-point-z p) -1)))
	(sy (+ (iso-point-y p)
	       (- (ash (iso-point-z p) -2)
		  (ash (iso-point-x p) -2)))))
    (setf sx (round (+ sx (ash (display-width) -1))))
    (setf sy (round (+ sy (display-height))))
    (values sx sy)))


(defun box-translate (box point)
  (make-box
   :position (make-iso-point
	      :x (+ (iso-point-x (box-position box)) (iso-point-x point))
	      :y (+ (iso-point-y (box-position box)) (iso-point-y point))
	      :z (+ (iso-point-z (box-position box)) (iso-point-z point)))
   :dimensions (box-dimensions box)))


(defun extents-overlap-p (a1 a2 b1 b2)
  (when (> a1 a2)
    (let ((tmp a1)) (setf a1 a2 a2 tmp)))
  (when (> b1 b2)
    (let ((tmp b1)) (setf b1 b2 b2 tmp)))
  (when (< a1 b1)
    (setf a1 b1))
  (when (> a2 b2)
    (setf a2 b2))
  (< a1 a2))

(defun boxes-overlap-p (a b)
  (and (let ((xa (iso-point-x (box-position a)))
	     (xb (iso-point-x (box-position b))))
	 (extents-overlap-p xa (+ xa (iso-point-x (box-dimensions a)))
			    xb (+ xb (iso-point-x (box-dimensions b)))))
       (let ((ya (iso-point-y (box-position a)))
	     (yb (iso-point-y (box-position b))))
	 (extents-overlap-p ya (+ ya (iso-point-y (box-dimensions a)))
			    yb (+ yb (iso-point-y (box-dimensions b)))))
       (let ((za (iso-point-z (box-position a)))
	     (zb (iso-point-z (box-position b))))
	 (extents-overlap-p za (+ za (iso-point-z (box-dimensions a)))
			    zb (+ zb (iso-point-z (box-dimensions b)))))))
