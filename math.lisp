;;;
;;; math.lisp -- Math and computational geometry routines.
;;;
;;; Contains some physics helpers, convenience macros, isometric
;;; projection stuff, low-level collision detection routines.
;;; Lots of refactoring is needed here.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :vgdev-iso-cl)

;;;; Generic

(defmacro sinkf (place value)
  "Sink the value of PLACE by VALUE, reducing to zero if |PLACE| <=
|VALUE|."
  (let ((gplace (gensym))
	(gval (gensym)))
    `(let ((,gval ,value)
	   (,gplace ,place))
      (cond ((>= (abs ,gval) (abs ,gplace)) (setf ,place 0))
	    ((or (and (plusp ,gplace) (plusp ,gval))
		 (and (minusp ,gplace) (minusp ,gval))) (decf ,place ,gval))
	    (t (incf ,place ,gval))))))

;; Note that we could add compiler macros here to do things like
;; precompute constant values, and use ash instead of / when the type
;; is integer.
(defmacro half (x)
  "Convenience division macro."
  `(/ ,x 2))

(defmacro quarter (x)
  "Convenience division macro."
  `(/ ,x 4))


;;;; Isometric fu

(defstruct iso-point
  (x 0 :type number) (y 0 :type number) (z 0 :type number))

(defun |#I-reader| (stream subchar arg)
  "Make an iso-point from a list #I(x y z)."
  (declare (ignore subchar arg))
  (let ((vals (read stream t nil t)))
    (unless (= 3 (length vals))
      (error "iso-point requires x, y, and z values exactly."))
    (list 'make-iso-point :x (first vals) :y (second vals)
	  :z (third vals))))
(set-dispatch-macro-character #\# #\I #'|#I-reader|)

(defstruct box
  position dimensions)

(defun iso-project-point (p)
  "Project a world coordinate (3D) point onto screen coordinates.
Returns two values, X and Y in screen coordinates.  This is *before*
camera positioning."
  (let ((sx (+ (half (iso-point-x p)) (half (iso-point-z p))))
	(sy (+ (iso-point-y p)
	       (- (quarter (iso-point-z p))
		  (quarter (iso-point-x p))))))
    (incf sy (half (display-height)))
    (values (round sx) (round sy))))

;;; XXX this and the following functions should have non-consing
;;; variants for sane in-game use.
(defun iso-point-translate (point origin)
  (make-iso-point
   :x (+ (iso-point-x point) (iso-point-x origin))
   :y (+ (iso-point-y point) (iso-point-y origin))
   :z (+ (iso-point-z point) (iso-point-z origin))))

(defun box-translate (box point)
  (make-box
   :position (iso-point-translate (box-position box) point) 
   :dimensions (box-dimensions box)))


(defun extents-penetrate-p (a1 a2 b1 b2)
  (when (> a1 a2) (psetf a1 a2 a2 a1))
  (when (> b1 b2) (psetf b1 b2 b2 b1))
  (when (< a1 b1) (setf a1 b1))
  (when (> a2 b2) (setf a2 b2))
  (< a1 a2))

(defun extents-contact-p (a1 a2 b1 b2)
  (when (> a1 a2) (psetf a1 a2 a2 a1))
  (when (> b1 b2) (psetf b1 b2 b2 b1))
  (when (< a1 b1) (setf a1 b1))
  (when (> a2 b2) (setf a2 b2))
  (<= a1 a2))

(defun extents-overlap-p (a1 a2 b1 b2)
  (when (> a1 a2) (psetf a1 a2 a2 a1))
  (when (> b1 b2) (psetf b1 b2 b2 b1))
  (when (< a1 b1) (setf a1 b1))
  (when (> a2 b2) (setf a2 b2))
  (< a1 a2))

(defun extents-overlap-p (amin amax bmin bmax)
  (and (> amax bmin) (> bmax amin)))

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
