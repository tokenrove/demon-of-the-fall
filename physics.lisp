;;;
;;; physics.lisp -- Physics routines.
;;;
;;; Totally fucked at the moment.  Needs to be reworked to deal with
;;; arbitrary frame timings.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :vgdev-iso-cl)

;;; XXX Each of these constants is totally arbitrary.
(defparameter *air-friction* 0.1
  "Resistance against actors while in the air.")
(defparameter *ground-friction* 0.5
  "Friction on the floor.  Note that this parameter will not be here
forever as friction becomes delegated to surfaces.")
(defparameter *gravity* -0.5
  "Gravity affecting actors.")
(defparameter *terminal-velocity* 16
  "Terminal velocity (currently, in any direction).")

(defun update-physics (a)
  "Update simple kinematics, friction, collision response on the given
actor."
  (dolist (axis '(:x :y :z))
    (clampf (iso-point-component axis (actor-velocity a))
	    *terminal-velocity*))

  (incf (iso-point-x (actor-position a)) (iso-point-x (actor-velocity a)))
  (incf (iso-point-z (actor-position a)) (iso-point-z (actor-velocity a)))
  (incf (iso-point-y (actor-position a)) (iso-point-y (actor-velocity a)))


  (detect-collisions a)
  ;;; call contact handlers?

  (if (actor-contact-surface a)
       ;; standing on something.
       (progn
	 ;; only if in contact with something below us.
	 (sinkf (iso-point-x (actor-velocity a)) *ground-friction*)
	 (sinkf (iso-point-z (actor-velocity a)) *ground-friction*))

       ;; in the air.
       (progn
	 (sinkf (iso-point-y (actor-velocity a)) *air-friction*)
	 (incf (iso-point-y (actor-velocity a)) *gravity*))))


(defun detect-collisions (alice)
  (when (and (actor-contact-surface alice)
	     (not (penetrating-p alice (actor-contact-surface alice))))
    (setf (actor-contact-surface alice) nil))

  (maphash (lambda (unused bob)
	     (declare (ignore unused))
	     (when (and (not (eql alice bob)) (penetrating-p alice bob))
	       (multiple-value-bind (impulse lsa not-lsa)
		   (resolve-collision alice bob)
		 (apply-impulse alice lsa
				(iso-point-component lsa impulse))
		 (dolist (axis not-lsa)
		   (decf (iso-point-component axis (actor-position alice))
			 (iso-point-component axis impulse)))
		   
		 ;; when lsa is :y...
		 (when (and (eql lsa :y)
			    (> (iso-point-y (actor-position alice))
			       (iso-point-y (actor-position bob))))
		   (setf (actor-contact-surface alice) bob))

		 (awhen (find (actor-type bob) *actor-archetypes* :key #'car)
			(funcall (cadr (assoc :contact (cdr it)))
				 bob alice lsa impulse)))))
	   *actor-map*)

  (room-collision-detection alice))


(defun room-collision-detection (alice)
  (let ((w (ceiling (iso-point-x (box-dimensions (actor-box alice)))
		    +tile-size+))
	(d (ceiling (iso-point-z (box-dimensions (actor-box alice)))
		    +tile-size+))
	(slice 0))
    (do* ((base-z (floor (iso-point-z (actor-position alice)) +tile-size+))
	  (z base-z (1+ z)))
	 ((> z (+ base-z d)))
      (do* ((base-x (floor (iso-point-x (actor-position alice)) +tile-size+))
	    (x base-x (1+ x)))
	   ((> x (+ base-x w)))
	(if (plusp slice)
	    (try-slice-collision alice slice x z)
	    (try-floor-collision alice x z))))))


(defun try-slice-collision (alice slice x z)
  nil)

(defun make-wall-object (x z)
  (let ((wall (make-instance 'actor)))
    (setf (actor-position wall) #I((* x +tile-size+) 0
				   (* z +tile-size+)))
    (setf (actor-box wall)
	  (make-box :position #I(0 0 0)
		    :dimensions #I(+tile-size+
				   *room-highest-point*
				   +tile-size+)))
    wall))

(defun make-floor-object (x z)
  (let ((floor (make-instance 'actor)))
    (setf (actor-position floor) #I((* x +tile-size+) -16
				   (* z +tile-size+)))
    (setf (actor-box floor)
	  (make-box :position #I(0 0 0)
		    :dimensions #I(+tile-size+
				   16
				   +tile-size+)))
    floor))

(defun try-floor-collision (alice x z)
  (if (or (minusp x) (minusp z))
      (let ((wall-obj (make-wall-object x z)))
	(when (penetrating-p alice wall-obj)
	  (multiple-value-bind (impulse lsa not-lsa)
	      (resolve-collision alice wall-obj)
	    (incf (iso-point-component lsa (actor-velocity alice))
		  (iso-point-component lsa impulse))
	    (dolist (axis not-lsa)
	      (decf (iso-point-component axis (actor-position alice))
		    (iso-point-component axis impulse))))))
      (let ((floor-obj (make-floor-object x z)))
	(when (penetrating-p alice floor-obj)
	  (multiple-value-bind (impulse lsa not-lsa)
	      (resolve-collision alice floor-obj)
	    (incf (iso-point-component lsa (actor-velocity alice))
		  (iso-point-component lsa impulse))
	    (dolist (axis not-lsa)
	      (decf (iso-point-component axis (actor-position alice))
		    (iso-point-component axis impulse))))
	  (setf (actor-contact-surface alice) floor-obj)))))


(defun sign-of (v)
  (cond ((plusp v) 1)
	((minusp v) -1)
	(t nil)))

;; eventually this will use a cool trick like in game physics section
;; 5.3 or so.
(defun resolve-collision (alice bob)
  (let ((impulse #I(0 0 0)))
    ;; handle fractional component first.
    (dolist (axis '(:x :y :z))
      (let* ((f (iso-point-component-function-of axis))
	     (amin (iso-point-component axis (actor-position alice)))
	     (bmin (iso-point-component axis (actor-position bob))))
	(when
	    (extents-overlap-p
	     amin (+ amin (funcall f (box-dimensions (actor-box alice))))
	     bmin (+ bmin (funcall f (box-dimensions (actor-box bob)))))
	  (multiple-value-bind (int frac)
	      (floor (funcall f (actor-velocity alice)))
	    (declare (ignore int))
	    (decf (iso-point-component axis (actor-position alice))
		  frac)
	    (decf (iso-point-component axis impulse) frac)))))

    ;; pixel by pixel step, otherwise.
    (do ((max-iterations 25 (1- max-iterations)))
	((or (not (penetrating-p alice bob))
	     (zerop max-iterations)))
      (dolist (axis '(:x :y :z))
	(let* ((f (iso-point-component-function-of axis))
	       (amin (funcall f (actor-position alice)))
	       (bmin (funcall f (actor-position bob))))
	  (when
	      (extents-overlap-p
	       amin (+ amin (funcall f (box-dimensions (actor-box alice))))
	       bmin (+ bmin (funcall f (box-dimensions (actor-box bob)))))
	    (awhen (sign-of (funcall f (actor-velocity alice)))
		   (decf (iso-point-component axis
					      (actor-position alice)) it)
		   (decf (iso-point-component axis impulse) it))))))
    
    (let (lsa not-lsa)
      (dolist (axis '(:x :y :z))
	(let* ((f (iso-point-component-function-of axis))
	       (amin (funcall f (actor-position alice)))
	       (bmin (funcall f (actor-position bob))))
	  (if (extents-overlap-p
	       amin (+ amin (funcall f (box-dimensions (actor-box alice))))
	       bmin (+ bmin (funcall f (box-dimensions (actor-box bob)))))
	      (push axis not-lsa)
	      (setf lsa axis))))
      (values impulse lsa not-lsa))))


(defun apply-impulse (actor &key (x 0.0) (y 0.0) (z 0.0))
  (incf (iso-point-x (actor-velocity actor)) x)
  (incf (iso-point-y (actor-velocity actor)) y)
  (incf (iso-point-z (actor-velocity actor)) z))


(defun report-axes-of-separation (alice-box bob-box)
  (let (separation)
    (dolist (axis '(:x :y :z))
      (let ((amin (iso-point-component axis (box-position alice-box)))
	    (bmin (iso-point-component axis (box-position bob-box))))
	(unless
	    (extents-overlap-p
	     amin (+ amin (iso-point-component axis
					       (box-dimensions alice-box)))
	     bmin (+ bmin (iso-point-component axis
					       (box-dimensions bob-box))))
	  (push axis separation))))
    separation))

(defun penetrating-p (alice bob)
  (let ((alice-box (box-translate (actor-box alice)
				  (actor-position alice)))
	(bob-box (box-translate (actor-box bob)
				  (actor-position bob))))
    (boxes-overlap-p alice-box bob-box)))

