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
(defparameter *ground-friction* 0.25
  "Friction on the floor.  Note that this parameter will not be here
forever as friction becomes delegated to surfaces.")
(defparameter *gravity* 1.5
  "Gravity affecting actors.")

(defun update-physics (actor)
  "Update simple kinematics, friction, collision response on the given
actor.  Assumes collisions have already been flagged by this point."
  (update-vertical-physics actor)
  (update-horizontal-physics actor))

(defun update-horizontal-physics (a)
  "Integrate acceleration into velocity, velocity into position,
update position and velocity according to collisions."
  (incf (iso-point-x (actor-velocity a)) (iso-point-x (actor-acceleration a)))
  (incf (iso-point-z (actor-velocity a)) (iso-point-z (actor-acceleration a)))
  (incf (iso-point-x (actor-position a)) (iso-point-x (actor-velocity a)))
  (incf (iso-point-z (actor-position a)) (iso-point-z (actor-velocity a)))
  (let ((axc (actor-x-collision a))
	(azc (actor-z-collision a)))
    (cond ((eql axc :wall)		; wall collision
	   (setf (iso-point-x (actor-velocity a)) 0))

	  ((not (null axc))		; actor collision
	   (setf (iso-point-x (actor-velocity a)) 0)
	   (setf (iso-point-x (actor-position a))
		 (if (< (+ (iso-point-x (actor-position a))
			   (iso-point-x (box-dimensions (actor-box a))))
			(+ (iso-point-x (actor-position axc))
			   (half (iso-point-x (box-dimensions
					       (actor-box axc))))))
		     (- (iso-point-x (actor-position axc))
			(iso-point-x (box-dimensions (actor-box a))))
		     (+ (iso-point-x (actor-position axc))
			(iso-point-x (box-dimensions (actor-box axc)))))))

	  ((eql azc :wall)		; wall collision
	   (setf (iso-point-z (actor-velocity a)) 0))

	  ((not (null azc))		; actor collision
	   (setf (iso-point-z (actor-velocity a)) 0)
	   (setf (iso-point-z (actor-position a))
		 (if (< (+ (iso-point-z (actor-position a))
			   (iso-point-z (box-dimensions (actor-box a))))
			(+ (iso-point-z (actor-position azc))
			   (half (iso-point-z (box-dimensions
					       (actor-box azc))))))
		     (- (iso-point-z (actor-position azc))
			(iso-point-z (box-dimensions (actor-box a))))
		     (+ (iso-point-z (actor-position azc))
			(iso-point-z (box-dimensions (actor-box azc))))))))))


(defun update-vertical-physics (a)
  (incf (iso-point-y (actor-velocity a)) (iso-point-y (actor-acceleration a)))
  (incf (iso-point-y (actor-position a)) (iso-point-y (actor-velocity a)))
  (let ((ayc (actor-y-collision a)))
    (cond ((null ayc)			; in the air
	   (sinkf (iso-point-y (actor-velocity a)) *air-friction*)
	   (incf (iso-point-y (actor-velocity a)) *gravity*)
	   (sinkf (iso-point-x (actor-velocity a)) *air-friction*)
	   (sinkf (iso-point-z (actor-velocity a)) *air-friction*))
	  ((eql ayc :floor)		; ground
	   ;; XXX call floor handler dynamically
	   (setf (iso-point-y (actor-velocity a)) 0)
	   (setf (iso-point-y (actor-position a)) 0)
	   (sinkf (iso-point-x (actor-velocity a)) *ground-friction*)
	   (sinkf (iso-point-z (actor-velocity a)) *ground-friction*))
	  (t				; other actor
	   ;; XXX call top handler
	   (setf (iso-point-y (actor-velocity a)) 0)
	   (setf (iso-point-y (actor-position a))
		 (- (iso-point-y (actor-position ayc))
		    (1- (iso-point-y (box-dimensions (actor-box ayc))))))
	   ;; get friction value
	   (sinkf (iso-point-x (actor-velocity a)) *ground-friction*)
	   (sinkf (iso-point-z (actor-velocity a)) *ground-friction*))
	   )))
