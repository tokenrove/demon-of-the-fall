
(in-package :vgdev-iso-cl)

(defparameter *air-friction* 0.1)
(defparameter *ground-friction* 0.25)
(defparameter *gravity* 1.5)

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
			   (/ (iso-point-x (box-dimensions (actor-box axc)))
			      2)))
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
			   (/ (iso-point-z (box-dimensions (actor-box azc)))
			      2)))
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
	   (incf (iso-point-y (actor-velocity a)) *gravity*))
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
