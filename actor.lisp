
(in-package :vgdev-iso-cl)

;;;; Actors

(defclass actor ()
  ((sprite :accessor actor-sprite)
   (position :accessor actor-position)
   (velocity :accessor actor-velocity)
   (acceleration :accessor actor-acceleration)
   (box :accessor actor-box)
   (h-collision :accessor actor-h-collision)
   (v-collision :accessor actor-v-collision)
   (handler :accessor actor-handler)))

(defvar *actor-map* (make-hash-table))
(defvar *actor-id-counter* 0)

(defun create-actor-manager ()
  (setf *actor-map* (make-hash-table))
  (setf *actor-id-counter* 0))

(defun manage-actor (actor)
  (setf (gethash *actor-id-counter* *actor-map*) actor)
  (prog1 *actor-id-counter*
    (incf *actor-id-counter*)))

(defun unmanage-actor (id)
  (remhash id *actor-map*))

(defun update-all-actors ()
  "Update collisions, physics, and handlers for all actors registered
with the actor manager."
  ;; Crude but simple n^2 collision checking.
  (maphash (lambda (id-a actor-a)
	     (setf (actor-h-collision actor-a) nil)
	     (setf (actor-v-collision actor-a) nil)
	     (maphash (lambda (id-b actor-b)
			(unless (= id-a id-b)
			  (check-collision actor-a actor-b)))
		      *actor-map*)
	     (unless (actor-h-collision actor-a)
	       (check-wall-collision actor-a))
	     (unless (actor-v-collision actor-a)
	       (check-floor-collision actor-a)))
	   *actor-map*)
  (maphash (lambda (id actor)
	     (declare (ignore id))
	     (update-physics actor))
	   *actor-map*)
  (maphash (lambda (id actor)
	     ;; XXX camera
	     (update-sprite-coords (actor-sprite actor)
				   (actor-position actor))
	     (funcall (actor-handler actor) id actor))
	   *actor-map*))


(defun check-collision (a b)
  "Checks for horizontal and vertical collisions between actors a and
b.  Uses the axis-aligned bounding boxes in ACTOR-BOX."
  (when (boxes-overlap-p (box-translate (actor-box a) (actor-position a))
			 (box-translate (actor-box b) (actor-position b)))
    ;; it's a vertical collision if a is above b and either a has
    ;; positive y velocity, or b has negative y velocity.
    (cond ((and (<= (iso-point-y (actor-position a))
		    (- (iso-point-y (actor-position b))
		       (/ (iso-point-y (box-dimensions (actor-box b)))
			  2)))
		(or (plusp (iso-point-y (actor-velocity a)))
		    (minusp (iso-point-y (actor-velocity b)))))
	   (setf (actor-v-collision a) b))

	  ;; XXX should check also for same/different velocities
	  (t (setf (actor-h-collision a) b)))
    t))

(defun check-wall-collision (actor)
  "Checks whether the actor is colliding with any walls."
  (declare (ignore actor))
  nil)

(defun check-floor-collision (actor)
  "Checks whether the actor is colliding with the floor, and sets
their v-collision state accordingly."
  (when (and (>= (iso-point-y (actor-position actor)) 0) t)
;	     (>= (iso-point-y (actor-velocity actor)) 0))
    (setf (actor-v-collision actor) :floor)))


;;;; Handlers

(defun create-human-input-handler ()
  "Create a handler which updates an actor based on current input
events."
  (lambda (id player)
    (declare (ignore id))
    (when (event-pressedp :up)
      (decf (iso-point-z (actor-velocity player)) 0.3))
    (when (event-pressedp :down)
      (incf (iso-point-z (actor-velocity player)) 0.3))
    (when (event-pressedp :left)
      (decf (iso-point-x (actor-velocity player)) 0.3))
    (when (event-pressedp :right)
      (incf (iso-point-x (actor-velocity player)) 0.3))
    (when (event-pressedp :jump)
      (decf (iso-point-y (actor-velocity player)) 1)
      (decf (iso-point-y (actor-position player)) 2))))

