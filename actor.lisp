
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
  ;; Crude but simple.
  (maphash (lambda (id-a actor-a)
	     (setf (actor-h-collision actor-a) nil)
	     (setf (actor-v-collision actor-a) nil)
	     (maphash (lambda (id-b actor-b)
			(check-collision actor-a actor-b))
		      *actor-map*)
	     (unless (actor-h-collision actor-a)
	       (check-wall-collision actor-a))
	     (unless (actor-v-collision actor-a)
	       (check-floor-collision actor-a)))
	   *actor-map*)
;  (maphash (lambda (id actor) (update-physics actor)) *actor-map*)
  (maphash (lambda (id actor)
	     (funcall (actor-handler actor) id actor)) *actor-map*))


(defun check-collision (a b)
  "Checks for horizontal and vertical collisions between actors a and b."
  (when (boxes-overlap-p (box-translate (actor-box a) (actor-position a))
			 (box-translate (actor-box b) (actor-position b)))
    ;;; XXX should check also for same/different velocities

    ;; it's a vertical collision if a is above b and either a has
    ;; positive y velocity, or b has negative y velocity.

    (cond ((and (<= (iso-point-y (actor-position a))
		    (iso-point-y (actor-position b)))
		(or (plusp (iso-point-y (actor-velocity a)))
		    (minusp (iso-point-y (actor-velocity b)))))
	   (format t "~&vertical collision!")
	   (setf (actor-v-collision a) b))
	  (t (setf (actor-h-collision a) b)))
    t))

(defun check-wall-collision (actor)
  "Checks whether the actor is colliding with any walls."
  nil)

(defun check-floor-collision (actor)
  "Checks whether the actor is colliding with the floor, and sets
their v-collision state accordingly."
  (when (and (>= (iso-point-y (actor-position actor)) 0)
	     (>= (iso-point-y (actor-velocity actor)) 0))
    (setf (actor-v-collision actor) :floor)))


;;;; Handlers

(defun create-human-input-handler ()
  "Create a handler which updates an actor based on current input
events."
  (lambda (id player)
    (when (event-pressedp :up)
      (decf (iso-point-z (actor-position player)) 2))
    (when (event-pressedp :down)
      (incf (iso-point-z (actor-position player)) 2))
    (when (event-pressedp :left)
      (decf (iso-point-x (actor-position player)) 2))
    (when (event-pressedp :right)
      (incf (iso-point-x (actor-position player)) 2))))

