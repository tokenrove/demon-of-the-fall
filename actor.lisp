
(in-package :vgdev-iso-cl)

;;;; Actors

(defclass actor ()
  ((type :accessor actor-type)
   (sprite :accessor actor-sprite)
   (position :accessor actor-position)
   (velocity :accessor actor-velocity)
   (acceleration :accessor actor-acceleration)
   (box :accessor actor-box)
   (x-collision :accessor actor-x-collision)
   (y-collision :accessor actor-y-collision)
   (z-collision :accessor actor-z-collision)
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

(defparameter *actor-archetypes*
  '((:glen 
     (:sprite
      (:image "pglen1.pcx")
      (:frames ((0 0 24 48)))
      (:animations ((:face-left (0 . 60)))))
     (:box
      (0 0 0)
      (24 48 24))
     (:handler
      create-human-input-handler))
    (:push-block
     (:sprite 
      (:image "pblock-1.pcx")
      (:frames ((0 0 64 64)))
      (:animations ((:default (0 . 60)))))
     (:handler
      create-do-nothing-handler)
     (:box
      (0 0 0)
      (64 32 64)))))

(defun create-do-nothing-handler ()
  (lambda (id actor) (declare (ignore id actor))))

(defun spawn-actor-from-archetype (name position)
  "Creates (and returns) a new ACTOR instance, reading default member
values from *ACTOR-ARCHETYPES*."
  (let ((actor (make-instance 'actor))
	(archetype (cdr (find name *actor-archetypes* :key #'car))))
    (setf (actor-type actor) name)
    (setf (actor-position actor) position)
    (setf (actor-handler actor)
	  (funcall (cadr (assoc :handler archetype))))
    (setf (actor-sprite actor)
	  (new-sprite-from-alist (cdr (assoc :sprite archetype))))
    (add-sprite-to-list (actor-sprite actor))
    (let ((box (cdr (assoc :box archetype))))
      (setf (actor-box actor)
	    (make-box :position (make-iso-point :x (first (first box))
						:y (second (first box))
						:z (third (first box)))
		    :dimensions (make-iso-point :x (first (second box))
						:y (second (second box))
						:z (third (second box))))))
    (setf (actor-velocity actor) (make-iso-point)
	  (actor-acceleration actor) (make-iso-point))
    (manage-actor actor)
    actor))


(defun update-all-actors ()
  "Update collisions, physics, and handlers for all actors registered
with the actor manager."
  ;; Crude but simple n^2 collision checking.
  (maphash (lambda (id-a actor-a)
	     (setf (actor-x-collision actor-a) nil)
	     (setf (actor-y-collision actor-a) nil)
	     (setf (actor-z-collision actor-a) nil)
	     (unless (eql (actor-type actor-a) :push-block)
	       (maphash (lambda (id-b actor-b)
			  (unless (= id-a id-b)
			    (check-collision actor-a actor-b)))
			*actor-map*))
	     (unless (actor-x-collision actor-a)
	       (check-wall-collision actor-a))
	     (unless (actor-y-collision actor-a)
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
		;; This test is >= rather than plusp to allow an actor
		;; to lie on the surface of another.
		(or (>= (iso-point-y (actor-velocity a)) 0)
		    (minusp (iso-point-y (actor-velocity b)))))
	   (setf (actor-y-collision a) b))

	  ;; if a is to the right of b and heading left,
	  ;; or if a is to the left of b and heading right,
	  ;; it's an x collision.
	  ((or (and (>= (iso-point-x (actor-position a))
			(+ (iso-point-x (actor-position b))
			   (/ (iso-point-x (box-dimensions (actor-box b)))
			      2)))
		    (or (minusp (iso-point-x (actor-velocity a)))
			(plusp (iso-point-x (actor-velocity b)))))
	       (and (<= (iso-point-x (actor-position a))
			(+ (iso-point-x (actor-position b))
			   (/ (iso-point-x (box-dimensions (actor-box b)))
			      2)))
		    (or (plusp (iso-point-x (actor-velocity a)))
			(minusp (iso-point-x (actor-velocity b))))))
	     (setf (actor-x-collision a) b))

	   ((or (and (>= (iso-point-z (actor-position a))
			(+ (iso-point-z (actor-position b))
			   (/ (iso-point-z (box-dimensions (actor-box b)))
			      2)))
		    (or (minusp (iso-point-z (actor-velocity a)))
			(plusp (iso-point-z (actor-velocity b)))))
	       (and (<= (iso-point-z (actor-position a))
			(+ (iso-point-z (actor-position b))
			   (/ (iso-point-z (box-dimensions (actor-box b)))
			      2)))
		    (or (plusp (iso-point-z (actor-velocity a)))
			(minusp (iso-point-z (actor-velocity b))))))
	    (setf (actor-z-collision a) b)))
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
    (setf (actor-y-collision actor) :floor)))


;;;; Handlers

(defun create-human-input-handler ()
  "Create a handler which updates an actor based on current input
events."
  (lambda (id player)
    (declare (ignore id))
    (when (event-pressedp :up)
      (decf (iso-point-z (actor-velocity player)) 0.5))
    (when (event-pressedp :down)
      (incf (iso-point-z (actor-velocity player)) 0.5))
    (when (event-pressedp :left)
      (decf (iso-point-x (actor-velocity player)) 0.5))
    (when (event-pressedp :right)
      (incf (iso-point-x (actor-velocity player)) 0.5))
    (when (event-pressedp :jump)
      (decf (iso-point-y (actor-velocity player)) 1.4)
      (decf (iso-point-y (actor-position player)) 2))))

