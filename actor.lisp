;;; actor.lisp -- Actor management code for Equinox-ish demo.
;;;
;;; Defines the actor class, deals with global actor list, actor
;;; handlers, et cetera.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;


(in-package :vgdev-iso-cl)

;;;; Actors

;; XXX add documentation for these slots.
(defclass actor ()
  ((type :reader actor-type)
   (sprite :accessor actor-sprite)
   (position :accessor actor-position)
   (velocity :accessor actor-velocity)
   (acceleration :accessor actor-acceleration)
   (box :accessor actor-box)
   (x-collision :accessor actor-x-collision)
   (y-collision :accessor actor-y-collision)
   (z-collision :accessor actor-z-collision)
   (handler :accessor actor-handler))
  (:documentation "An ACTOR is an object that exists at the game-logic
level persistently.  Actors have handlers that are called at each time
slice in the game, handlers that are called in response to collisions,
and physical properties."))

(defvar *actor-map* (make-hash-table)
  "Global hash of ID => ACTOR containing each actor ``alive'' in the
game world.")
(defvar *actor-id-counter* 0
  "Actor unique ID counter, should always contain an ID which is not
currently in use by any live actors.")

(defun create-actor-manager ()
  "function CREATE-ACTOR-MANAGER

Initialize the global actor manager.  Note that this doesn't check
whether it has previously been initialized."
  (setf *actor-map* (make-hash-table))
  (setf *actor-id-counter* 0))

(defun manage-actor (actor)
  "function MANAGE-ACTOR actor => id

Register actor with actor manager."
  (setf (gethash *actor-id-counter* *actor-map*) actor)
  (prog1 *actor-id-counter*
    (incf *actor-id-counter*)))

(defun unmanage-actor (id)
  "function UNMANAGE-ACTOR id => boolean

Remove the actor specified by id from the actor manager."
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
      (64 32 64))))
  "The actor archetypes table, which defines the default values for
many parameters of an actor.")

(defun spawn-actor-from-archetype (name position)
  "function SPAWN-ACTOR-FROM-ARCHETYPE name position => actor

Creates (and returns) a new ACTOR instance, reading default member
values from *ACTOR-ARCHETYPES*."
  (let ((actor (make-instance 'actor))
	(archetype (cdr (find name *actor-archetypes* :key #'car))))
    (setf (slot-value actor 'type) name)
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


(defun update-actor-collisions (id actor)
  ;; Crude but simple n^2 collision checking.
  (setf (actor-x-collision actor) nil)
  (setf (actor-y-collision actor) nil)
  (setf (actor-z-collision actor) nil)
  (unless (eql (actor-type actor) :push-block)
    (maphash (lambda (id-b actor-b)
	       (unless (= id id-b)
		 (check-collision actor actor-b)))
	     *actor-map*))
  (unless (actor-x-collision actor)
    (check-wall-collision actor))
  (unless (actor-y-collision actor)
    (check-floor-collision actor)))

(defun update-all-actors ()
  "Update collisions, physics, and handlers for all actors registered
with the actor manager."
  (maphash (lambda (id actor)
	     (update-actor-collisions id actor)
	     (update-physics actor)
	     ;; XXX camera
	     (update-sprite-coords (actor-sprite actor)
				   (actor-position actor))
	     (funcall (actor-handler actor) id actor))
	   *actor-map*))


;;; XXX good god this needs refactoring
(defun check-collision (a b)
  "Checks for horizontal and vertical collisions between actors a and
b.  Uses the axis-aligned bounding boxes in ACTOR-BOX."
  (when (boxes-overlap-p (box-translate (actor-box a) (actor-position a))
			 (box-translate (actor-box b) (actor-position b)))
    ;; it's a vertical collision if a is above b and either a has
    ;; positive y velocity, or b has negative y velocity.
    (cond ((and (<= (iso-point-y (actor-position a))
		    (- (iso-point-y (actor-position b))
		       (half (iso-point-y (box-dimensions (actor-box b))))))
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
			   (half (iso-point-x (box-dimensions
					       (actor-box b))))))
		    (or (minusp (iso-point-x (actor-velocity a)))
			(plusp (iso-point-x (actor-velocity b)))))
	       (and (<= (iso-point-x (actor-position a))
			(+ (iso-point-x (actor-position b))
			   (half (iso-point-x (box-dimensions
					       (actor-box b))))))
		    (or (plusp (iso-point-x (actor-velocity a)))
			(minusp (iso-point-x (actor-velocity b))))))
	     (setf (actor-x-collision a) b))

	   ((or (and (>= (iso-point-z (actor-position a))
			(+ (iso-point-z (actor-position b))
			   (half (iso-point-z (box-dimensions
					       (actor-box b))))))
		    (or (minusp (iso-point-z (actor-velocity a)))
			(plusp (iso-point-z (actor-velocity b)))))
	       (and (<= (iso-point-z (actor-position a))
			(+ (iso-point-z (actor-position b))
			   (half (iso-point-z (box-dimensions
					       (actor-box b))))))
		    (or (plusp (iso-point-z (actor-velocity a)))
			(minusp (iso-point-z (actor-velocity b))))))
	    (setf (actor-z-collision a) b)))
    t))


;;; XXX nop
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

(defun create-do-nothing-handler ()
  "Create an actor handler which does nothing."
  (lambda (id actor) (declare (ignore id actor))))

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

