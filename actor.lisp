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
   (box :accessor actor-box)
   (handler :accessor actor-handler)
   (contact-surface :accessor actor-contact-surface)
   (facing :accessor actor-facing))
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
      (:image "ret-data/petsheet.pcx")
      (:blit-offset (12 . 0))
      (:frames ((0 0 24 48)
		(24 0 24 48)
		(48 0 24 48)
		(72 0 24 48)
		(96 0 24 48)
		(120 0 24 48)
		(144 0 24 48)
		(168 0 24 48)
		(192 0 24 48)))
      (:animations ((:default (0 . 60))
		    (:walk-east (1 . 5)
				(2 . 5)
				(3 . 5)
				(2 . 5))
		    (:stand-east (0 . 60))
		    (:stand-north (4 . 60))
		    (:walk-north (5 . 5)
				 (6 . 5)
				 (7 . 5)
				 (6 . 5)))))
     (:box
      (0 0 0)
      (24 36 24))
     (:handler
      create-human-input-handler)
     (:contact
      player-contact-handler))
    (:push-block
     (:sprite
      (:image "ret-data/block.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 64)))
      (:animations ((:default (0 . 60)))))
     (:handler
      create-do-nothing-handler)
     (:contact
      pushable-block-handler)
     (:box
      (0 0 0)
      (64 32 64)))
    (:float-block
     (:sprite
      (:image "ret-data/bl-cushi.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 64)))
      (:animations ((:default (0 . 60)))))
     (:handler
      create-floating-block-handler)
     (:contact
      pushable-block-handler)
     (:box
      (0 0 0)
      (64 32 64)))
    (:floor-block
     (:sprite
      (:image "ret-data/fl-check.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 40)))
      (:animations ((:default (0 . 60)))))
     (:handler
      create-floating-block-handler)
     (:contact
      pushable-block-handler)
     (:box
      (0 0 0)
      (64 32 40))))
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
    (setf (actor-contact-surface actor) nil)
    (setf (actor-facing actor) :east)
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
    (setf (actor-velocity actor) (make-iso-point))
    (manage-actor actor)
    actor))



(defun update-all-actors (foo)
  "Update collisions, physics, and handlers for all actors registered
with the actor manager."
  (declare (ignore foo))
  (maphash (lambda (id actor)
	     (update-physics actor)

	     ;; ensure no penetrations.
	     (maphash (lambda (id-b actor-b)
			(unless (= id id-b)
			  (assert (not (penetrating-p actor actor-b)))))
		      *actor-map*)

	     ;; XXX update contact handlers
	     ;; XXX camera
	     (update-sprite-coords (actor-sprite actor)
				   (actor-position actor)
				   (actor-box actor))
	     (funcall (actor-handler actor) id actor))
	   *actor-map*))


;;;; Handlers

(defun create-do-nothing-handler ()
  "Create an actor handler which does nothing."
  (lambda (id actor) (declare (ignore id actor))))

(defun create-floating-block-handler ()
  "Create an actor handler which floats up and down."
  (let ((direction :up))
    (lambda (id actor)
      (declare (ignore id))
      (when (actor-contact-surface actor)
	(setf direction :up))
      (if (eql direction :up)
	  (if (< (iso-point-y (actor-position actor)) 42)
	      ;(apply-impulse actor :y -0.2)
	      (setf (iso-point-y (actor-velocity actor)) 0.2)
	      (setf direction :down))
	  (if (> (iso-point-y (actor-position actor)) 0)
	      (setf (iso-point-y (actor-velocity actor)) -0.2)
	      (setf direction :up))))))

(defun create-human-input-handler ()
  "Create a handler which updates an actor based on current input
events."
  (lambda (id player)
    (declare (ignore id))
    (when (event-pressedp :up)
      (apply-impulse player :x 0.5)
      (set-sprite-animation (actor-sprite player) :walk-north))
    (when (event-pressedp :down)
      (apply-impulse player :x -0.5))
    (when (event-pressedp :left)
      (apply-impulse player :z 0.5))
    (when (event-pressedp :right)
      (apply-impulse player :z -0.5)
      (set-sprite-animation (actor-sprite player) :walk-east))
    (when (event-pressedp :jump)
      (apply-impulse player :y 1.4))))


(defun pushable-block-handler (face impulse actor)
;;   if something is on top of us,
;;       sink its horizontal velocities by our friction,
;;       add our velocity to its velocity.
  )

(defun player-contact-handler (face impulse actor)
;;   if something is on top of us,
;;       sink its horizontal velocities by our friction,
;;       add our velocity to its velocity.
;;   if something is being pushed horizontally by us,
;;       apply impulse to it.
  )

;; (defun monster-contact-handler ()
;;    if something is on top of us,
;;        sink its horizontal velocities by our friction,
;;        add our velocity to its velocity.
;;    if we're touching the player at all,
;;        kill them.

