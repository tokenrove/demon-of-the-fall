
(in-package :demon-of-the-fall)

(defclass actor (equinox::actor)
  ((exit-counter :accessor exit-counter-of :initform 0)
   (last-exit-taken :accessor last-exit-taken-by :initform nil)
   (health)
   (inventory)
   (keys)))

(defun spawn-actor (name position sprite-manager)
  ;; XXX package moving ugliness
  (let* ((archetype (or (cdr (find name equinox::*actor-archetypes* :key #'car))
			(error "archetype ~A not found" name)))
	 (actor (make-instance 'actor :type name)))
    (equinox::initialize-actor-from-archetype actor position sprite-manager
					      archetype)
    actor))


;;; make apple handler
;;; if actor has health, add to it, and destroy ourselves.

(defun make-gate-handler ()
  (let ((state 'closed))
    (lambda (id actor)
      ;; state closed, do nothing
      ;; state rising, rise til we reach the open height
      ;; state open, just fight against gravity

      ;; when state open, move up til we reach opened-height
      (when (and (eql state 'open) (iso-point-y (position-of actor)))
	(apply-impulse :y -0.3))	;XXX gate speed constant
      (warn "Doesn't do anything yet."))))

;;; key-based gate collision:
;;; if the actor has a key in their possession,
;;;     spend it, and open us.
(defun make-keyed-gate-collision-handler (key)
  (lambda (id actor)
    ;; set state
    ))

;;; trigger-based gate collision
(defun make-gate-trigger (trigger))