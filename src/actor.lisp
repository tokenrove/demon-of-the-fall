
(in-package :demon-of-the-fall)

(defclass actor (equinox::actor)
  ((exit-counter :accessor exit-counter-of :initform 0)
   (last-exit-taken :accessor last-exit-taken-by :initform nil)))

(defun spawn-actor (name position sprite-manager)
  ;; XXX package moving ugliness
  (let* ((archetype (or (cdr (find name equinox::*actor-archetypes* :key #'car))
			(error "archetype ~A not found" name)))
	 (actor (make-instance 'actor :type name)))
    (equinox::initialize-actor-from-archetype actor position sprite-manager
					      archetype)
    actor))

