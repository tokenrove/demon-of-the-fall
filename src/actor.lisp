(in-package :demon-of-the-fall)

(defclass actor (equinox:actor)
  ((exit-counter :accessor exit-counter-of :initform 0)
   (last-exit-taken :accessor last-exit-taken-by :initform nil)
   (health)
   (inventory)
   (keys))
  (:documentation
   "DEMON:ACTOR extends EQUINOX:ACTOR to add spawn information."))

(defvar *actor-archetypes* nil
  "The actor archetypes table, which defines the default values for
many parameters of an actor.")

(defun spawn-actor (room name position)
  ;; XXX package moving ugliness
  (let* ((archetype (or (cdr (find name *actor-archetypes* :key #'car))
                        (error "archetype ~A not found" name))))
    (aprog1 (make-instance (intern (symbol-name name)))
      (initialize-actor-from-archetype it position archetype)
      (equinox:add-actor-to-room room it))))

(defun initialize-actor-data (&optional (archetypes-file "archetypes.sexp"))
  (let ((*read-eval* nil))
    (with-open-file (stream archetypes-file)
      (setf *actor-archetypes* (read stream)))))

(defun spawn-actor-from-archetype (room name position)
  "function SPAWN-ACTOR-FROM-ARCHETYPE room name position => actor

Creates (and returns) a new ACTOR instance, reading default member
values from *ACTOR-ARCHETYPES*."
  (let* ((archetype (or (cdr (find name *actor-archetypes* :key #'car))
                        (error "archetype ~A not found" name)))
         (box (destructuring-bind ((x y z) (w h d))
                  (cdr (assoc :box archetype))
                (make-box :position (make-iso-point :x x :y y :z z)
                          :dimensions (make-iso-point :x w :y h :z d)))))
    (aprog1
        (make-instance (intern (symbol-name name))
                       :position position
                       :sprite (fetus:new-sprite-from-alist
                                (cdr (assoc :sprite archetype)))
                       :box box)
      (equinox:add-actor-to-room room it))))

(defun initialize-actor-from-archetype (actor position archetype)
  (let* ((box (destructuring-bind ((x y z) (w h d))
                  (cdr (assoc :box archetype))
                (make-box :position (make-iso-point :x x :y y :z z)
                          :dimensions (make-iso-point :x w :y h :z d)))))
    (setf (position-of actor) position
          (sprite-of actor) (fetus:new-sprite-from-alist
                             (cdr (assoc :sprite archetype)))
          (box-of actor) box)
    actor))
