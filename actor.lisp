
(in-package :vgdev-iso-cl)

;;;; Actors

(defclass actor ()
  ((sprite :accessor actor-sprite)
   (x :accessor actor-x)
   (y :accessor actor-y)
   (z :accessor actor-z)))


;;;; Handlers

(defun update-human-input (player)
  "Update physics and state of an actor based on current input events."
  (when (event-pressedp :up)
    (decf (actor-z player) 2))
  (when (event-pressedp :down)
    (incf (actor-z player) 2))
  (when (event-pressedp :left)
    (decf (actor-x player) 2))
  (when (event-pressedp :right)
    (incf (actor-x player) 2)))
