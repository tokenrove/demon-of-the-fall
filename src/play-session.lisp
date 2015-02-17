(in-package :demon-of-the-fall)

(defclass play-session ()
  ((scenario :accessor scenario-of :initarg :scenario)

   (current-room :accessor current-room-of :initarg :current-room)
   (visited-rooms)
   (visited-exits)
   (cues-triggered)

   (current-character)
   (characters-unlocked)
   (inventory)

   (configuration))
  (:documentation "The dynamic component of an active game.  Its
  serialized contents unambiguously represent a saved game that can be
  restored.  Compare SCENARIO."))
