(in-package :demon-of-the-fall)

(defclass scenario ()
  ((version)
   (rooms)
   (archetypes)
   (music)
   (automap)
   (starting-room)
   (starting-music))
  (:documentation "The static aspects of a game; its serialization is
  a fixed level or world.  Compare PLAY-SESSION."))

(defun load-scenario (path))
(defmethod save ((scenario scenario) path))
