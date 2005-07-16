
(in-package :demon-of-the-fall)


(defclass game-state ()
  ((trigger-states)
   (game-progress)))

(defclass player ()
  ((lives)
   (current-character)))

(defclass trigger ()
  ())


(defclass iso-room (equinox:iso-room)
  ((exits)
   (player-spawn)
   (name)))
