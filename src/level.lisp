
(in-package :demon-of-the-fall)


(defclass game-state ()
  ((trigger-states)
   (game-progress)))

(defclass player ()
  ((lives)
   (current-character)))

(defclass trigger ()
  ())


(defclass room (equinox:room)
  ((exits)
   (player-spawn)
   (name)))
