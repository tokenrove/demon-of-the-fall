
(in-package :demon-of-the-fall)

(defclass level ()
  ((rooms)
   (tiles)
   (archetypes)
   (magic)))

(defclass iso-room (equinox:iso-room)
  ((exits)
   (player-spawn)
   (name)))
