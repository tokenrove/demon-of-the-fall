
(in-package :demon-of-the-fall)


(defclass game-state ()
  ((trigger-states)
   (game-progress)))

(defclass player ()
  ((lives :accessor lives-of)
   (avatar :accessor avatar-of)))

(defclass trigger ()
  ())
