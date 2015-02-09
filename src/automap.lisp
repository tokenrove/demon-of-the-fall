(in-package :demon-of-the-fall)

;;; Beware of shadowing.
(defclass automap ()
  ((image)
   (regions))
  (:documentation "The map image that gets filled in as a player
  explores a SCENARIO.  Keeps track of the mapping between abstract
  ROOMs/EXITs and polygonal regions on the map image."))

(defmethod region<-room ((r room) (m automap)))
(defmethod region<-exit ((e exit) (m automap)))
