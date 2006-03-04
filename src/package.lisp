
(defpackage :demon-of-the-fall
  (:nicknames :demon)
  (:use :cl :anaphora)
  (:shadow #:room)
  (:import-from :equinox
		#:width-of
		#:depth-of
		#:archetype-of
		#:+tile-size+
		#:position-of
		#:sinkf
		#:clampf
		#:half
		#:iso-project-point
		#:iso-point-x
		#:iso-point-y
		#:iso-point-z))
