
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

(in-package :demon-of-the-fall)
(5am:def-suite unit :description "Fast-running tests")
(5am:def-suite integration :description "Big or slow non-interactive tests")
(5am:def-suite acceptance :description "Interactive tests")
(5am:in-suite unit)
