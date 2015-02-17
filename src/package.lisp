
(defpackage :demon-of-the-fall
  (:nicknames :demon)
  (:use :cl :anaphora)
  (:shadow #:room)
  (:import-from :equinox
                #:paint
                #:update
                #:notify
                #:width-of
		#:depth-of
		#:archetype-of
		#:+tile-size+
                #:position-of
                #:velocity-of
                #:contact-surface-of
                #:apply-impulse
		#:sinkf
		#:clampf
		#:half
                #:iso-project-point
                #:make-iso-point
		#:iso-point-x
		#:iso-point-y
                #:iso-point-z
                #:iso-point-component
                #:make-box
                #:box-of
                #:sprite-of
                #:facing-of))

(in-package :demon-of-the-fall)
(5am:def-suite unit :description "Fast-running tests")
(5am:def-suite integration :description "Big or slow non-interactive tests")
(5am:def-suite acceptance :description "Interactive tests")
(5am:in-suite unit)
