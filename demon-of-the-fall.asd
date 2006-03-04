;; -*- Lisp -*-

(defpackage #:demon-of-the-fall-system (:use #:cl #:asdf))
(in-package #:demon-of-the-fall-system)

(defsystem demon-of-the-fall
    :depends-on (:game-fetus-alpha :aequus-noctis :anaphora)
    :components
    ((:module "src"
	      :components
	      ((:file "package")
	       ;; scenario logic
	       (:file "maze" :depends-on ("package"))
	       (:file "osd" :depends-on ("package"))
	       (:file "room" :depends-on ("package"))
	       (:file "actor" :depends-on ("package"))
	       (:file "main" :depends-on ("package" "room" "osd"))))))

