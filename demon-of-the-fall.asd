;; -*- Lisp -*-

(defpackage #:demon-of-the-fall-system (:use #:cl #:asdf))
(in-package #:demon-of-the-fall-system)

(defsystem demon-of-the-fall
    :depends-on (:game-fetus-alpha :aequus-noctis :anaphora)
    :components
    ((:module "demon-of-the-fall"
	      :components
	      ((:file "package")
	       ;; scenario logic
	       (:file "maze" :depends-on ("package"))
	       (:file "osd" :depends-on ("package"))
	       (:file "main" :depends-on ("package" "osd"))))))

