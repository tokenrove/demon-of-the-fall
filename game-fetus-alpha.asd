;; -*- Lisp -*-

(defpackage #:game-fetus-alpha-system (:use #:cl #:asdf))
(in-package #:game-fetus-alpha-system)

(defsystem game-fetus-alpha
    :depends-on (#-clisp :uffi :anaphora)
    :components
    ((:module "game-fetus-alpha"
	      :components
	      ((:file "package")
	       ;; low-level
	       #-clisp(:file "uffi" :depends-on ("package"))
	       (:file "graphics" :depends-on ("package" "uffi"))
	       (:file "event" :depends-on ("package" "uffi"))
	       ;; middle-level
	       (:file "font" :depends-on ("package" "graphics" "uffi"))
	       (:file "sprite" :depends-on ("package" "graphics"))))))
