;; -*- Lisp -*-

(defpackage #:demon-of-the-fall-system (:use #:cl #:asdf))
(in-package #:demon-of-the-fall-system)

(defsystem demon-of-the-fall
    :depends-on (#-clisp :uffi :anaphora)
    :components
    ((:file "package")
     ;; low-level
     #-clisp(:file "uffi" :depends-on ("package"))
     (:file "graphics" :depends-on ("package" "uffi"))
     (:file "event" :depends-on ("package" "uffi"))
     ;; middle-level
     (:file "math" :depends-on ("package" "graphics"))
     (:file "font" :depends-on ("package" "graphics" "uffi"))
     (:file "sprite" :depends-on ("package" "graphics" "math"))
     ;; high-level logic
     (:file "actor" :depends-on ("package" "sprite" "math" "event"))
     (:file "utilities" :depends-on ("package" "graphics" "math" "actor"))
     (:file "osd" :depends-on ("package" "graphics" "actor"))
     (:file "physics" :depends-on ("actor" "math"))
     (:file "room" :depends-on ("package" "graphics" "math"))
     (:file "maze" :depends-on ("package"))
     (:file "room-editor" :depends-on ("room"))
     ;; scenario logic
     (:file "main" :depends-on ("actor" "room" "graphics" "event"
					"sprite" "font" "math"))))

