;; -*- Lisp -*-

(defpackage #:vgdev-iso-cl-system (:use #:cl #:asdf))
(in-package #:vgdev-iso-cl-system)

(defsystem vgdev-iso-cl
    :depends-on (:uffi :anaphora :osicat)
    :components
    ((:file "package")
     ;; low-level
     (:file "ffi" :depends-on ("package"))
     (:file "graphics" :depends-on ("package" "ffi"))
     (:file "event" :depends-on ("package" "ffi"))
     ;; middle-level
     (:file "math" :depends-on ("package" "graphics"))
     (:file "font" :depends-on ("package" "graphics"))
     (:file "sprite" :depends-on ("package" "graphics" "math"))
     ;; high-level logic
     (:file "actor" :depends-on ("package" "sprite" "math" "event"))
     (:file "utilities" :depends-on ("package" "graphics" "math" "actor"))
     (:file "osd" :depends-on ("package" "graphics" "actor"))
     (:file "physics" :depends-on ("actor" "math"))
     (:file "room" :depends-on ("package" "graphics" "math"))
     (:file "main" :depends-on ("actor" "room" "graphics" "event"
					"sprite" "font" "math"))
     (:file "maze" :depends-on ("package"))
     (:file "room-editor" :depends-on ("main"))))
