
(defpackage #:vgdev-iso-cl-system (:use #:cl #:asdf))
(in-package #:vgdev-iso-cl-system)

(defsystem vgdev-iso-cl
    :depends-on (:uffi :sdl :sdl-img :sdl-ttf :anaphora)
    :components
    ((:file "package")
     ;; low-level
     (:file "graphics" :depends-on ("package"))
     (:file "event" :depends-on ("package"))
     (:file "timer" :depends-on ("package"))
     ;; middle-level
     (:file "math" :depends-on ("package" "graphics"))
     (:file "font" :depends-on ("package" "graphics"))
     (:file "sprite" :depends-on ("package" "graphics" "math"))
     ;; high-level logic
     (:file "actor" :depends-on ("package" "sprite" "math"))
     (:file "utilities" :depends-on ("package" "graphics" "math" "actor"))
     (:file "osd" :depends-on ("package" "graphics" "actor"))
     (:file "physics" :depends-on ("actor" "math"))
     (:file "room" :depends-on ("package" "graphics" "math"))
     (:file "main" :depends-on ("actor" "room" "graphics" "event"
					"timer" "sprite" "font" "math"))))
