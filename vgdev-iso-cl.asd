
(defpackage #:vgdev-iso-cl-system (:use #:cl #:asdf))
(in-package #:vgdev-iso-cl-system)

(defsystem vgdev-iso-cl
    :depends-on (:uffi :sdl :sdl-img)
    :components
    ((:file "package")
     (:file "graphics" :depends-on ("package"))
     (:file "event" :depends-on ("package"))
     (:file "timer" :depends-on ("package"))
     (:file "main" :depends-on ("graphics" "event" "timer"))))
