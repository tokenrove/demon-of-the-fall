;; -*- Lisp -*-

(in-package :asdf-user)

(defsystem demon-of-the-fall
  :description "An isometric puzzle-platformer game."
  :author "Julian Squires <julian@cipht.net>"
  :license "GPL-3; assets under CC-BY-SA"
  :depends-on (:game-fetus-alpha :aequus-noctis :anaphora :fiveam)
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :unit :demon-of-the-fall)))
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "actor" :depends-on ("package"))
     (:file "actor-handlers" :depends-on ("actor"))
     (:file "maze" :depends-on ("package"))
     (:file "osd" :depends-on ("package"))
     (:file "cue" :depends-on ("package"))
     (:file "music" :depends-on ("package"))
     (:file "room" :depends-on ("actor" "cue"))
     (:file "automap" :depends-on ("room"))
     (:file "scenario" :depends-on ("automap" "music"))
     (:file "play-session" :depends-on ("scenario"))
     (:file "main" :depends-on ("play-session" "osd"))))))
