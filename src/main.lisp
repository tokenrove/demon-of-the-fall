;;;
;;; main.lisp -- Main game loop for Equinox-ish demo.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :demon-of-the-fall)

(defvar *camera-follow*)

(defmacro with-everything (&body body)
  `(fetus:with-display (:scale 2)
     (fetus/os:with-directory-of-system (:demon-of-the-fall)
       (initialize-actor-data)
       (equinox:initialize-room-data)
       ,@body)))

(defun in-game-loop (play-session)
  "Interactive game-loop on the current display, starting from the
given ROOM.  Note that the display must already have been created."
  (equinox:initialize-tiles)
  (fetus:wipe-events)

  (let ((fps-count (cons 0 (fetus:timer-get-ticks))))
    (fetus:with-font (font "other-data/spn.ttf" 24)
      (let ((room (load-room (current-room-of play-session)))
            (camera (equinox:make-camera (fetus:display-width) (fetus:display-height))))
        ;; spawn the player, have the camera follow it.
        ;; XXX yuck, package moving ugliness.
        (setf *camera-follow*
              (spawn-actor
               room
               :peter (aif (player-spawn-of room)
                           (equinox::iso-point-from-list it)
                           (equinox::make-iso-point))))
        (equinox:follow camera *camera-follow*)

        (loop
          (fetus:timer-start-frame 20)

          (let ((time-elapsed 20))
            (fetus:clear-display)
            (fetus:event-update)
            (when (fetus:event-pressedp :quit)
              (return))

            ;; XXX: ugly hack.
            (awhen (check-room-change *camera-follow*)
              (return it))

            (equinox:update camera room time-elapsed)
            (equinox:update room play-session time-elapsed)
            (equinox:paint room camera)

            ;;(paint-osd font)
            (fetus:present-display))

          (fetus:timer-end-frame)
          (incf (car fps-count)))
        (format t "~&Frames-per-second: ~D"
                (float (* 1000 (/ (car fps-count) (- (fetus:timer-get-ticks)
                                                     (cdr fps-count))))))))))
