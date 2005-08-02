;;;
;;; main.lisp -- Main game loop for Equinox-ish demo.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :demon-of-the-fall)

(defun first-init ()
  "Called when we first start."
  (fetus:create-display)
  (fetus:event-init)
  (fetus:font-init)
  (equinox:initialize-actor-data)
  (equinox:initialize-room-data))

(defun final-shutdown ()
  (fetus:event-shutdown)
  (fetus:destroy-display))


(defvar *camera-follow*)


(defun in-game-loop (starting-room)
  "Interactive game-loop on the current display, starting from the
given ROOM.  Note that the display must already have been created."
  (equinox:initialize-tiles)
  (fetus:with-sprite-manager (sprite-manager #'equinox:isometric-sprite-cmp)
    (equinox:create-actor-manager)
    (fetus:wipe-events)
    ;; XXX yuck, package moving ugliness.
    (setf equinox:*magic-exit-hack* nil
	  equinox:*exit-counter* 0)

    (let ((fps-count (cons 0 (fetus:timer-get-ticks))))
      (fetus:with-font (font "other-data/spn.ttf" 24)
	(equinox:load-room starting-room sprite-manager)

	;; spawn the player, have the camera follow it.
	;; XXX yuck, package moving ugliness.
	(aif (equinox::room-player-spawn equinox::*current-room*)
	     (setf *camera-follow*
		   (equinox:spawn-actor-from-archetype
		    :peter (equinox::iso-point-from-list it) sprite-manager))
	     (setf *camera-follow*
		   (equinox:spawn-actor-from-archetype
		    :peter (equinox::make-iso-point) sprite-manager)))

	(loop
	   (fetus:timer-start-frame 20)
	   (fetus:event-update)
	   (when (fetus:event-pressedp fetus:+ev-quit+)
	     (return))

	   ;; XXX: ugly hack.
	   (multiple-value-bind (room point)
	       (equinox:check-room-change *camera-follow*)
	     (when room
	       (fetus:destroy-sprite-manager sprite-manager)
	       (setf sprite-manager (fetus:create-sprite-manager
				     #'equinox:isometric-sprite-cmp))
	       (equinox:create-actor-manager)
	       (equinox:load-room room sprite-manager)
	       (setf *camera-follow*
		     (equinox:spawn-actor-from-archetype :peter point
							 sprite-manager))))

	   (equinox:update-camera *camera-follow*)
	   (equinox:update-all-actors 20)
	   (equinox:room-redraw)
	   (fetus:update-all-sprites sprite-manager)

	   ;(paint-osd font)
	   (fetus:refresh-display)

	   (fetus:timer-end-frame)
	   (incf (car fps-count)))
	(format t "~&Frames-per-second: ~D"
		(float (* 1000 (/ (car fps-count) (- (fetus:timer-get-ticks)
						     (cdr fps-count))))))))))