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
  (equinox:initialize-actor-data)
  (equinox:initialize-room-data))

(defun final-shutdown ()
  (fetus:event-shutdown)
  (fetus:destroy-display))


(defun in-game-loop (starting-room)
  "Interactive game-loop on the current display, starting from the
given ROOM.  Note that the display must already have been created."
  (fetus:font-init)
  (equinox:initialize-tiles)
  (fetus:with-sprite-manager (sprite-manager #'equinox:isometric-sprite-cmp)
    (equinox:create-actor-manager)
    (fetus:wipe-events)
    (setf equinox:*magic-exit-hack* nil
	  equinox:*exit-counter* 0)

    (let ((fps-count (cons 0 (fetus:timer-get-ticks))))
      (fetus:with-font (font "other-data/spn.ttf" 24)
	(equinox:load-room starting-room sprite-manager)

	;; spawn the player, have the camera follow it.
	(setf equinox:*camera-follow*
	      (equinox:spawn-actor-from-archetype :peter
						  (equinox::iso-point-from-list
						   (equinox::room-player-spawn equinox::*current-room*))
						  sprite-manager))
	(loop
	   (fetus:timer-start-frame 20)
	   (fetus:event-update)
	   (when (fetus:event-pressedp fetus:+ev-quit+)
	     (return))

	   (when (equinox:check-room-change sprite-manager)
	     (fetus:destroy-sprite-manager sprite-manager)
	     (setf sprite-manager (fetus:create-sprite-manager
				   #'equinox:isometric-sprite-cmp)))
	   
	   (equinox:update-all-actors 20)
	   (equinox:room-redraw)
	   (update-camera equinox::*camera-follow*)
	   (fetus:update-all-sprites sprite-manager)

	   (paint-osd)
	   (fetus:refresh-display)

	   (fetus:timer-end-frame)
	   (incf (car fps-count)))

	(fetus:destroy-sprite-manager sprite-manager)
	(format t "~&Frames-per-second: ~D"
		(float (* 1000 (/ (car fps-count) (- (fetus:timer-get-ticks)
						     (cdr fps-count))))))))))


(defun update-camera (actor)
  (multiple-value-bind (x y)
      (equinox::iso-project-point (equinox::actor-position actor))
    (decf x (equinox:half (fetus:display-width)))
    (decf y (equinox:half (fetus:display-height)))
    (setf (car equinox::*camera*) (- x)
	  (cdr equinox::*camera*) (- y))))
