;;;
;;; main.lisp -- Main game loop for Equinox-ish demo.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :demon-of-the-fall)

(defvar *camera* (cons 100 140))
(defvar *camera-follow*)
(defvar *magic-exit-hack* nil)
(defvar *exit-counter* 0)

(defun first-init ()
  "Called when we first start."
  (create-display)
  (initialize-actor-data)
  (initialize-room-data))


(defun in-game-loop (starting-room)
  "Interactive game-loop on the current display, starting from the
given ROOM.  Note that the display must already have been created."
  (font-init)
  (initialize-tiles)
  (create-sprite-manager)
  (create-actor-manager)
  (wipe-events)

  (let ((fps-count (cons 0 (timer-get-ticks))))
    (load-default-font "spn.ttf" 24)
    (load-room starting-room)

    ;; spawn the player, have the camera follow it.
    (setf *camera-follow*
	  (spawn-actor-from-archetype :peter
				      (iso-point-from-list
				       (room-player-spawn *current-room*))))
    (loop
     (timer-start-frame 20)
     (event-update)
     (when (event-pressedp +ev-quit+)
       (return))

     (when (and *magic-exit-hack* (zerop *exit-counter*))
       (let ((old-y (iso-point-y (actor-position *camera-follow*))))
	 (destroy-sprite-manager)
	 (create-sprite-manager)
	 (create-actor-manager)
	 (load-room (car *magic-exit-hack*))
	 (setf *camera-follow*
	       (spawn-actor-from-archetype :peter
					   #I((* (caadr *magic-exit-hack*)
						 +tile-size+)
					      old-y
					      (* (cdadr *magic-exit-hack*)
						 +tile-size+))))
	 (setf *exit-counter* 61)
	 (setf *magic-exit-hack* nil)))

     (when (plusp *exit-counter*) (decf *exit-counter*))

     (update-all-actors 20)
     (update-camera *camera-follow*)
     (room-redraw)
     (update-all-sprites)

     (paint-osd)
     (refresh-display)

     (timer-end-frame)
     (incf (car fps-count)))

    (destroy-font)
    (destroy-sprite-manager)
    (format t "~&Frames-per-second: ~D"
	    (float (* 1000 (/ (car fps-count) (- (timer-get-ticks)
						 (cdr fps-count))))))))


(defun update-camera (actor)
  (multiple-value-bind (x y)
      (iso-project-point (actor-position actor))
    (decf x (half (display-width)))
    (decf y (half (display-height)))
    (setf (car *camera*) (- x)
	  (cdr *camera*) (- y))))
