;;;
;;; main.lisp -- Main game loop for Equinox-ish demo.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;


(in-package :vgdev-iso-cl)

(defun demo-loop ()
  "Do a little interactive demo-loop on the current display.  Note
that the display must already have been created."
  (font-init)
  (create-sprite-manager)
  (create-actor-manager)

  (let ((floor-img (load-image "pfloor-1.pcx" t))
	(fps-count (cons 0 (sdl:get-ticks))))

    (load-default-font "Jagged Dreams.ttf" 18)

    ;; XXX this stuff will all go in level-loading
    (use-image-palette floor-img)
    (spawn-actor-from-archetype :glen (make-iso-point))
    (spawn-actor-from-archetype :push-block
				(make-iso-point :z -64))

    (loop
     (sync-start-frame)
     (event-update)
     (when (event-pressedp :quit)
       (return))

     ;; Background
     ;; XXX replace with room drawing
     (fill-background 255)
     (paint-floor floor-img)

     (update-all-actors)
     (update-all-sprites)

     ;; OSD
     (paint-string "Equinox-ish..." 10 10 255 255 255)
     (refresh-display)

     (sync-end-frame)
     (incf (car fps-count)))

    (destroy-font)
    (free-image floor-img)
    (destroy-sprite-manager)
    (format t "~&Frames-per-second: ~D" (float (* 1000
						  (/ (car fps-count)
						     (- (sdl:get-ticks)
							(cdr fps-count))))))))

