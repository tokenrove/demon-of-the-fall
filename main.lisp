
(in-package :vgdev-iso-cl)

(defun demo-loop ()
  "Do a little interactive demo-loop on the current display.  Note
that the display must already have been created."
  (font-init)
  (create-sprite-manager)
  (create-actor-manager)

  (let ((player (spawn-actor-from-archetype :glen (make-iso-point)))
	(floor-img (load-image "pfloor-1.pcx" t))
	(fps-count (cons 0 (sdl:get-ticks))))

    (load-default-font "Jagged Dreams.ttf" 18)
    (use-image-palette (sprite-image (actor-sprite player)))
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

     ;; Actors
     (update-all-actors)
     ;; Sprites
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

