
(in-package :vgdev-iso-cl)

(defun iso-project-point (x y z)
  "Project a world coordinate (3D) point onto screen coordinates.
Returns two values, X and Y in screen coordinates."
  (let ((sx (+ (ash x -1) (ash z -1)))
	(sy (+ y (- (ash z -2) (ash x -2)))))
    (setf sx (round (+ sx (ash (display-width) -1))))
    (setf sy (round (+ sy (display-height))))
    (values sx sy)))


;;; example sprite initializer
(defvar *demo-sprite-alists*
  '((:glen ((:image "pglen1.pcx")
	    (:frames ((0 0 24 48)))
	    (:animations ((:face-left (0 . 60))))))
    (:block ((:image "pblock-1.pcx")
	     (:frames ((0 0 64 64)))
	     (:animations ((:default (0 . 60))))))))

(defun demo-loop ()
  "Do a little interactive demo-loop on the current display.  Note
that the display must already have been created."
  (font-init)
  (create-sprite-manager)
  (let ((player (make-instance 'actor))
	(floor-img (load-image "pfloor-1.pcx" t))
	(fps-count (cons 0 (sdl:get-ticks))))

    (load-default-font "Jagged Dreams.ttf" 18)

    (setf (actor-x player) 0)
    (setf (actor-y player) 0)
    (setf (actor-z player) 0)

    (setf (actor-sprite player)
	  (new-sprite-from-alist (cadr (assoc :glen *demo-sprite-alists*))))
    (add-sprite-to-list (actor-sprite player))

    (let ((block-spr (new-sprite-from-alist (cadr
					     (assoc :block
						    *demo-sprite-alists*)))))
      (update-sprite-coords block-spr 0 0 -64)
      (add-sprite-to-list block-spr))

    (use-image-palette (sprite-image (actor-sprite player)))

    (loop
     (sync-start-frame)
     (event-update)
     (when (event-pressedp :quit)
       (return))
     (update-human-input player)

     ;; Background
     ;; XXX replace with room drawing
     (fill-background 255)
     (paint-floor floor-img)

     ;; Sprites
     ;; XXX replace with sprite engine
     (update-sprite-coords (actor-sprite player)
			   (actor-x player)
			   (actor-y player)
			   (actor-z player))
     (update-all-sprites)

     ;; OSD
     (paint-string (format nil "Player: (~D, ~D, ~D)" (actor-x player)
			   (actor-y player) (actor-z player))
		   10 10 255 255 255)
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

