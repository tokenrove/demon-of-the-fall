;;;
;;; main.lisp -- Main game loop for Equinox-ish demo.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;


(in-package :vgdev-iso-cl)

(defun debugging-line-draw (point-1 point-2 origin)
  (multiple-value-bind (x1 y1)
      (iso-project-point (iso-point-translate point-1 origin))
    (multiple-value-bind (x2 y2)
	(iso-project-point (iso-point-translate point-2 origin))
      (sdl:draw-line *vbuffer* x1 y1 x2 y2 255 255 255))))

(defun lil-demo ()
  (fill-background 255)
  (update-all-actors)
  (maphash #'(lambda (id actor)
	       (declare (ignore id))
	       (draw-back-of-actor-box actor))
	   *actor-map*)
  (update-all-sprites)
  (maphash #'(lambda (id actor)
	       (declare (ignore id))
	       (draw-front-of-actor-box actor))
	   *actor-map*)
  (refresh-display))

(defun draw-back-of-actor-box (actor)
  (let* ((pos (copy-iso-point (actor-position actor)))
	 (box (box-dimensions (actor-box actor)))
	 (x (iso-point-x box))
	 (y (- (iso-point-y box)))
	 (z (iso-point-z box)))
    ;; back
    (debugging-line-draw #i(x y z) #i(x y 0) pos)
    (debugging-line-draw #i(x y 0) #i(x 0 0) pos)
    (debugging-line-draw #i(0 y 0) #i(x y 0) pos)
    (debugging-line-draw #i(0 0 0) #i(x 0 0) pos)
    (debugging-line-draw #i(x 0 z) #i(x 0 0) pos)))

(defun draw-front-of-actor-box (actor)
  (let* ((pos (copy-iso-point (actor-position actor)))
	 (box (box-dimensions (actor-box actor)))
	 (x (iso-point-x box))
	 (y (- (iso-point-y box)))
	 (z (iso-point-z box)))
    ;; front
    (debugging-line-draw #i(0 y z) #i(0 0 z) pos)
    (debugging-line-draw #i(x y z) #i(0 y z) pos)
    (debugging-line-draw #i(x 0 z) #i(0 0 z) pos)
    (debugging-line-draw #i(x y z) #i(x 0 z) pos)
    (debugging-line-draw #i(0 y 0) #i(0 y z) pos)
    (debugging-line-draw #i(0 y 0) #i(0 0 0) pos)
    (debugging-line-draw #i(0 0 0) #i(0 0 z) pos)))

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
    (spawn-actor-from-archetype :glen #I(0 0 0))
    (spawn-actor-from-archetype :push-block #I(224 0 94))

    (loop
     (sync-start-frame)
     (event-update)
     (when (event-pressedp :quit)
       (return))

     ;; Background
     ;; XXX replace with room drawing
     (fill-background 255)
     (paint-floor floor-img)

     (update-all-actors 20)
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

