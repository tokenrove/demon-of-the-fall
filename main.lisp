;;;
;;; main.lisp -- Main game loop for Equinox-ish demo.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :vgdev-iso-cl)

(defun lil-demo ()
  (fill-background 255)
  (update-all-actors 20)
  (maphash #'(lambda (id actor)
	       (declare (ignore id))
	       (draw-back-of-actor-box actor))
	   *actor-map*)
  (update-all-sprites)
  (maphash #'(lambda (id actor)
	       (declare (ignore id))
	       (draw-front-of-actor-box actor))
	   *actor-map*)

  (multiple-value-bind (x y)
      (iso-project-point (actor-position (gethash 0 *actor-map*)))
    (incf x (car *camera*))
    (incf y (cdr *camera*))
    (sdl:draw-line *vbuffer* x y x y 255 10 10))

  (multiple-value-bind (x y)
      (iso-project-point (iso-point-translate
			  #I(0 36 0)
			  (actor-position (gethash 0 *actor-map*))))
    (incf x (car *camera*))
    (incf y (cdr *camera*))
    (sdl:draw-line *vbuffer* x y x y 255 10 10))


  (refresh-display))

(defvar *debug-projections* nil)
(defvar *camera* (cons 100 140))

(defun demo-loop ()
  "Do a little interactive demo-loop on the current display.  Note
that the display must already have been created."
  (font-init)
  (create-sprite-manager)
  (create-actor-manager)

  (let ((floor-img (load-image "ret-data/fl-check.pcx" t))
	(fps-count (cons 0 (sdl:get-ticks))))

    (load-default-font "Jagged Dreams.ttf" 18)

    ;; XXX this stuff will all go in level-loading
    (use-image-palette floor-img)
    (spawn-actor-from-archetype :glen #I(64 0 0))
    (spawn-actor-from-archetype :push-block #I(224 0 94))
    (spawn-actor-from-archetype :float-block #I(224 0 166))

    (loop
     (sync-start-frame)
     (event-update)
     (when (event-pressedp :quit)
       (return))

     ;; Background
     ;; XXX replace with room drawing
     (fill-background 0)
     (paint-floor floor-img)

     (update-all-actors 20)
     (update-all-sprites)

     (paint-osd)
     (refresh-display)

     (sync-end-frame)
     (incf (car fps-count)))

    (destroy-font)
    (free-image floor-img)
    (destroy-sprite-manager)
    (format t "~&Frames-per-second: ~D"
	    (float (* 1000 (/ (car fps-count) (- (sdl:get-ticks)
						 (cdr fps-count))))))))

