
(in-package :vgdev-iso-cl)

(defclass actor ()
  ((sprite :accessor actor-sprite)
   (x :accessor actor-x)
   (y :accessor actor-y)
   (z :accessor actor-z)))

(defun update-human-input (player)
  "Update physics and state of an actor based on current input events."
  (when (event-pressedp :up)
    (decf (actor-z player) 2))
  (when (event-pressedp :down)
    (incf (actor-z player) 2))
  (when (event-pressedp :left)
    (decf (actor-x player) 2))
  (when (event-pressedp :right)
    (incf (actor-x player) 2)))


(defun draw-iso-axis ()
  "Draw isometric axis aligned with world coordinates."
  ;; Y (easy!)
  (sdl:draw-line *vbuffer* (/ 320 2) 0 (/ 320 2) 240 255 0 0)
  ;; X
  (sdl:draw-line *vbuffer* (- (/ 320 2) 120) 120 (/ 320 2) 240 0 255 0)
  ;; Z
  (sdl:draw-line *vbuffer* (+ (/ 320 2) 120) 120 (/ 320 2) 240 0 0 255))


(defun iso-project-point (x y z)
  (let ((sx (+ (/ x 2) (/ z 2)))
	(sy (+ y (- (/ z 4) (/ x 4)))))
    (setf sx (round (+ sx (/ 320 2))))
    (setf sy (round (+ sy 240)))
    (values sx sy)))


(defvar *floor-description*
  #(#(0 0 1 1 1 0 0)
    #(0 0 1 1 1 0 0)
    #(1 1 1 1 1 1 1)
    #(1 1 1 1 1 1 1)
    #(0 0 1 1 1 0 0)
    #(0 0 1 1 1 0 0)))

(defun paint-floor (floor-img)
  "Paint floor tiles according to *floor-description*."
  (let ((h-extent (array-dimension (aref *floor-description* 0) 0))
	(v-extent (array-dimension *floor-description* 0)))
    (do ((y (1- v-extent) (1- y)))
	((< y 0))
      (do ((x (1- h-extent) (1- x)))
	  ((< x 0))
	;; lowest tile is x=0, y=1- v-extent
	;; first painted tile is x=1- h-extent, y=0, with world coords
	;;    (64*x, 0, -64*y)
	(let ((tile (aref (aref *floor-description* y) x)))
	  (when (= tile 1)
	    (multiple-value-bind (u v)
		(iso-project-point (* 64 x) 0 (* -64 y))
	      (blit-image floor-img nil u v))))))))

(defun demo-loop ()
  "Do a little interactive demo-loop on the current display.  Note
that the display must already have been created."
  (let ((player (make-instance 'actor))
	(floor-img (load-image "pfloor-1.pcx" t))
	(block-img (load-image "pblock-1.pcx" t))
	(fps-count (cons 0 (sdl:get-ticks))))

    (setf (actor-sprite player) (load-image "pglen1.pcx" t))
    (setf (actor-x player) 0)
    (setf (actor-y player) 0)
    (setf (actor-z player) 0)

    (use-image-palette (actor-sprite player))

    (loop
     (sync-start-frame)
     (event-update)
     (when (event-pressedp :quit)
       (return))
     (update-human-input player)
     
     ;; XXX replace with room drawing
     (fill-background 255)
;     (draw-iso-axis)
     (paint-floor floor-img)
     (multiple-value-bind (x y) (iso-project-point 0 0 -64)
       (blit-image block-img nil x y))

     ;; XXX replace with sprite engine
     (multiple-value-bind (x y) (iso-project-point (actor-x player)
						   (actor-y player)
						   (actor-z player))
       (blit-image (actor-sprite player) nil x y))
     (refresh-display)

     (sync-end-frame)
     (incf (car fps-count)))

    (format t "~&Frames-per-second: ~D" (float (* 1000
						  (/ (car fps-count)
						     (- (sdl:get-ticks)
							(cdr fps-count))))))))

