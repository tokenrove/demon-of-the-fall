
(in-package :vgdev-iso-cl)

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
  (create-actor-manager)

  (let ((player (make-instance 'actor))
	(block-actor (make-instance 'actor))
	(floor-img (load-image "pfloor-1.pcx" t))
	(fps-count (cons 0 (sdl:get-ticks))))

    (load-default-font "Jagged Dreams.ttf" 18)

    (setf (actor-position player) (make-iso-point))
    (setf (actor-handler player) (create-human-input-handler))
    (setf (actor-sprite player)
	  (new-sprite-from-alist (cadr (assoc :glen *demo-sprite-alists*))))
    (add-sprite-to-list (actor-sprite player))
    (setf (actor-box player)
	  (make-box :position (make-iso-point)
		    :dimensions (make-iso-point :x 24 :y 48 :z 24)))
    (setf (actor-velocity player) (make-iso-point)
	  (actor-acceleration player) (make-iso-point))
    (manage-actor player)

    (setf (actor-position block-actor) (make-iso-point :z -64))
    (setf (actor-handler block-actor) (lambda (id a) (declare (ignore id a))))
    (setf (actor-sprite block-actor)
	  (new-sprite-from-alist (cadr (assoc :block
					      *demo-sprite-alists*))))
    (add-sprite-to-list (actor-sprite block-actor))
    (setf (actor-box block-actor)
	  (make-box :position (make-iso-point)
		    :dimensions (make-iso-point :x 64 :y 32 :z 64)))
    (setf (actor-velocity block-actor) (make-iso-point)
	  (actor-acceleration block-actor) (make-iso-point))
    (manage-actor block-actor)

    (use-image-palette (sprite-image (actor-sprite player)))

    (loop
     (sync-start-frame)
     (event-update)
     (when (event-pressedp :quit)
       (return))
     (funcall (actor-handler player) 0 player)

     ;; Background
     ;; XXX replace with room drawing
     (fill-background 255)
     (paint-floor floor-img)

     ;; Actors
     (update-all-actors)
     ;; Sprites
     (update-all-sprites)

     ;; OSD
     (let* ((b (box-translate (actor-box player) (actor-position player)))
	    (p (box-position b))
	    (d (box-dimensions b)))
       (paint-string (format nil "Player: (~A,~A,~A)(~A,~A,~A) -- collision: ~A"
			     (iso-point-x p) (iso-point-y p) (iso-point-z p)
			     (iso-point-x d) (iso-point-y d) (iso-point-z d)
			     (check-collision player block-actor))
		     10 10 255 255 255))
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

