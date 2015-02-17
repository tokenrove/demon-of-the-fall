
(in-package :demon-of-the-fall)

(defclass room (equinox:room)
  ((exits :accessor exits-of)
   (player-spawn :accessor player-spawn-of)))

(defclass exit ()
  ((goes-to)
   (comes-from))
  (:documentation "A link between two ROOMs."))

(defclass spawn ()
  ((archetype)
   (initial-state)
   (predicate))
  (:documentation "A predicated birther of ACTORs."))

(defun load-room (name &key (spawn-actors-p t))
  (let ((room (make-instance 'room)))
    (equinox::load-room-int room name)
    #+(or) (when spawn-actors-p
            (dolist (actor (cdr (assoc :actors (cdr archetype))))
              (add-actor-to-room room
                                 (spawn-actor-from-archetype room
                                                             (first actor)
                                                             (iso-point-from-list (second actor))))))
    (setf (exits-of room) (cdr (assoc :exits (cdr (archetype-of room)))))
    (setf (player-spawn-of room)
	  (cadr (assoc :player-spawn (cdr (archetype-of room)))))
    room))

(defmethod paint :after ((room room) (camera equinox:camera))
  ;; draw exits
  (loop for exit in (exits-of room)
	when (consp (first exit))
	do (destructuring-bind (x . z) (first exit)
	     (when (minusp (1- x))
               (draw-exit camera exit t nil))
	     (when (minusp (1- z))
               (draw-exit camera exit nil nil))
	     (when (= x (1- (width-of room)))
               (draw-exit camera exit t t))
	     (when (= z (1- (depth-of room)))
               (draw-exit camera exit nil t)))))

;;; XXX this is still kind of gross.
(defmethod equinox:notify (actor (room room) (what (eql :border-collision)) &key at)
  (let ((x (equinox::iso-point-x at))
        (z (equinox::iso-point-z at)))
    (clampf x (1- (width-of room)) 0)
    (clampf z (1- (depth-of room)) 0)

    (loop for exit in (exits-of room)
          when (consp (first exit))
            do (destructuring-bind (exit-x . exit-z) (first exit)
                 (when (and (= x exit-x)
                            (= z exit-z)
                            (zerop (exit-counter-of actor)))
                   (setf (last-exit-taken-by actor) exit)
                   (return))))))

(defun check-room-change (actor)
  (when (plusp (exit-counter-of actor)) (decf (exit-counter-of actor)))
  (when (and (last-exit-taken-by actor) (zerop (exit-counter-of actor)))
    (setf (exit-counter-of actor) 61)
    (prog1 (last-exit-taken-by actor)
      (setf (last-exit-taken-by actor) nil))))

;; Draws triangles.  These could be more efficiently drawn as sprites,
;; but for the moment it doesn't matter too much.
(defun draw-exit (camera exit ex great)
  (multiple-value-bind (x1 y1)
      (iso-project-point #I((* equinox::+tile-size+ (+ (caar exit)
					      (if (and ex great) 1 0))) 0
			    (* equinox::+tile-size+ (+ (cdar exit)
					      (if (and (not ex) great) 1 0)))))
    (multiple-value-bind (x2 y2)
	(iso-project-point #I((* equinox::+tile-size+ (+ (caar exit)
						(if (and ex (not great)) 0 1)))
			      0
			      (* equinox::+tile-size+ (+ (cdar exit)
						(if (or ex great) 1 0)))))
      (multiple-value-bind (x3 y3)
	  (iso-project-point
	   #I((+ (* equinox::+tile-size+ (caar exit))
		 (cond ((and ex (not great)) (- (half equinox::+tile-size+)))
		       ((not ex) (half equinox::+tile-size+))
		       (t (+ equinox::+tile-size+ (half equinox::+tile-size+)))))
	      0
	      (+ (* equinox::+tile-size+ (cdar exit))
		 (cond (ex (half equinox::+tile-size+))
		       ((not great) (- (half equinox::+tile-size+)))
		       (t (+ equinox::+tile-size+ (half equinox::+tile-size+)))))))

        (let ((x (slot-value camera 'equinox::x))
              (y (slot-value camera 'equinox::y)))
          (incf x1 x)
          (incf x2 x)
          (incf x3 x)
          (incf y1 y)
          (incf y2 y)
          (incf y3 y))
	(fetus:draw-filled-triangle (list x1 x2 x3) (list y1 y2 y3) 150)
	(fetus:draw-triangle (list x1 x2 x3) (list y1 y2 y3) 64)))))

