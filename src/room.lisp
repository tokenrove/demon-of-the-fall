
(in-package :demon-of-the-fall)

(defclass room (equinox:room)
  ((exits :accessor exits-of)
   (player-spawn :accessor player-spawn-of)))

(defun load-room (name sprite-manager &key (spawn-actors-p t))
  (let ((room (make-instance 'room)))
    (equinox::load-room-int room name sprite-manager :spawn-actors-p spawn-actors-p)
    (setf (exits-of room) (cdr (assoc :exits (cdr (equinox::archetype-of room)))))
    (setf (player-spawn-of room)
	  (cadr (assoc :player-spawn (cdr (equinox::archetype-of room)))))
    room))

(defmethod equinox:redraw ((room room))
  (call-next-method)
  ;; draw exits
  (dolist (exit (exits-of room))
    (when (consp (first exit))
      (when (minusp (1- (caar exit)))
	(draw-exit exit t nil))
      (when (minusp (1- (cdar exit)))
	(draw-exit exit nil nil))
      (when (= (caar exit) (1- (equinox::width-of room)))
	(draw-exit exit t t))
      (when (= (cdar exit) (1- (equinox::depth-of room)))
	(draw-exit exit nil t)))))

;;; XXX this is gross.	
(defun check-exit (room x z)
  (when (minusp x) (setf x 0))
  (when (minusp z) (setf z 0))
  (when (>= x (1- (width-of room))) (setf x (1- (width-of room))))
  (when (>= z (1- (depth-of room))) (setf z (1- (depth-of room))))

  (dolist (exit (exits-of room))
    (let ((this-side (first exit)))
      (when (and (consp this-side)
		 (= x (car this-side))
		 (= z (cdr this-side)))
	(when (zerop *exit-counter*)
	  (setf *magic-exit-hack* (list (second exit) (third exit))))))))

;;; XXX this will go elsewhere

(defun check-room-change (actor)
  (when (plusp *exit-counter*) (decf *exit-counter*))
  (when (and *magic-exit-hack* (zerop *exit-counter*))
    (let* ((old-y (iso-point-y (actor-position actor)))
	   (point #I((* (caadr *magic-exit-hack*) +tile-size+)
		     old-y
		     (* (cdadr *magic-exit-hack*) +tile-size+)))
	   (room (car *magic-exit-hack*)))
      (setf *exit-counter* 61)
      (setf *magic-exit-hack* nil)
      (values room point))))

;; Draws triangles.  These could be more efficiently drawn as sprites,
;; but for the moment it doesn't matter too much.
(defun draw-exit (exit ex great)
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

	(incf x1 (car equinox::*camera*))
	(incf x2 (car equinox::*camera*))
	(incf x3 (car equinox::*camera*))
	(incf y1 (cdr equinox::*camera*))
	(incf y2 (cdr equinox::*camera*))
	(incf y3 (cdr equinox::*camera*))
	(fetus:draw-filled-triangle (list x1 x2 x3) (list y1 y2 y3) 150)
	(fetus:draw-triangle (list x1 x2 x3) (list y1 y2 y3) 64)))))

