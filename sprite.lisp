
(in-package :vgdev-iso-cl)

;;;; General Sprites

(defun anim-frame-timing (animation)
  (car animation))
(defun anim-frame-idx (animation)
  (cdr animation))

(defclass sprite ()
  ((image :accessor sprite-image)
   (frames :accessor sprite-frames)
   (animations :accessor sprite-animations)
   (x :accessor sprite-x)
   (y :accessor sprite-y)
   (priority :accessor sprite-priority)))

(defun load-sprite (stream)
  "Loads sprite data from STREAM."
  (unwind-protect
       (let* ((sprite (make-instance 'sprite)))
	 (setf (sprite-image sprite)
	       (load-image (read-line stream)))
	 ;; read frames
	 ;; read animations
	 sprite)))

(defun new-sprite-from-alist (alist)
  "Creates a new sprite from an alist with keys :image, :frames, and
:animations."
  (let ((sprite (make-instance 'sprite)))
    (setf (sprite-image sprite)
	  (load-image (cadr (assoc :image alist)) t))
    (setf (sprite-frames sprite) (cadr (assoc :frames alist)))
    (setf (sprite-animations sprite) (cadr (assoc :animations alist)))
    (setf (sprite-x sprite) 0 (sprite-y sprite) 42 ; XXX
	  (sprite-priority sprite) 0)
    sprite))

(defun update-sprite-coords (sprite position)
  "Update sprite screen coordinates from world coordinates."
  (multiple-value-bind (u v) (iso-project-point position)
    (setf (sprite-x sprite) u
	  (sprite-y sprite) v
	  (sprite-priority sprite) (iso-point-z position))))

(defun draw-sprite (sprite)
  "Draw a sprite's current frame and update."
  ;; XXX incomplete, should parse animations
  (blit-image (sprite-image sprite) nil (sprite-x sprite)
	      (sprite-y sprite)))

;;;; Sprite Manager

(defvar *global-sprite-list*)

(defun create-sprite-manager ()
  (setf *global-sprite-list* nil))

(defun destroy-sprite-manager ()
  #+nil(loop for sprite in *global-sprite-list*
	do (format t "~&Destroy sprite with image ~A." (sprite-image sprite))))

(defun add-sprite-to-list (sprite)
  (push sprite *global-sprite-list*))

(defun update-all-sprites ()
  ;; XXX in priority order, draw sprites
  (setf *global-sprite-list* (stable-sort *global-sprite-list*
					  #'<=
					  :key #'sprite-priority))
  (loop for sprite in *global-sprite-list*
	do (draw-sprite sprite)))
