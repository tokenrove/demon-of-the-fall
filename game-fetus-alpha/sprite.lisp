;;;
;;; sprite.lisp -- Simple sprite management routines.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :game-fetus-alpha)

;;;; General Sprites

(defun anim-frame-timing (animation)
  (car animation))
(defun anim-frame-idx (animation)
  (cdr animation))

(defclass sprite ()
  ((image :accessor sprite-image)
   (blit-offset :accessor sprite-blit-offset)
   (frames :accessor sprite-frames)
   (animations :accessor sprite-animations)
   (x :accessor sprite-x)
   (y :accessor sprite-y)
   (priority :accessor sprite-priority)
   (cur-anim :accessor sprite-cur-anim)
   (cur-frame :accessor sprite-cur-frame)
   (frame-counter :accessor sprite-frame-counter))
  (:documentation "An animated display object, with a concept of
priority and screen position."))


(defun new-sprite-from-alist (alist)
  "Creates a new sprite from an alist with keys :image, :frames, and
:animations."
  (let ((sprite (make-instance 'sprite)))
    (setf (sprite-image sprite)
	  (load-image (cadr (assoc :image alist)) t))
    (setf (sprite-blit-offset sprite) (cadr (assoc :blit-offset alist)))
    (setf (sprite-frames sprite) (cadr (assoc :frames alist)))
    (setf (sprite-animations sprite) (cadr (assoc :animations alist)))
    (setf (sprite-x sprite) 0 (sprite-y sprite) 42 ; XXX sane defaults?
	  (sprite-priority sprite) 0
	  (sprite-cur-anim sprite) (caar (sprite-animations sprite))
	  (sprite-cur-frame sprite) 0
	  (sprite-frame-counter sprite) 0)
    sprite))

(defun draw-sprite (sprite)
  "Draw a sprite's current frame and update."
  ;; get current frame
  (let ((flist (cdr (assoc (sprite-cur-anim sprite)
			   (sprite-animations sprite))))
	(u (sprite-x sprite))
	(v (sprite-y sprite)))
    (blit-image (sprite-image sprite) u v
		:src-rect (nth (car (nth (sprite-cur-frame sprite)
					 flist))
			       (sprite-frames sprite)))
    ;; update frame
    (decf (sprite-frame-counter sprite))
    (when (minusp (sprite-frame-counter sprite))
      (incf (sprite-cur-frame sprite))
      (when (>= (sprite-cur-frame sprite) (length flist))
	(setf (sprite-cur-frame sprite) 0))
      (setf (sprite-frame-counter sprite)
	    (cdr (nth (sprite-cur-frame sprite) flist))))))


(defun set-sprite-animation (sprite anim)
  ;; XXX set correct frame count, etc etc
  (unless (eql anim (sprite-cur-anim sprite))
    (setf (sprite-cur-anim sprite) anim)
    (setf (sprite-cur-frame sprite) 0)))

;;;; Sprite Manager

(defclass sprite-manager ()
  ((sprites :accessor sprite-manager-sprites)
   (sort-fn :accessor sprite-manager-sort-fn)))

(defun create-sprite-manager (sort-fn)
  (let ((manager (make-instance 'sprite-manager)))
    (setf (sprite-manager-sprites manager) nil)
    (setf (sprite-manager-sort-fn manager) sort-fn)
    manager))

(defun destroy-sprite-manager (manager)
  (dolist (sprite (sprite-manager-sprites manager))
    (awhen (sprite-image sprite)
      (free-image it))
    (setf (sprite-image sprite) nil)))

(defun add-sprite-to-manager (manager sprite)
  (push sprite (sprite-manager-sprites manager)))

(defun remove-sprite-from-manager (manager sprite)
  (setf (sprite-manager-sprites manager)
	(remove sprite (sprite-manager-sprites manager))))

(defun update-all-sprites (manager)
  ;; XXX could be a lot more efficient if we cared.
  (setf (sprite-manager-sprites manager)
	(stable-sort
	 (sprite-manager-sprites manager)
	 (sprite-manager-sort-fn manager)
	 :key #'sprite-priority))

  (dolist (sprite (sprite-manager-sprites manager))
    (draw-sprite sprite)))
