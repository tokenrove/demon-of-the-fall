;;;
;;; sprite.lisp -- Simple sprite management routines.
;;;
;;; The interface is here to provide animation facilities, but they're
;;; currently unimplemented as I don't have any art to make use of
;;; them.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :demon-of-the-fall)

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
  ;; XXX incomplete, should parse animations
  ;; get current frame
  (let ((flist (cdr (assoc (sprite-cur-anim sprite)
			   (sprite-animations sprite))))
	(u (sprite-x sprite))
	(v (sprite-y sprite)))
    (multiple-value-bind (x y w h)
	(values-list (nth (car (nth (sprite-cur-frame sprite) flist))
			  (sprite-frames sprite)))
      (incf u (car *camera*))
      (incf v (cdr *camera*))
      (blit-image (sprite-image sprite) u v :src-rect (list x y w h)))
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

(defvar *global-sprite-list*)

(defun create-sprite-manager ()
  (setf *global-sprite-list* nil))

(defun destroy-sprite-manager ()
  #+nil(loop for sprite in *global-sprite-list*
	do (format t "~&Destroy sprite with image ~A." (sprite-image sprite))))

(defun add-sprite-to-list (sprite)
  (push sprite *global-sprite-list*))

(defun remove-sprite (sprite)
  (setf *global-sprite-list* (remove sprite *global-sprite-list*)))

(defun update-all-sprites ()
  ;; XXX could be a lot more efficient if we cared.
  (setf *global-sprite-list*
	(stable-sort
	 *global-sprite-list*
	 #'isometric-sprite-cmp
	 :key #'sprite-priority))

  (loop for sprite in *global-sprite-list*
	do (draw-sprite sprite)))
