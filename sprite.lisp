;;;
;;; sprite.lisp -- Simple sprite management routines.
;;;
;;; The interface is here to provide animation facilities, but they're
;;; currently unimplemented as I don't have any art to make use of
;;; them.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

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
   (priority :accessor sprite-priority))
  (:documentation "An animated display object, with a concept of
priority and screen position."))


(defun new-sprite-from-alist (alist)
  "Creates a new sprite from an alist with keys :image, :frames, and
:animations."
  (let ((sprite (make-instance 'sprite)))
    (setf (sprite-image sprite)
	  (load-image (cadr (assoc :image alist)) t))
    (setf (sprite-frames sprite) (cadr (assoc :frames alist)))
    (setf (sprite-animations sprite) (cadr (assoc :animations alist)))
    (setf (sprite-x sprite) 0 (sprite-y sprite) 42 ; XXX sane defaults?
	  (sprite-priority sprite) 0)
    sprite))

(defun update-sprite-coords (sprite position)
  "Update sprite screen coordinates from world coordinates."
  (multiple-value-bind (u v) (iso-project-point position)
    (setf (sprite-x sprite) u
	  (sprite-y sprite) v
	  ;; XXX priority formula isn't quite right
	  (sprite-priority sprite) (- (iso-point-z position)
				      (iso-point-y position)))))

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
  ;; XXX could be a lot more efficient if we cared.
  (setf *global-sprite-list* (stable-sort *global-sprite-list*
					  #'<=
					  :key #'sprite-priority))
  (loop for sprite in *global-sprite-list*
	do (draw-sprite sprite)))
