;;;
;;; maze.lisp -- Maze generation code.
;;;
;;; Simple maze generation code using depth-first search and random
;;; neighbors.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :demon-of-the-fall)

(defparameter *maze-array* #(#(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

#+nil(defparameter *maze-array* #(#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
			     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

(defun maze-point-visitedp (A x y)
  "function MAZE-POINT-VISITEDP array x y => boolean

Returns true if point (x,y) in array has been visited, is a wall, or
is unreachable.  Returns false is the point can be visited safely."
  (not (and (<= 0 x (1- (array-dimension (aref A 0) 0)))
	    (<= 0 y (1- (array-dimension A 0)))
	    (= (aref (aref A y) x) 0))))

(defun print-maze ()
  "function PRINT-MAZE

Just dumps the maze in a quick ASCII representation to
*standard-output*."
  (loop for y across *maze-array*
	do (progn
	     (loop for x across y
		   do (princ (if (= x 1) #\x #\Space)))
	     (format t "~%"))))

;; initial position = 0,0 (or whatever)
;; 0 is an unvisited node, 1 is a wall, 2 is a visited node.

(defun visitable-neighbors (x y)
  "function VISITABLE-NEIGHBORS x y => list

Returns a list of visitable neighboring points in *maze-array* from
point (x,y)."
  (mapcan (lambda (p)
	    (let ((nx (+ x (* 2 (car p))))
		  (ny (+ y (* 2 (cdr p)))))
	      (unless (maze-point-visitedp *maze-array* nx ny)
		(list (cons nx ny)))))
	  '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))))

(defun generate-maze (x y)
  "function GENERATE-MAZE x y => *maze-array*

Generates a new maze in *maze-array*, with the entrance at point
(x,y)."
  (setf (aref (aref *maze-array* y) x) 2)
  (do ((list #1=(visitable-neighbors x y) #1#)
       (next nil))
      ((null list))
    (setf next (nth (random (length list)) list))
    (let ((dx (+ x (/ (- (car next) x) 2)))
	  (dy (+ y (/ (- (cdr next) y) 2))))
      (setf (aref (aref *maze-array* dy) dx) 0))
    (generate-maze (car next) (cdr next)))
  *maze-array*)
