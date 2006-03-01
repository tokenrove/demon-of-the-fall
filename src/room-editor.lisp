
(in-package :equinox)

;;; XXX should cause main editor to note unsaved changes.
(defun edit-exits-dialog ()
  (do ((cursor 0)
       (max-row (length (demon::exits-of *current-room*))))
      (nil)
    (fetus:draw-filled-rectangle 8 8 (- (fetus:display-width) 16)
				 (- (fetus:display-height) 32)
				 128) ;(gfx-map-rgb 128 128 128)
    (draw-rectangle 8 8 (- (display-width) 16)
		    (- (display-height) 32) 32) ;(gfx-map-rgb 32 32 32)
    (draw-filled-rectangle 11 (+ 10 (* cursor 22)) (- (display-width) 19) 20
			   240) ;(gfx-map-rgb 240 100 50)

    (do ((i 0 (1+ i))
	 (list (demon::exits-of *current-room*) (cdr list)))
	((null list))
      (paint-string
       *default-font*
       (format nil "~A ~A ~A" (first (car list))
	       (second (car list)) (third (car list)))
       12
       (+ 10 (* i 22)) 255 255 255))
    (paint-string *default-font*
		  "Hit N to create a new exit, D to delete an exit."
		  12 170 240 200 200)
    (paint-string *default-font*
		  "Hit 1, 2, or 3 to edit parts of an exit."
		  12 190 240 200 200)
    (refresh-display)

    (let ((event (get-key-event)))
      ;; mode events; please excuse the silly hardcoded SDL keysyms.
      (cond ((= event (char-code #\q))
	     ;; ensure demon::exits-of and *room-set* are in sync.
	     (aif (assoc :exits (cdr (archetype-of *current-room*)))
		  (setf (cdr it) (demon::exits-of *current-room*))
		  (push (append (list :exits) (demon::exits-of *current-room*))
			(cdr (archetype-of *current-room*))))
	     (return))
	    ((= event 273) (when (plusp cursor) (decf cursor)))
	    ((= event 274) (if (< cursor (1- max-row))
			       (incf cursor)
			       (setf cursor 0)))
	    ((= event (char-code #\1))
	     (setf (car (first (nth cursor (demon::exits-of *current-room*))))
		   (prompt-for-integer *default-font* "X: ")
		   (cdr (first (nth cursor (demon::exits-of *current-room*))))
		   (prompt-for-integer *default-font* "Z: ")))
	    ((= event (char-code #\3))
	     (setf (car (third (nth cursor (demon::exits-of *current-room*))))
		   (prompt-for-integer *default-font* "X: ")
		   (cdr (third (nth cursor (demon::exits-of *current-room*))))
		   (prompt-for-integer *default-font* "Z: ")))
	    ((= event (char-code #\2))
	     (setf (second (nth cursor (demon::exits-of *current-room*)))
		   (change-rooms-dialog
		    (second (nth cursor (demon::exits-of *current-room*))))))
	    ((= event (char-code #\d))
	     (unless (<= max-row 0)
	       (setf (demon::exits-of *current-room*)
		     (delete (nth cursor (demon::exits-of *current-room*))
			     (demon::exits-of *current-room*)))
	       (decf max-row)))
	    ((= event (char-code #\n))
	     (push (list (cons 0 0) (caar *room-set*) (cons 0 0))
		   (demon::exits-of *current-room*))
	     (incf max-row))))))

