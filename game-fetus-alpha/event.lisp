;;; event.lisp -- Event handler code.
;;;
;;; Very basic SDL event handling, mapping some keys to symbols.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :game-fetus-alpha)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +ev-quit+ 0)
  (defconstant +ev-up+ 1)
  (defconstant +ev-down+ 2)
  (defconstant +ev-left+ 3)
  (defconstant +ev-right+ 4)
  (defconstant +ev-button-a+ 5)
  (defconstant +ev-button-b+ 6))

;; XXX it would be nice if this table used keysym names, rather than
;; hardcoded constants.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((hash (make-hash-table)))
    (dolist (x  '((27 . #.+ev-quit+)	; ESC -> :quit
		  (113 . #.+ev-quit+)	; q -> :quit
		  (273 . #.+ev-up+)	; up -> :up
		  (274 . #.+ev-down+)	; down -> :down
		  (276 . #.+ev-left+)	; left -> :left
		  (275 . #.+ev-right+)	; right -> :right
		  (122 . #.+ev-button-a+) ; z -> :jump
		  (120 . #.+ev-button-b+))) ; x -> :button-b
      (setf (gethash (car x) hash) (cdr x)))

    (defparameter *xlate-symbol->map-idx* hash
      "Translates an SDL KeySym value to a symbolic *event-map* key.")))

(defparameter *last-ev-code* 8)

(defvar *event-map* (make-sequence 'simple-bit-vector *last-ev-code*)
  "Hash which tracks whether or not a button is being pressed.")


;;; XXX any point having a with-event-handling macro?  I think this is
;;; probably real first-init stuff.
(defun event-init ()
  (ll-event-init))

(defun event-shutdown ()
  (ll-event-shutdown))


(defun wipe-events ()
  (setf *event-map* (bit-xor *event-map* *event-map*)))

(defun event-type (event)
  (if event
      #-openmcl(get-slot-value event 'll-event 'type)
      #+openmcl(get-slot-value event ll-event type)
      (ll-event-type)))

(defun event-axis (event)
  (if event
      #-openmcl(get-slot-value event 'll-event 'axis)
      #+openmcl(get-slot-value event ll-event axis)
      (ll-event-axis)))

(defun event-value (event)
  (if event
      #-openmcl(get-slot-value event 'll-event 'value)
      #+openmcl(get-slot-value event ll-event value)
      (ll-event-value)))


(defun event-update ()
  "function EVENT-UPDATE

Refresh the state of *EVENT-MAP*."
  (do* ((rv #1=(ll-poll-event-stub) #1#)
	(event nil))
       ((= rv 0))
    (cond ((= (event-type event) +ll-event-key-down+)
	   (awhen (gethash (event-value event) *xlate-symbol->map-idx*)
	     (setf (bit *event-map* it) 1)))
	  ((= (event-type event) +ll-event-key-up+)
	   (awhen (gethash (event-value event) *xlate-symbol->map-idx*)
	     (setf (bit *event-map* it) 0)))
	  ((= (event-type event) +ll-event-joy-move+)
	   (case (event-axis event)
	     (0 (cond ((plusp (event-value event))
		       (setf (bit *event-map* +ev-right+) 1
			     (bit *event-map* +ev-left+) 0))
		      ((minusp (event-value event))
		       (setf (bit *event-map* +ev-right+) 0
			     (bit *event-map* +ev-left+) 1))
		      (t
		       (setf (bit *event-map* +ev-right+) 0
			     (bit *event-map* +ev-left+) 0))))
	     (1 (cond ((plusp (event-value event))
		       (setf (bit *event-map* +ev-down+) 1
			     (bit *event-map* +ev-up+) 0))
		      ((minusp (event-value event))
		       (setf (bit *event-map* +ev-down+) 0
			     (bit *event-map* +ev-up+) 1))
		      (t
		       (setf (bit *event-map* +ev-down+) 0
			     (bit *event-map* +ev-up+) 0))))))
	    ((= (event-type event) +ll-event-joy-button-down+)
	     (awhen (case (event-value event)
		      (0 +ev-button-a+)
		      (1 +ev-button-b+))
	       (setf (bit *event-map* it) 1)))
	    ((= (event-type event) +ll-event-joy-button-up+)
	     (awhen (case (event-value event)
		      (0 +ev-button-a+)
		      (1 +ev-button-b+))
	       (setf (bit *event-map* it) 0))))))


(defun event-pressedp (ev)
  "Returns true if the given button is pressed."
  (= (bit *event-map* ev) 1))


(defun get-key-event ()
  (with-foreign-object (event 'll-event)
    (do ((rv #1=(ll-wait-event event) #1#))
	((= rv 0))
      (let ((type (event-type event)))
	(cond ((= type +ll-event-key-down+)
	       (return (event-value event))))))))
