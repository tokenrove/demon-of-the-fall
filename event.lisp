;;; event.lisp -- Event handler code.
;;;
;;; Very basic SDL event handling, mapping some keys to symbols.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :vgdev-iso-cl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +ev-quit+ 0)
  (defconstant +ev-up+ 1)
  (defconstant +ev-down+ 2)
  (defconstant +ev-left+ 3)
  (defconstant +ev-right+ 4)
  (defconstant +ev-jump+ 5))

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
		  (122 . #.+ev-jump+)))	; z -> :jump
      (setf (gethash (car x) hash) (cdr x)))

    (defparameter *xlate-symbol->map-idx* hash
      "Translates an SDL KeySym value to a symbolic *event-map* key.")))

(defparameter *last-ev-code* 8)

(defvar *event-map* (make-sequence 'simple-bit-vector *last-ev-code*)
  "Hash which tracks whether or not a button is being pressed.")

(defun wipe-events ()
  (setf *event-map* (bit-xor *event-map* *event-map*)))

(defun event-type (event)
  #-openmcl(get-slot-value event 'll-event 'type)
  #+openmcl(get-slot-value event ll-event type))

(defun event-keysym (event)
  #-openmcl(get-slot-value event 'll-event 'keysym)
  #+openmcl(get-slot-value event ll-event keysym))

(defun event-update ()
  "function EVENT-UPDATE

Refresh the state of *EVENT-MAP*."
  (with-foreign-object (event 'll-event)
    (do* ((rv #1=(ll-poll-event event) #1#)
	  (type #2=(event-type event) #2#))
	 ((= rv 0))
      (cond ((= type +ll-event-key-down+)
	     (awhen (gethash (event-keysym event)
			     *xlate-symbol->map-idx*)
		    (setf (bit *event-map* it) 1)))
	    ((= type +ll-event-key-up+)
	     (awhen (gethash (event-keysym event)
			     *xlate-symbol->map-idx*)
		    (setf (bit *event-map* it) 0)))))))

(defun event-pressedp (ev)
  "Returns true if the given button is pressed."
  (= (bit *event-map* ev) 1))
