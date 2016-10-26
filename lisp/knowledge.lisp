(in-package #:net-game)

(defparameter *knowledge-base* nil)

(defun load-brain (data)
  (unless (eq (car data) 'npc-brain) (error "Flawed data - npc-brain"))
  (cdr data))

(defun load-knowledge-base (data)
  (loop with hash = (make-hash-table)
        for dd = data then (cddr dd)
        for key = (first dd)
        for value = (second dd)
        while dd
        do (setf (gethash key hash) (load-brain value))
        finally (return hash)))

(defun get-quest-list (id)
  (gethash id *knowledge-base*))
