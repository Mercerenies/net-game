(in-package #:net-game)

(defparameter *knowledge-base* nil)

(defun delta-load-knowledge-base (data)
  (unless (eq (car data) 'knowledge-base) (error "Flawed data - knowledge-base"))
  (loop for arg = (cdr data) then (cddr arg)
        for key = (first arg)
        for value = (second arg)
        while arg
        do (case key
             (:new (loop for dd = value then (cddr dd)
                         while dd
                         do (setf (gethash (first dd) *knowledge-base*) (load-brain (second dd)))))
             (:mod (loop for dd = value then (cddr dd) ; TODO Test this and make sure it reintegrates
                         while dd
                         do (setf (gethash (car dd) *knowledge-base*)
                                  (append (cdr dd) (gethash (car dd) *knowledge-base*))))))))

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