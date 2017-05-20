(in-package #:net-game)

(defparameter *object-pool*
  nil)

(defun pool-add (&rest objs)
  (check-type *object-pool* list)
  (pool-add-list objs))

(defun pool-add-list (objs)
  (check-type *object-pool* list)
  (setf *object-pool*
        (append objs *object-pool*)))

(defun pool-matching (pred)
  (check-type *object-pool* list)
  (remove-if-not pred *object-pool*))

(defun pool-count (pred)
  (check-type *object-pool* list)
  (count-if pred *object-pool*))

(defun pool-remove (obj)
  (check-type *object-pool* list)
  (setf *object-pool* (delete obj *object-pool*)))

(defun pool-first-match (pred)
  (check-type *object-pool* list)
  (find pred *object-pool*))
