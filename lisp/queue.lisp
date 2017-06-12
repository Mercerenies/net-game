(in-package #:net-game)

(defstruct (queue (:constructor make-queue-impl (list)))
  (list nil))

(defun make-queue (list)
  (let* ((list-1 (copy-list list))
         (end (last list-1)))
    (setf (cdr end) list-1)
    (make-queue-impl end)))

(defun queue-push (queue arg)
  (if (queue-empty-p queue)
      (let ((temp (cons arg nil)))
        (setf (cdr temp) temp)
        (setf (queue-list queue) temp))
      (with-accessors ((list queue-list))
          queue
        (setf (cdr list) (cons arg (cdr list)))
        (setf list (cdr list))))
  arg)

(defun queue-pop (queue)
  (if (queue-empty-p queue)
      nil
      (with-accessors ((list queue-list))
          queue
        (prog1 (cadr list)
          (if (eq (cdr list) (cddr list))
              (setf list nil)
              (setf (cdr list) (cddr list)))))))

(defun queue-empty-p (queue)
  (null (queue-list queue)))

(defmethod print-object ((obj queue) stream)
  (let ((*print-circle* t))
    (call-next-method)))
