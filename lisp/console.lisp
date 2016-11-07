(in-package #:net-game)

(defclass console-state ()
  ((prior :accessor console-prior
          :initform nil)
   (error-term :accessor console-error
               :initform nil)
   (is-set :accessor console-is-set
           :initform nil)))

(defmethod (setf console-prior) :after (val (obj console-state))
  (declare (ignore val))
  (setf (console-is-set obj) t))

(defun make-console-state ()
  (make-instance 'console-state))

(defmethod mode-name ((state console-state))
  "CONSOLE")

(defmethod mode-text ((state console-state))
  (with-accessors ((is-set console-is-set) (prior console-prior) (err console-error)) state
    (format nil "~A~&"
            (cond
              (err)
              (is-set (format nil "~S" prior))
              (t "Enter Lisp code to evaluate:")))))

(defmethod do-command ((state console-state) arg)
  (handler-case
      (setf (console-prior state)
            (eval (read-from-string arg)))
    (t (c) (setf (console-error state) (format nil "~A" c)))
    (:no-error (&rest vals)
      (setf (console-error state) nil)
      vals)))

(defmethod do-action ((act (eql 'console)) obj preps)
  (declare (ignore obj preps))
  (push (make-console-state) *state*))
