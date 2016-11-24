(in-package #:net-game)

(defclass dialogue-state ()
  ((prompt :accessor dia-prompt
           :initarg :prompt
           :initform "")
   (answers :accessor dia-answers
            :initarg :answers
            :initform nil)))

(defun make-dialogue-state (&key (prompt "") (answers nil))
  (check-type prompt string)
  (check-type answers list "an associative list")
  (make-instance 'dialogue-state
                 :prompt prompt
                 :answers answers))

(defmethod mode-name ((state dialogue-state))
  "DIALOGUE")

(defmethod mode-text ((state dialogue-state))
  (format nil "\"~A\"~%~{~A. \"~A\"~%~}"
          (dia-prompt state)
          (loop for e in (dia-answers state)
                for i upfrom 1
                collect i
                collect (car e))))

(defmethod do-command ((state dialogue-state) arg)
  (let ((int (parse-integer arg :junk-allowed t)))
    (if (and int (typep int `(integer 1 ,(length (dia-answers state)))))
        (let ((callback (cdr (nth (1- int) (dia-answers state)))))
          (pop *state*)
          (funcall callback))
        (format t "Invalid answer.~%"))))

(defun speak-line (text)
  (check-type text string)
  (format t "\"~A\"~%" text))

(defun speak-branch (prompt &rest answers)
  (check-type answers list "an associative list")
  (format t "\"~A\"~%" prompt)
  (push (make-dialogue-state :prompt prompt
                             :answers answers)
        *state*)
  nil)

(defmacro speak-branches (prompt &rest answers)
  (let ((answer-block (loop for (ans . blck) in answers
                            collect `(cons ,ans (lambda () ,@blck)))))
    `(speak-branch ,prompt ,@answer-block)))
