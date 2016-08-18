(in-package #:net-game)

(defparameter *speech* nil)

(defparameter *speech-variables*
  nil)

(defconstant +speech-functions+
  `((to-lower . ,(lambda (x) (string-downcase x)))
    (to-upper . ,(lambda (x) (string-upcase x)))))

(defconstant +speech-commands+
  `((one-liner . ,(lambda (stmt)
                          (format t "\"~A\"~%" (eval-speech-line stmt))))
    (branch . ,(lambda (stmt &rest answers)
                       (let ((line (eval-speech-line stmt))
                             (answers (mapcar (lambda (x) (cons (eval-speech-line (car x)) (cdr x)))
                                              (cddr cmd))))
                         (format t "\"~A\"~%" line)
                         (push (make-dialogue-state :plist *speech-variables*
                                                    :prompt line
                                                    :answers answers)
                               *state*))))))

(defclass dialogue-state ()
  ((plist :accessor dia-plist
          :initarg :plist
          :initform nil)
   (prompt :accessor dia-prompt
           :initarg :prompt
           :initform "")
   (answers :accessor dia-answers
            :initarg :answers
            :initform nil)))

(defun make-dialogue-state (&key plist prompt answers)
  (make-instance 'dialogue-state
                 :plist plist
                 :prompt prompt
                 :answers answers))

(defmethod mode-name ((state dialogue-state))
  "DIALOGUE")

(defmethod mode-text ((state dialogue-state))
  (format nil "\"~A\"~%~{~A. ~A~%~}"
          (dia-prompt state)
          (loop for e in (dia-answers state)
                for i upfrom 1
                collect i
                collect (car e))))

(defmethod do-command ((state dialogue-state) arg)
  (let ((int (parse-integer arg :junk-allowed t)))
    (if (and int (typep int `(integer 1 ,(length (dia-answers state)))))
        (let ((new-id (cdr (nth (1- int) (dia-answers state)))))
          (pop *state*)
          (with-speech-var-pack (dia-plist state)
            (do-speak new-id)))
        (format t "Invalid answer.~%"))))

(defun load-speeches (&key (force nil) &aux (*read-eval* nil))
  (unless (and *speech* (not force))
    (with-open-file (file "./data/speech.txt")
      (setf *speech* (make-hash-table))
      (loop for expr = (read file nil)
            while expr
            do (setf (gethash (car expr) *speech*) (cdr expr)))))
  *speech*)

(defun get-speech-line (id &key (*speech* *speech*))
  (gethash id *speech*))

(defun eval-speech-line (line)
  (apply #'concatenate 'string
         (loop for elem in line
               collect (cond
                         ((stringp elem) elem)
                         ((and (keywordp elem)
                               (getf *speech-variables* elem)))
                         ((and (listp elem)
                               (assoc (first elem)
                                      +speech-functions+))
                          (apply (cdr (assoc (first elem)
                                             +speech-functions+))
                                 (mapcar (lambda (x) (eval-speech-line (list x)))
                                         (rest elem))))
                         (t (error "Invalid speech content ~S in ~S" elem line))))))

(defun do-speak (id &key (*speech* *speech*))
  (loop with cmds = (get-speech-line id)
        with final-cmd = nil ; TODO Commands line (branch ...) should not allow things to come after them
        for cmd in cmds
        unless cmd
            do (error "Speech of form ~S with nature NIL does not exist" id) ; TODO Speech "natures"
        do (apply (cdr (assoc (first cmd)
                              +speech-commands+))
                  (rest cmd))))

(defmacro with-speech-var-pack (vars &body body)
  `(let ((*speech-variables* (append ,vars *speech-variables*)))
     ,@body))

(defmacro with-speech-vars ((&rest vars) &body body)
  (let ((qvars (loop for vv in vars
                     collect `(quote ,(first vv))
                     collect (second vv))))
    `(with-speech-var-pack (list ,@qvars)
       ,@body)))
