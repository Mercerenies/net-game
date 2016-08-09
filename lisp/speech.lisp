(in-package #:net-game)

(defparameter *speech* nil)

(defconstant +speech-functions+
  `((to-lower . ,(lambda (x) (string-downcase x)))
    (to-upper . ,(lambda (x) (string-upcase x)))))

(defclass dialogue-state ()
  ((plist :accessor dia-plist
          :initarg :plist
          :initform nil)
   (nature :accessor dia-nature
           :initarg :nature
           :initform 'neutral)
   (prompt :accessor dia-prompt
           :initarg :prompt
           :initform "")
   (answers :accessor dia-answers
            :initarg :answers
            :initform nil)))

(defun make-dialogue-state (&key plist nature prompt answers)
  (make-instance 'dialogue-state
                 :plist plist
                 :nature nature
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
          (apply #'do-speak new-id (dia-nature state) (dia-plist state)))
        (format t "Invalid answer.~%"))))

(defun load-speeches (&key (force nil) &aux (*read-eval* nil))
  (unless (and *speech* (not force))
    (with-open-file (file "./data/speech.txt")
      (setf *speech* (make-hash-table))
      (loop for expr = (read file nil)
            while expr
            do (setf (gethash (car expr) *speech*) (cdr expr)))))
  *speech*)

(defun get-speech-line (id nature &key (*speech* *speech*))
  (cdr (assoc nature (gethash id *speech*))))

(defun eval-speech-line (line &rest pairs &key &allow-other-keys)
  (apply #'concatenate 'string
         (loop for elem in line
               collect (cond
                         ((stringp elem) elem)
                         ((and (keywordp elem)
                               (getf pairs elem)))
                         ((and (listp elem)
                               (assoc (first elem)
                                      +speech-functions+))
                          (apply (cdr (assoc (first elem)
                                             +speech-functions+))
                                 (mapcar (lambda (x) (apply #'eval-speech-line (list x) pairs))
                                         (rest elem))))
                         (t (error "Invalid speech content ~S in ~S" elem line))))))

(defun do-speak (id nature &rest pairs &key (*speech* *speech*) &allow-other-keys)
  (let ((cmd (get-speech-line id nature)))
    (unless cmd
      (error "Speech of form ~S with nature ~S does not exist" id nature))
    (ecase (first cmd)
      (one-liner (format t "\"~A\"~%"
                         (apply #'eval-speech-line (second cmd) pairs)))
      (branch (let ((line (apply #'eval-speech-line (second cmd) pairs))
                    (answers (mapcar (lambda (x) (cons (apply #'eval-speech-line (car x) pairs) (cdr x)))
                                     (cddr cmd))))
                (format t "\"~A\"~%" line)
                (push (make-dialogue-state :plist pairs
                                           :nature nature
                                           :prompt line
                                           :answers answers) *state*))))))
