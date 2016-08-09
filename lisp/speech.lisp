(in-package #:net-game)

(defparameter *speech* nil)

(defconstant +speech-functions+
  `((to-lower . ,(lambda (x) (string-downcase x)))
    (to-upper . ,(lambda (x) (string-upcase x)))))

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

(defun eval-speech (id nature &rest pairs &key (*speech* *speech*) &allow-other-keys)
  (let ((line (get-speech-line id nature)))
    (and line
         (apply #'eval-speech-line line pairs))))

(defun do-speak (id nature &rest pairs &key (*speech* *speech*) &allow-other-keys)
  (let ((result (apply #'eval-speech id nature pairs)))
    (unless result
      (error "Speech of form ~S with nature ~S does not exist" id nature))
    (format t "~A~%" result)))

