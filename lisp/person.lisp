(in-package #:net-game)

(defclass person (identifiable named located)
  ((short-name :accessor person-nickname
               :initform ""
               :initarg :short-name
               :type string)
   (gender :accessor person-gender
           :initform nil
           :initarg :gender)
   (job :accessor person-job
        :initform nil
        :initarg :job)
   (job-name :accessor person-job-name
             :initform nil
             :initarg :job-name)
   (old-job :accessor person-old-job
            :initform nil
            :initarg :old-job)
   (old-job-name :accessor person-old-job-name
                 :initform nil
                 :initarg :old-job-name)))

(defun make-person (id name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'person :id id :name name keys))

(defmethod load-object-with-type (node (type (eql 'npc)) &rest args)
  (let ((person (apply #'make-person args)))
    (move-object person node)))

; TODO More user-friendly text here
(defmethod do-action ((type (eql 'examine)) (obj person) preps)
  (declare (ignore preps))
  (case (person-gender obj)
    (male (format t "An ordinary guy.~%"))
    (female (format t "An ordinary woman.~%"))))

(defmethod system-keys append ((obj person))
  `((person-nickname "Nickname" ,(person-nickname obj))
    (person-gender "Gender" ,(person-gender obj))
    (person-job "Occupation" ,(person-job obj))
    (person-job-name "Occupation Name" ,(person-job-name obj))
    (person-old-job "Prior Occupation" ,(person-old-job obj))
    (person-old-job-name "Prior Occupation Name" ,(person-old-job-name obj))
    (get-quest-list "Quest List" ,(get-quest-list (get-id obj)))))

; Uses *player*
(defmethod do-action ((type (eql 'talk)) (obj person) preps)
  (declare (ignore preps))
  (with-speech-vars ((speaker obj))
    (default-dialogue obj)))

(defun default-dialogue (person)
  (with-accessors ((name get-name)
                   (nickname person-nickname))
      person
    (cond
      ((not (string-equal nickname name))
       (do-speak '((one-liner ("Hi!"))))) ; TODO Update these to say something intelligent
      (t
       (do-speak '((one-liner "Hi!")))))))
