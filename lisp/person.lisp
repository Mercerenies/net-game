(in-package #:net-game)

(defclass person (named located)
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

(defmethod load-object-with-type (node (type (eql 'npc)) &rest args)
  (let ((person (apply #'make-instance 'person :name args)))
    (move-object person node)))

; TODO This
(defmethod do-action ((type (eql 'examine)) (obj person) preps)
  (declare (ignore preps))
  (case (person-gender obj)
    (male (format t "An ordinary guy.~%"))
    (female (format t "An ordinary woman.~%"))))

(defmethod do-action ((type (eql 'probe)) (obj person) preps)
  (declare (ignore preps))
  (format t "Person: ~S~%"
          obj))

(defmethod do-action ((type (eql 'talk)) (obj person) preps)
  (declare (ignore preps))
  (if (string-equal (person-nickname obj) (get-name obj))
      (do-speak 'basic-intro 'neutral
                :my-name (get-name obj)
                :my-occu (person-job-name obj))
      (do-speak 'basic-nicknamed-intro 'neutral
                :my-name (get-name obj)
                :my-nickname (person-nickname obj)
                :my-occu (person-job-name obj))))
