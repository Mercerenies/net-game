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
  (format t "An ordinary guy."))

(defmethod do-action ((type (eql 'probe)) (obj person) preps)
  (declare (ignore preps))
  (format t "Person: ~S~%"
          obj))

(defmethod do-action ((type (eql 'talk)) (obj person) preps)
  (declare (ignore pres))
  (format t "\"My name is ~A.~@[ You can call me ~A.~] I am something of a ~(~A~).\"~%"
          (get-name obj)
          (and (not (string-equal (person-nickname obj) (get-name obj)))
               (person-nickname obj))
          (person-job-name obj)))
