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
                 :initarg :old-job-name)
   (quest-list :accessor quest-list
               :initform nil
               :initarg :quest-list)))

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

; Uses *player*
(defmethod do-action ((type (eql 'talk)) (obj person) preps)
  (declare (ignore preps))
  (let* ((next-quest-id (find-if (complement #'has-finished-quest)
                                 (quest-list obj)))
         (next-quest (and next-quest-id (get-quest-details next-quest-id))))
    (with-speech-vars ((speaker obj))
      (cond
        ((and next-quest
              (has-started-quest next-quest-id))
         (quest-status-update next-quest))
        (next-quest
         (introduce-quest next-quest))
        (t (default-dialogue obj))))))

(defun default-dialogue (person)
  (with-accessors ((name get-name)
                   (nickname person-nickname))
      person
    (cond
      ((not (string-equal nickname name))
       (do-speak 'basic-nicknamed-intro))
      (t
       (do-speak 'basic-intro)))))
