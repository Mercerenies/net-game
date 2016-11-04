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
                 :initarg :old-job-name)
   (casual-dialogue :accessor person-casual-dialogue
                    :initform ""
                    :initarg :casual-dialogue
                    :type string)))

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
  (npc-talk-menu obj))

(defun npc-talk-menu (npc)
  (let ((basic-responses `(("What's going on?" . ,(lambda () (npc-initiate-quests npc)))
                           ("Nothing, really." . ,(lambda ()))))
        (quest-responses (loop for quest in (active-quests *player*)
                               for quest-data = (get-quest-data (get-id quest))
                               for state = (gethash (quest-state quest) (quest-states quest-data))
                               append (loop for (key . value) in state
                                            do (format t "~S~%" key)
                                            when (and (listp key)
                                                      (eql (first key) 'talk-to)
                                                      (eql (second key) (get-id npc)))
                                                collect (let ((quest1 quest)
                                                              (prompt (third key)))
                                                          (cons prompt ; TODO Trigger properly, not forcing
                                                                (lambda () (do-quest-trigger quest1
                                                                             `(talk-to ,(get-id npc)
                                                                                       ,prompt)))))))))
    (apply #'speak-branch "What can I do for you?"
           (append quest-responses
                   basic-responses
                   nil))))

(defun npc-initiate-quests (person)
  (let ((quest (loop for qid in (get-quest-list (get-id person))
                     for quest = (get-quest-data qid)
                     unless (has-started-quest qid)
                         return quest)))
    (if quest
        (do-initiate-quest quest)
        (default-dialogue person))))

(defun default-dialogue (person)
  (speak-line (person-casual-dialogue person)))
