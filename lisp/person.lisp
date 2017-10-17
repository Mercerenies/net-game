(in-package #:net-game)

(defclass person (identifiable named located loaded)
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

(defun person-jobs (npc)
  (remove nil (list (person-job npc) (person-old-job npc))))

;; TODO More user-friendly text here
(defmethod do-action ((type (eql 'examine)) (obj person) preps)
  (declare (ignore preps))
  (case (person-gender obj)
    (male (format t "An ordinary guy.~%"))
    (female (format t "An ordinary woman.~%"))))

(defmethod object-nature ((obj person))
    'person)

(defmethod system-keys append ((obj person))
  `((person-nickname "Nickname" ,(person-nickname obj))
    (person-gender "Gender" ,(person-gender obj))
    (person-job "Occupation" ,(person-job obj))
    (person-job-name "Occupation Name" ,(person-job-name obj))
    (person-old-job "Prior Occupation" ,(person-old-job obj))
    (person-old-job-name "Prior Occupation Name" ,(person-old-job-name obj))
    (get-quest-list "Quest List" ,(get-quest-list (get-id obj)))))

(defmethod do-action ((type (eql 'talk)) (obj person) preps)
  (declare (ignore preps))
  (check-type *player* player)
  (npc-talk obj))

(defmethod entity-turn ((obj person))
  (check-type *knowledge-base* knowledge-base)
  (check-type *player* player)
  (let ((brain (knowledge-get *knowledge-base* (get-id obj)))
        (counts (npc-quest-counts obj)))
    ;; A quest should not be generated if the questgiver has a quest
    ;; which has not been completed
    (when (and (zerop (getf counts :incomplete))
               (zerop (getf counts :unaccepted)))
      (generate-and-integrate-quest obj))))

(defun npc-quest-counts (npc)
  (check-type *knowledge-base* knowledge-base)
  (check-type *player* player)
  (loop with brain = (knowledge-get *knowledge-base* (get-id npc))
        with result = (list :unaccepted 0 :incomplete 0 :complete 0)
        for qid in (know-quests brain)
        for active = (find qid (active-quests *player*))
        when active
          when (is-quest-completed active)
            do (incf (getf result :complete))
          else
            do (incf (getf result :incomplete))
        else
          do (incf (getf result :unaccepted))
        finally (return result)))

(defun npc-talk (npc)
  (let ((urgent `(talk-to! ,(get-id npc))))
    (if (has-trigger urgent)
        (do-first-trigger urgent)
        (npc-talk-menu npc))))

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
                                                          (cons prompt
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
