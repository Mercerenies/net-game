(in-package #:net-game)

(defparameter *knowledge-base* nil)

(defstruct (human-knowledge (:conc-name know-))
  id
  quests)

(defun delta-load-knowledge-base (data)
  (load-formatted data 'knowledge-base
                  (:new values (loop for dd in values
                                     for loaded = (load-brain dd)
                                     do (setf (gethash (know-id loaded) *knowledge-base*)
                                              loaded)))
                  (:mod values (loop for dd in values
                                     for loaded = (load-brain dd)
                                     do (merge-into (gethash (know-id loaded) *knowledge-base*)
                                                    loaded)))))

(defun load-brain (data)
  (let ((brain (make-human-knowledge :quests nil)))
    (load-formatted data 'npc-brain
                    (id (setf (know-id brain) id))
                    (:quests quests (setf (know-quests brain) quests))
                    (:meta meta))
    brain))

(defun merge-into (dest src)
  (check-type dest human-knowledge)
  (check-type src human-knowledge)
  (loop for quest in (know-quests src)
        do (push quest (know-quests dest))))

(defun load-knowledge-base (data)
  (let ((hash (make-hash-table)))
    (load-formatted data 'knowledge-base
                    ((entry) (let ((brain (load-brain entry)))
                               (setf (gethash (know-id brain) hash) brain))))
    hash))

(defun get-quest-list (id)
  (know-quests (gethash id *knowledge-base*)))
