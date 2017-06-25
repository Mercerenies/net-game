(in-package #:net-game)

(defparameter *knowledge-base* nil)

(defconstant +brain-types+
  '(npc-brain city-brain))

; ///// TODO Abstract the knowledge base database to make counting cities more efficient?
;            (We need to be able to count cities to get the triggers for actor requests)

;(defgeneric know-id (knowledge))

(defstruct (human-knowledge (:conc-name hmn-know-))
  id
  quests)

(defstruct (city-knowledge (:conc-name cit-know-))
  id)

(defgeneric know-id (k))

(defmethod know-id ((k human-knowledge))
  (hmn-know-id k))

(defmethod know-id ((k city-knowledge))
  (cit-know-id k))

(defun know-quests (k)
  (check-type k human-knowledge)
  (hmn-know-quests k))

(defun (setf know-quests) (q k)
  (check-type k human-knowledge)
  (setf (hmn-know-quests k) q))

(defgeneric (setf know-id) (i k))

(defmethod (setf know-id) (i (k human-knowledge))
  (setf (hmn-know-id k) i))

(defmethod (setf know-id) (i (k city-knowledge))
  (setf (cit-know-id k) i))

(defmethod delta-load-object ((header (eql 'knowledge-base)) data)
  (load-formatted data 'knowledge-base
                  (:new values (loop for dd in values
                                     for loaded = (whitelisted-load #'load-object +brain-types+ dd)
                                     do (setf (gethash (know-id loaded) *knowledge-base*)
                                              loaded)))
                  (:mod values (loop for dd in values
                                     for loaded = (whitelisted-load #'load-object +brain-types+ dd)
                                     do (merge-into (gethash (know-id loaded) *knowledge-base*)
                                                    loaded)))))

(defmethod load-object ((header (eql 'npc-brain)) data)
  (let ((brain (make-human-knowledge :quests nil)))
    (load-formatted data 'npc-brain
                    (id (setf (know-id brain) id))
                    (:quests quests (setf (know-quests brain) quests))
                    (:meta meta))
    brain))

(defmethod load-object ((head (eql 'city-brain)) data)
  (let ((brain (make-city-knowledge)))
    (load-formatted data 'city-brain
                    (id (setf (know-id brain) id)))
    brain))

(defgeneric merge-into (dest src))

(defmethod merge-into ((dest human-knowledge) (src human-knowledge))
  (loop for quest in (know-quests src)
        do (push quest (know-quests dest))))

(defmethod merge-into ((dest city-knowledge) (src city-knowledge))
  nil) ; There's nothing to do here right now, but there probably will be in the future

(defmethod load-object ((header (eql 'knowledge-base)) data)
  (let ((hash (make-hash-table)))
    (load-formatted data 'knowledge-base
                    ((entry) (let ((brain (whitelisted-load #'load-object +brain-types+ entry)))
                               (setf (gethash (know-id brain) hash) brain))))
    hash))

(defun get-quest-list (id)
  (know-quests (gethash id *knowledge-base*)))
