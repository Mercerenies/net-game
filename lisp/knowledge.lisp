(in-package #:net-game)

(defparameter *knowledge-base* nil)

(defconstant +brain-types+
  '(npc-brain city-brain))

; TODO Abstract the knowledge base database to make counting cities more efficient?
;      (We need to be able to count cities to get the triggers for actor requests)

; TODO Make 'probe' able to do more detailed stuff so we can access e.g. motives

(defclass knowledge-base ()
  ((hash-table :reader knowledge-hash-table
               :initarg :hash
               :initform (make-hash-table)
               :type hash-table)
   (type-table :reader knowledge-type-table
               :initarg :types
               :initform (make-hash-table)
               :type hash-table)))

(defun make-knowledge-base ()
  (make-instance 'knowledge-base))

(defgeneric knowledge-get (know key))

(defmethod knowledge-get ((know knowledge-base) key)
    (gethash key (knowledge-hash-table know)))

(defgeneric (setf knowledge-get) (new-value know key))

(defmethod (setf knowledge-get) (new-value (know knowledge-base) key)
  (with-accessors ((hash knowledge-hash-table) (types knowledge-type-table)) know
    (multiple-value-bind (old-value old-value-p) (gethash key hash)
      (let ((old-type (class-of old-value))
            (new-type (class-of new-value)))
        (when old-value-p
          (decf (gethash old-type types 0)))
        (incf (gethash new-type types 0))
        (setf (gethash key hash) new-value)))))

(defstruct (human-knowledge (:conc-name hmn-know-))
  id
  quests
  motives)

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

(defun know-motives (k)
  (check-type k human-knowledge)
  (hmn-know-motives k))

(defun (setf know-motives) (q k)
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
                                     do (setf (knowledge-get *knowledge-base* (know-id loaded))
                                              loaded)))
                  (:mod values (loop for dd in values
                                     for loaded = (whitelisted-load #'load-object +brain-types+ dd)
                                     do (merge-into (knowledge-get *knowledge-base* (know-id loaded))
                                                    loaded)))))

(defmethod load-object ((header (eql 'motives)) data)
  (load-formatted data 'motives
                  (motives (return-from load-object
                             (loop for cell = motives then (cddr cell)
                                   while cell
                                   collect (cons (first cell) (second cell)))))))

(defmethod load-object ((header (eql 'npc-brain)) data)
  (let ((brain (make-human-knowledge :quests nil)))
    (load-formatted data 'npc-brain
                    (id (setf (know-id brain) id))
                    (:quests quests (setf (know-quests brain) quests))
                    (:motives motives (setf (hmn-know-motives brain) (load-object 'motives motives)))
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
  (let ((hash (make-knowledge-base)))
    (load-formatted data 'knowledge-base
                    ((entry) (let ((brain (whitelisted-load #'load-object +brain-types+ entry)))
                               (setf (knowledge-get hash (know-id brain)) brain))))
    hash))

(defun get-quest-list (id)
  (know-quests (knowledge-get *knowledge-base* id)))
