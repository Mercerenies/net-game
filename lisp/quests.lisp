(in-package #:net-game)

(defparameter *quests* nil)

; TODO Quests should reward the player upon completion; currently they just sort of... finish

; These are the read-only objects that are stored in *quests*
(defclass quest-details (identifiable named)
  ((nature :accessor quest-nature
           :initarg :nature
           :initform nil
           :type symbol)
   (specifics :accessor quest-specifics
              :initarg :specifics
              :initform nil
              :type list)))

; These are the mutable structures that the player modifies locally
(defclass quest (identifiable named)
  ((nature :accessor quest-nature
           :initarg :nature
           :initform nil
           :type symbol)
   (flags :accessor quest-flags
          :initarg :flags
          :initform nil
          :type list)))

(defclass fetch-quest (quest)
  ()
  (:default-initargs :nature 'fetch))

; Uses *quests*
(defun get-quest-details (qname)
  (gethash qname *quests*))

(defgeneric quest-specific-slot (details slot-name))

(defmethod quest-specific-slot ((details quest-details) slot-name)
  (getf (quest-specifics details) slot-name))

; Uses *quests*
(defun add-quest (details)
  (setf (gethash (get-id details) *quests*) details))

(defgeneric create-quest-instance (details nature))

(defmethod create-quest-instance ((details quest-details) nature)
  (declare (ignore nature))
  (make-instance 'quest
                 :id (get-id details)
                 :name (get-name details)
                 :nature (quest-nature details)))

(defmethod create-quest-instance ((details quest-details) (nature (eql 'fetch)))
  (declare (ignore nature))
  (make-instance 'fetch-quest
                 :id (get-id details)
                 :name (get-name details)))

(defun start-quest (details)
  (check-type details quest-details)
  (create-quest-instance details (quest-nature details)))

(defgeneric is-quest-completed (quest))

; Unless overridden specifically, a quest is never complete
(defmethod is-quest-completed ((quest quest))
  nil)

(defmethod is-quest-completed ((quest fetch-quest))
  (member 'delivered (quest-flags quest)))

(defgeneric load-quest (type &rest data))

(defmethod load-quest ((type (eql 'quest)) &rest data)
  (destructuring-bind (id name nature specifics) data
    (make-instance 'quest-details
                   :id id
                   :name name
                   :nature nature
                   :specifics specifics)))

(defun has-started-quest (quest-id &key ((:player *player*) *player*))
  (member quest-id (active-quests *player*) :key #'get-id))

(defun has-finished-quest (quest-id &key ((:player *player*) *player*))
  (and (has-started-quest quest-id)
       (is-quest-completed (find quest-id (active-quests *player*) :key #'get-id))))

(defgeneric introduce-quest-instance (details nature))

(defmethod introduce-quest-instance ((details quest-details) (nature (eql 'fetch)))
  (with-speech-vars ((item-name (quest-specific-slot details 'item-name))
                     (item-loc (quest-specific-slot details 'item-loc))
                     (current-quest details))
    (do-speak 'fetch-quest-start)))

(defun introduce-quest (details)
  (introduce-quest-instance details (quest-nature details)))

(defgeneric quest-status-update-instance (details nature))

(defmethod quest-status-update-instance ((details quest-details) (nature (eql 'fetch)))
  (let* ((q-item-flag (quest-specific-slot details 'item-flag))
         (matching-item (find-if (lambda (x) (check-flag q-item-flag x))
                                 (inv-items *player*))))
    (with-speech-vars ((item-name (quest-specific-slot details 'item-name))
                       (item-loc (quest-specific-slot details 'item-loc))
                       (current-quest details))
      (if matching-item
          (progn (remove-item matching-item *player*)
                 (do-speak 'fetch-quest-success))
          (do-speak 'fetch-quest-reminder)))))

(defun quest-status-update (details)
  (quest-status-update-instance details (quest-nature details)))

; Uses *quests*
(defun total-quest-count ()
  (hash-table-count *quests*))

; Uses *player*
(defun started-quests ()
  ; Returns quests which have been started, including those that are completed
  (active-quests *player*))

; Uses *player* indirectly
(defun finished-quests ()
  (remove-if (complement #'is-quest-completed)
             (started-quests)))

; Uses *player*, *quests*
(defun percent-started-quests ()
  (if (zerop (total-quest-count))
      1
      (/ (length (started-quests)) (total-quest-count))))

; Uses *player*, *quests*
(defun percent-finished-quests ()
  (if (zerop (total-quest-count))
      1
      (/ (length (finished-quests)) (total-quest-count))))
