(in-package #:net-game)

(defparameter *quests* nil)

; These are the read-only objects that are stored in *quests*
(defclass quest-details (named)
  ((nature :accessor quest-nature
           :initarg :nature
           :initform nil
           :type symbol)
   (specifics :accessor quest-specifics
              :initarg :specifics
              :initform nil
              :type list)))

; These are the mutable structures that the player modifies locally
(defclass quest (named)
  ((nature :accessor quest-nature
           :initarg :nature
           :initform nil
           :type symbol)
   (flags :accessor quest-flags
          :initarg :flags
          :initform nil
          :type list)))

; Uses *quests*
(defun get-quest-details (qname)
  (gethash qname *quests*))

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

(defun start-quest (details)
  (check-type details quest-details)
  (create-quest-instance details (quest-nature details)))

(defgeneric is-quest-completed (quest))

; Unless overridden specifically, a quest is never complete
(defmethod is-quest-completed ((quest quest))
  nil)

(defgeneric load-quest (type &rest data))

(defmethod load-quest ((type (eql 'quest)) &rest data)
  (destructuring-bind (id name nature specifics) data
    (make-instance 'quest-details
                   :id id
                   :name name
                   :nature nature
                   :specifics specifics)))
