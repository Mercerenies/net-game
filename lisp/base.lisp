(in-package #:net-game)

(defclass damageable ()
  ((hp :accessor hp
       :initform 1
       :initarg :hp)))

(defun check-for-death (obj)
  (check-type obj (and damageable located))
  (when (<= (hp obj) 0)
    (move-object obj nil)))

(defmethod (setf hp) :after (val (obj damageable))
  (check-for-death obj))

(defclass named ()
  ((name :accessor get-name
         :initarg :name
         :initform ""
         :type string)))

(defclass identifiable ()
  ((id :accessor get-id
       :initarg :id
       :initform nil
       :type t)))

(defclass located ()
  ((loc :accessor get-loc
        :initarg :loc
        :initform nil
        :type (or null location))))

(defclass flagged ()
  ((flags :accessor get-flags
          :initarg :flags
          :initform nil
          :type list)))

(defgeneric add-flag (flag obj))

(defmethod add-flag ((flag symbol) (obj flagged))
  (push flag (get-flags obj)))

(defgeneric check-flag (flag obj))

(defmethod check-flag (flag (obj flagged)) ; Non-symbols will not be found and will simply return nil
  (member flag (get-flags obj)))

; Returns a list of elements of the form (key-name friendly-name value)
(defgeneric system-keys (obj)
  (:method-combination append))

(defmethod system-keys append ((obj t))
  nil)

(defmethod system-keys append ((obj damageable))
  `((hp "Health" ,(* 100 (hp obj)))))

(defmethod system-keys append ((obj flagged))
  `((get-flags "Flags" ,(get-flags obj))))
