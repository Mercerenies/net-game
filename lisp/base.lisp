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

; TODO "Named" and "ID'd" should really be separate concepts a lot of the time
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
