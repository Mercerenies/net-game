
(defclass inventory ()
  ((items :accessor inv-items
          :initarg :items
          :initform nil
          :type list)))

(defclass carrying ()
  ((inventory :accessor inventory
              :initarg :inventory
              :initform (make-inventory)
              :type inventory)))

(defmethod inventory ((obj inventory))
  obj)

(defmethod inv-items ((obj carrying))
  (inv-items (inventory obj)))

(defmethod (setf inv-items) (arg (obj carrying))
  (setf (inv-items (inventory obj)) arg))

(defun make-inventory ()
  (make-instance 'inventory))

(defgeneric remove-item (obj inv))

(defmethod remove-item (obj (inv carrying))
  (setf (inv-items inv) (remove obj (inv-items inv))))

(defgeneric has-item (obj inv))

(defmethod has-item (obj (inv carrying))
  (member obj (inv-items inv)))

(defgeneric add-item (obj inv))

(defmethod add-item (obj (inv carrying))
  (pushnew obj (inv-items inv)))
