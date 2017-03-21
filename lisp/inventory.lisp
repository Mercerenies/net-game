
(defclass carrying ()
  ((inventory :accessor inventory
              :initarg :inventory
              :initform (make-inventory)
              :type inventory)))

;; `inventory` objects act like `carrying` objects so that the inventory-modification methods
;; can be written once and will behave correctly given an inventory OR a carrying instance.
(defclass inventory (carrying)
  ((items :accessor inv-items
          :initarg :items
          :initform nil
          :type list)
   (max-weight :accessor inv-max-weight
               :initarg :max-weight
               :initform 100
               :type integer))
  (:default-initargs :inventory nil))

(defmethod inventory ((obj inventory))
  obj)

(defmethod (setf inventory) (val (obj inventory))
  (declare (ignore val))
  (error "Cannot set an inventory's inventory slot"))

(defmethod inv-items ((obj carrying))
  (inv-items (inventory obj)))

(defmethod (setf inv-items) (arg (obj carrying))
  (setf (inv-items (inventory obj)) arg))

(defmethod inv-max-weight ((obj carrying))
  (inv-max-weight (inventory obj)))

(defmethod (setf inv-max-weight) (arg (obj carrying))
  (setf (inv-max-weight (inventory obj)) val))

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
  (when (can-carry-item obj inv)
    (pushnew obj (inv-items inv))))

(defgeneric inv-current-weight (inv))

(defmethod inv-current-weight ((inv carrying))
  (loop for item in (inv-items inv)
        sum (item-weight item)))

(defgeneric can-carry-item (obj inv))

(defmethod can-carry-item (obj (inv carrying))
  (<= (+ (inv-current-weight inv)
         (item-weight obj))
      (inv-max-weight inv)))
