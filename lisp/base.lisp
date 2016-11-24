(in-package #:net-game)

(defclass damageable ()
  ((hp :accessor hp
       :initform 1
       :initarg :hp))
  (:documentation "A base class for objects that have a concept of health."))

(defun check-for-death (obj)
  "Checks whether or not the object, which should be a damageable instance, is in fact dead, removing
   it from the world if it is."
  (check-type obj (and damageable located))
  (when (<= (hp obj) 0)
    (move-object obj nil)))

(defmethod (setf hp) :after (val (obj damageable))
  "Whenever the object's HP is altered, if the object is damageable then check-for-death is run automatically."
  (check-for-death obj))

(defclass named ()
  ((name :accessor get-name
         :initarg :name
         :initform ""
         :type string))
  (:documentation "A base class for objects which have a name. Many display functions assume the arguments
                   are named objects."))

(defclass identifiable ()
  ((id :accessor get-id
       :initarg :id
       :initform nil
       :type t))
  (:documentation "A base class for objects which have an ID. The ID should be eql-comparable and unique
                   within the context of the program. In some cases, ID values of integers are preferred
                   or even required. As such, it is recommended that IDs be integers whenever possible."))

(defclass located ()
  ((loc :accessor get-loc
        :initarg :loc
        :initform nil
        :type (or null location)))
  (:documentation "A base class for objects which have a position in the game world. The location should
                   not be directly mutated, in most cases. Instead, the move-object generic function
                   should be used."))

(defclass flagged ()
  ((flags :accessor get-flags
          :initarg :flags
          :initform nil
          :type list))
  (:documentation "A base class for objects, such as items, which have flags. The list of flags should
                   be a list of symbols. In general, flag symbols should be interned, although this is
                   not explicitly required. Flags can be more easily interacted with using add-flag
                   and check-flag."))

(defclass loaded ()
  ((origin :accessor get-origin
           :initarg :origin
           :initform *origin*
           :type string))
  (:documentation "A base class for objects which were loaded from the data files. A loaded instance
                   maintains the file from which it was loaded, to help with debugging."))

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

(defmethod system-keys append ((obj loaded))
  `((get-origin "Origin" ,(get-origin obj))))
