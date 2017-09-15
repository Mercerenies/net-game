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
  "Whenever the object's HP is altered, if the object is damageable then check-for-death is run
   automatically."
  (check-for-death obj))

(defclass named ()
  ((name :accessor get-name
         :initarg :name
         :initform ""
         :type string))
  (:documentation "A base class for objects which have a name. Many display functions assume
                   the arguments are named objects."))

(defclass identifiable ()
  ((id :accessor get-id
       :initarg :id
       :initform nil
       :type t))
  (:documentation "A base class for objects which have an ID. The ID should be eql-comparable
                   and unique within the context of the program. In some cases, ID values of
                   integers are preferred or even required. As such, it is recommended that
                   IDs be integers whenever possible."))

(defclass located ()
  ((loc :accessor get-loc
        :initarg :loc
        :initform nil
        :type (or null location)))
  (:documentation "A base class for objects which have a position in the game world. The location
                   should not be directly mutated, in most cases. Instead, the move-object generic
                   function should be used."))

(defclass hideable ()
  ((hidden :accessor is-hidden
           :initarg :hidden
           :initform t
           :type boolean))
  (:documentation "A base class for objects which can be hidden from the player's view. A hidden
                   instance for which is-hidden yields true will not be shown in the location
                   contents list and will not be directly interact-able."))

(defclass flagged ()
  ((flags :accessor get-flags
          :initarg :flags
          :initform nil
          :type list))
  (:documentation "A base class for objects, such as items, which have flags. The list of flags should
                   be a list of lists of symbols or simply symbols. In general, flag symbols should be
                   interned, although this is not explicitly required. Flags should, in general, not be
                   referenced directly and should instead be referenced using the add-flag
                   and check-flag generic functions."))

(defclass loaded ()
  ((origin :accessor get-origin
           :initarg :origin
           :initform *origin*
           :type string))
  (:documentation "A base class for objects which were loaded from the data files. A loaded instance
                   maintains the file from which it was loaded, to help with debugging."))

(deftype node-linkage ()
  (member nil city-exit))

(defclass location (identifiable named flagged loaded)
  ((exits :accessor location-exits
          :initform nil
          :type list)
   (contents :accessor location-contents
             :initform nil
             :type list)
   (short-name :accessor location-short-name
               :initarg :short-name
               :initform ""
               :type string)
   (fitness :accessor location-fitness
            :initarg :fitness
            :initform nil ; This will be a plist
            :type list)
   (structure :accessor location-structure
              :initarg :structure
              :initform nil ; Will be an ID value
              :type t)
   (linkage :accessor location-linkage
            :initarg :linkage
            :initform nil
            :type node-linkage)))

(defun make-location (id name &key short-name)
  (check-type name string)
  (check-type short-name (or string null))
  (when (null short-name)
    (setf short-name name))
  (make-instance 'location
                 :id id
                 :name name
                 :short-name short-name))

(defun location-fitness-for (loc parm)
  (or (getf (location-fitness loc) parm)
      0.0))

(defgeneric add-flag (flag obj))

(defmethod add-flag ((flag symbol) (obj flagged))
  (push flag (get-flags obj)))

(defmethod add-flag ((flag list) (obj flagged))
  (loop for x in flag
        do (check-type x symbol "a hierarchical symbol flag"))
  (push flag (get-flags obj)))

(defgeneric check-flag (flag obj))

(defmethod check-flag ((flag symbol) (obj flagged))
  (member flag (get-flags obj)))

(defmethod check-flag ((flag list) (obj flagged))
  (member flag (get-flags obj) :test #'equal))

(defgeneric move-object (obj new-loc))

(defmethod move-object ((obj located) (new-loc location))
  (let ((old-loc (get-loc obj)))
    (when old-loc
      (setf (location-contents old-loc)
            (remove obj (location-contents old-loc))))
    (setf (get-loc obj) new-loc)
    (push obj (location-contents new-loc))))

(defmethod move-object ((obj located) (new-loc null))
  (let ((old-loc (get-loc obj)))
    (when old-loc
      (setf (location-contents old-loc)
            (remove obj (location-contents old-loc))))
    (setf (get-loc obj) new-loc)))

(defun halo (node &optional (n 1) &key (self t))
  (check-type *world* hash-table)
  (check-type node location)
  (if (not (plusp n))
      (list node)
      (loop for exit in (location-exits node)
            append (halo (gethash exit *world*) (1- n)) into result
            finally (if self
                        (return (remove-duplicates (cons node result)))
                        (return (remove-duplicates result))))))

(defmethod print-object ((obj named) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~S ~S"
            (if (typep obj 'identifiable)
                (get-id obj)
                nil)
            (get-name obj))))

;; Returns the "nature" of an object; this will only be used for debugging purposes
;;  (entity person item creature location unknown) are a few recommended values for
;;  object-nature, but any interned, non-keyword symbol can be returned.
(defgeneric object-nature (obj))

(defmethod object-nature ((obj t))
  'unknown)

(defmethod object-nature ((obj location))
  'location)

;; Returns a list of elements of the form (key-name friendly-name value)
(defgeneric system-keys (obj)
  (:method-combination append))

(defmethod system-keys append ((obj t))
  `((object-nature "Nature" ,(object-nature obj))))

(defmethod system-keys append ((obj damageable))
  `((hp "Health" ,(* 100 (hp obj)))))

(defmethod system-keys append ((obj flagged))
  `((get-flags "Flags" ,(get-flags obj))))

(defmethod system-keys append ((obj loaded))
  `((get-origin "Origin" ,(get-origin obj))))

; TODO Make the system-keys table have a maximum length for elements so they don't get too long

(defmethod system-keys append ((obj location))
  `((location-fitness "Fitness" ,(location-fitness obj))
    (location-structure "Structure Node" ,(location-structure obj))
    (location-linkage "Linkage" ,(location-linkage obj))))

(defmethod system-keys append ((obj hideable))
  `((is-hidden "Hidden" ,(is-hidden obj))))

(defun remove-hidden (lst)
  (remove-if (lambda (x) (and (typep x 'hideable) (is-hidden x))) lst))

(defun find-by-id (elem list)
  (check-type list list)
  (find elem list :key #'get-id))
