
(defclass named ()
  ((name :accessor get-name
         :initarg :name
         :initform ""
         :type string)))

(defmethod print-object ((obj named) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~S" (get-name obj))))

(defclass located ()
  ((loc :accessor get-loc
        :initarg :loc
        :initform nil
        :type (or null location))))

(defclass person (named)
  ((gender :accessor person-gender
           :initarg :gender
           :initform nil
           :type (member nil male female))
   (occupations :accessor person-occupations
                :initarg :occupations
                :initform nil
                :type list)))

(defclass place (named)
  ((info :accessor place-info
         :initarg :info
         :initform nil
         :type list)))

(defclass weapon (named)
  ((type :accessor weapon-type
         :initarg :type
         :initform nil
         :type list)))

(defun load-instance (arg)
  (unless (member (car arg) '(person place weapon))
    (error "Instance ~S not in whitelist" (car arg)))
  (apply #'make-instance (car arg) :name (cdr arg)))

(defclass independent-state (named)
  ((parts :accessor state-parts
          :initform nil
          :initarg :parts
          :type list)))

(defclass meta ()
  ((country :accessor meta-country
            :initarg :country
            :initform nil
            :type (or null independent-state))
   (city :accessor meta-city
         :initarg :city
         :initform nil
         :type (or null independent-state))
   (nature :accessor meta-nature
           :initarg :nature
           :initform 'city
           :type (member 'city 'wild 'building))))

(defclass location (named)
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
   (meta :accessor location-meta
         :initarg :meta
         :initform (make-instance 'meta)
         :type meta)))

(defclass loc-exit (named)
  ((dest :accessor exit-dest
         :initarg :dest
         :type location)))

(defclass warp-point (named located)
  ((active :accessor warp-active
           :initform nil
           :type boolean))
  (:default-initargs :name "Warp Point"))

(defun copy-meta (meta)
  (make-instance 'meta
                 :country (meta-country meta)
                 :city (meta-city meta)
                 :nature (meta-nature meta)))

(defun move-object (obj new-loc)
  (let ((old-loc (get-loc obj)))
    (when old-loc
      (setf (location-contents old-loc)
            (remove obj (location-contents old-loc))))
    (setf (get-loc obj) new-loc)
    (push *player* (location-contents new-loc))))
