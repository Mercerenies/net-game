(in-package #:net-game)

(defclass named ()
  ((id :accessor get-id
       :initarg :id
       :initform nil
       :type t)
   (name :accessor get-name
         :initarg :name
         :initform ""
         :type string)))

(defmethod print-object ((obj named) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~S ~S" (get-id obj) (get-name obj))))

(defclass located ()
  ((loc :accessor get-loc
        :initarg :loc
        :initform nil
        :type (or null location))))

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
               :type string)))

(defclass warp-point (named located)
  ((active :accessor warp-active
           :initform nil
           :type boolean))
  (:default-initargs :name "Warp Point"))

(defun move-object (obj new-loc)
  (let ((old-loc (get-loc obj)))
    (when old-loc
      (setf (location-contents old-loc)
            (remove obj (location-contents old-loc))))
    (setf (get-loc obj) new-loc)
    (push *player* (location-contents new-loc))))

(defun load-data (&key (file *standard-input*))
  (destructuring-bind (map-sym . locs) (read file)
    (unless (eq map-sym 'map) (error "Flawed data"))
    (loop for (loc id name . rst) in locs
          for inst = (make-instance 'location :name name :short-name name :id id)
          do (loop for elems = rst then (cdr (cdr elems))
                   for key = (first elems)
                   for value = (second elems)
                   while elems
                   do (case key
                        (:country (setf (get-name inst) (format nil "~A, ~A"
                                                                (get-name inst)
                                                                value)))
                        (:links (setf (location-exits inst) value))))
          collect inst)))
