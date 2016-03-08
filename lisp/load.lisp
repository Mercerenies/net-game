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
   (creatures :accessor location-creatures
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
    (when new-loc
      (push obj (location-contents new-loc)))))

(defun load-data (&key (file *standard-input*))
  (let ((data (read file)))
    (values
     (destructuring-bind (map-sym . locs) (first data)
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
                           (:links (setf (location-exits inst) value))
                           (:contents (mapc #'(lambda (x) (load-object inst x)) value))
                           (:creatures (mapc #'(lambda (x)
                                                 (push (location-creatures inst) x))
                                             value))))
             collect inst))
     (destructuring-bind (anim-sym . anims) (second data)
       (unless (eq anim-sym 'creature-set) (error "Flawed data"))
       (loop for data in anims
             collect (apply #'load-creature (first data) (rest data)))))))

(defun load-object (node obj)
  (apply #'load-object-with-type node (car obj) (cdr obj)))

(defgeneric load-object-with-type (node type &rest args))

(defmethod load-object-with-type (node (type (eql 'player)) &rest args)
  (declare (ignore args))
  (let ((obj (make-instance 'player)))
    (move-object obj node)))

(defmethod load-object-with-type (node (type (eql 'warp-point)) &rest args)
  (declare (ignore args))
  (let ((obj (make-instance 'warp-point)))
    (move-object obj node)))

(defmethod load-object-with-type (node (type (eql 'weapon)) &rest args)
  (loop with name = (first args)
        with type = nil
        with mod = nil
        for rest = (cdr args) then (cddr rest)
        for key = (first rest)
        for value = (second rest)
        while (not (null rest))
        do (case key
             (:type (setf type value))
             (:mod (setf mod value)))
        finally (let ((wpn (make-weapon name type mod)))
                  (move-object wpn node))))
