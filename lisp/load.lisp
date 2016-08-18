(in-package #:net-game)

; TODO Consolidate some things, like how player and animal have HP; they should share an ancestor
;      CLOS has multiple inheritance; use it

(defmethod print-object ((obj named) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~S ~S"
            (if (typep obj 'identifiable)
                (get-id obj)
                nil)
            (get-name obj))))

(defclass location (identifiable named)
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
   (civilized :accessor location-civilized
              :initarg :civilized
              :initform nil
              :type boolean)))

(defclass warp-point (named located)
  ((active :accessor warp-active
           :initform nil
           :type boolean))
  (:default-initargs :name "Warp Point"))

(defun move-object (obj new-loc)
  (check-type obj located)
  (check-type new-loc location)
  (let ((old-loc (get-loc obj)))
    (when old-loc
      (setf (location-contents old-loc)
            (remove obj (location-contents old-loc))))
    (setf (get-loc obj) new-loc)
    (when new-loc
      (push obj (location-contents new-loc)))))

(defun load-data (&key (file *standard-input*))
  (let ((data (with-scheme-notation (read file))))
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
                           (:civilized (setf (location-civilized inst) value))))
             collect inst))
     (destructuring-bind (anim-sym . anims) (second data)
       (unless (eq anim-sym 'creature-set) (error "Flawed data"))
       (loop for data in anims
             collect (apply #'load-creature (first data) (rest data))))
     (destructuring-bind (spawner-sym . spawners) (third data)
       (unless (eq spawner-sym 'spawner-set) (error "Flawed data"))
       (loop for data in spawners
             collect (apply #'load-spawner (first data) (rest data))))
     (destructuring-bind (quest-sym . quests) (fourth data)
       (unless (eq quest-sym 'quest-set) (error "Flawed data"))
       (loop with hash = (make-hash-table)
             for data in quests
             for quest = (apply #'load-quest (first data) (rest data))
             do (let ((*quests* hash))
                  (add-quest quest))
             finally (return hash))))))

(defun load-object (node obj)
  (apply #'load-object-with-type node (car obj) (cdr obj)))

(defgeneric entity-turn (obj))

(defmethod entity-turn ((obj t))
  ; By default, do nothing
  nil)

(defgeneric load-object-with-type (node type &rest args))

(defmethod load-object-with-type (node (type (eql 'player)) &rest args)
  (declare (ignore args))
  (let ((obj (make-instance 'player)))
    (move-object obj node)))

(defmethod load-object-with-type (node (type (eql 'warp-point)) &rest args)
  (declare (ignore args))
  (let ((obj (make-instance 'warp-point)))
    (move-object obj node)))

(defmethod load-object-with-type (node (type (eql 'item)) &rest args)
  (let* ((name (first args))
         (args (rest args))
         (item (apply #'make-instance 'item :name name args)))
    (move-object item node)))

(defmethod load-object-with-type (node (type (eql 'weapon)) &rest args)
  (loop with name = (first args)
        with type = nil
        with mod = nil
        with flags = nil
        for rest = (cdr args) then (cddr rest)
        for key = (first rest)
        for value = (second rest)
        while (not (null rest))
        do (case key
             (:type (setf type value))
             (:mod (setf mod value))
             (:flags (setf flags value)))
        finally (let ((wpn (make-weapon name type mod)))
                  (when flags
                    (setf (item-flags wpn) flags))
                  (move-object wpn node))))

(defmethod load-object-with-type (node (type (eql 'plant)) &rest args)
  (loop with name = (first args)
        with type = nil
        with food = nil
        with growth-time = 5
        for rest = (cdr args) then (cddr rest)
        for key = (first rest)
        for value = (second rest)
        while (not (null rest))
        do (case key
             (:type (setf type value))
             (:food (progn
                      (unless (eq (car value) 'food) (error "Flawed data"))
                      (setf food (apply #'make-instance 'food-data :name (cdr value)))))
             (:growth-time (setf growth-time value)))
        finally (let ((plant (make-instance 'plant :name name :type type :food food :growth-time growth-time)))
                  (move-object plant node))))

; Uses *world*
(defun halo (node &optional (n 1) &key (self t))
  (if (not (plusp n))
      (list node)
      (loop for exit in (location-exits node)
            append (halo (find exit *world* :key #'get-id) (1- n)) into result
            finally (if self
                        (return (remove-duplicates (cons node result)))
                        (return (remove-duplicates result))))))
