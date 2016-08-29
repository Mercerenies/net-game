(in-package #:net-game)

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

(defun make-location (id name &key short-name)
  (make-instance 'location
                 :id id
                 :name name
                 :short-name short-name))

(defclass warp-point (named located)
  ((active :accessor warp-active
           :initform nil
           :type boolean))
  (:default-initargs :name "Warp Point"))

(defun make-warp-point ()
  (make-instance 'warp-point))

(defun move-object (obj new-loc)
  (check-type obj located)
  (check-type new-loc (or location null))
  (let ((old-loc (get-loc obj)))
    (when old-loc
      (setf (location-contents old-loc)
            (remove obj (location-contents old-loc))))
    (setf (get-loc obj) new-loc)
    (when new-loc
      (push obj (location-contents new-loc)))))

(defun load-loc (loc)
  (destructuring-bind (loc id name . rst) loc
    (let ((inst (make-location id name :short-name name)))
      (loop for elems = rst then (cdr (cdr elems))
            for key = (first elems)
            for value = (second elems)
            while elems
            do (case key
                 (:country (setf (get-name inst) (format nil "~A, ~A"
                                                         (get-name inst)
                                                         value)))
                 (:links (setf (location-exits inst) value))
                 (:contents (mapc #'(lambda (x) (load-object inst x)) value))
                 (:civilized (setf (location-civilized inst) value))
                 (:meta))) ; Explicitly ignore this case
      inst)))

(defun load-with (data func header)
  (unless (eq (first data) header)
    (error "Flawed data - ~(~S~)" header))
  (loop for dd in (rest data)
        collect (apply func dd)))

; Returns (values map creatures spawners quests)
(defun load-data (&key (file *standard-input*))
  (let ((data (with-scheme-notation (read file))))
    ; Note that the sixth element of data is meta and is intentionally ignored by this segment of the program
    (unless (eq (first data) 'alpha)
      (error "Flawed data - alpha"))
    (values
     (destructuring-bind (map-sym . locs) (second data)
       (unless (eq map-sym 'map) (error "Flawed data - map"))
       (loop for loc in locs
             collect (load-loc loc)))
     (destructuring-bind anims (third data)
       (load-with anims #'load-creature 'creature-set))
     (destructuring-bind spawners (fourth data)
       (load-with spawners #'load-spawner 'spawner-set))
     (destructuring-bind (quest-sym . quests) (fifth data)
       (unless (eq quest-sym 'quest-set) (error "Flawed data - quest-set"))
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
  (let ((obj (make-player)))
    (move-object obj node)))

(defmethod load-object-with-type (node (type (eql 'warp-point)) &rest args)
  (declare (ignore args))
  (let ((obj (make-warp-point)))
    (move-object obj node)))

(defmethod load-object-with-type (node (type (eql 'item)) &rest args)
  (let* ((name (first args))
         (args (rest args))
         (item (apply #'make-item name args)))
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
                      (unless (eq (car value) 'food) (error "Flawed data - food"))
                      (setf food (apply #'make-food-data (cdr value)))))
             (:growth-time (setf growth-time value)))
        finally (let ((plant (make-plant name :type type :food food :growth-time growth-time)))
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
