(in-package #:net-game)

(defparameter *world-info*
  nil)

(defun load-world-info (filename &optional (reset nil) &aux (*read-eval* nil))
  (when reset
    (setf *world-info* nil))
  (with-open-file (stream filename :direction :input)
    (setf *world-info*
          (nconc *world-info*
                 (loop for next = (read stream nil nil)
                       while next
                       collect (load-instance next))))))

(defclass node (named)
  ((short-name :accessor node-short-name
               :initform ""
               :initarg :short-name)
   (interior :accessor node-interior
             :initform nil)
   (exits :accessor node-exits
          :initform nil)
   (meta :accessor node-meta
         :initform (make-instance 'meta)
         :initarg :meta)
   (flags :accessor node-flags
          :initform nil
          :initarg :flags)))
; Flags: (warp-point)

(defclass top-node (node) ())
(defclass country-node (node) ())
(defclass city-node (node) ())
(defclass district-node (node) ())

; These two don't do anything special right now.
; I plan to make exits into a struct/class later.
(defun exit-node (x)
  x)

(defun make-exit (dest)
  dest)

(defun connect (n1 n2)
  (unless (or (member n1 (mapcar #'exit-node (node-exits n2)))
              (member n2 (mapcar #'exit-node (node-exits n1)))
              (eq n1 n2))
    (push (make-exit n2) (node-exits n1))
    (push (make-exit n1) (node-exits n2))))

(defun partitions (nodes)
  (when (null nodes)
    (return-from partitions nil))
  (let ((unvisited (copy-list nodes)))
    (labels ((traverse (node)
               (when (member node unvisited)
                 (setf unvisited (remove node unvisited))
                 (mapcan (lambda (x) (traverse (exit-node x)))
                         (node-exits node)))))
      (loop until (null unvisited)
            collect (traverse (first unvisited))))))

(defun interconnect (forest)
  (loop for i = (nth (random (length forest)) forest)
        for j = (nth (random (length forest)) forest)
        until (<= (length (partitions forest)) 1)
        do (connect i j)))

(defgeneric expand-node (node))

(defmethod expand-node ((node node))
  "By default, we want to not expand at all. Only for higher level nodes do
   we expand.")

(defun make-base-node (node affix &key (city nil))
  (make-instance 'node
                 :name
                 (format nil "~A~@[, ~A~]"
                         (apply-affix
                          (node-short-name node) affix)
                         (and
                          (meta-country (node-meta node))
                          (get-name (meta-country
                                     (node-meta node)))))
                 :short-name
                 (format nil "~A"
                         (apply-affix
                          (node-short-name node) affix))
                 :meta
                 (make-instance
                  'meta
                  :country (meta-country (node-meta node))
                  :city city)))

(defmethod expand-node ((node city-node))
  (unless (node-interior node)
    (let* ((node-count (min (+ 3 (round (/ (log (random 1.0)) -0.85)))
                            (loop for value in *name-affixes*
                                  maximize (length value))))
           (node-affixes (remove-if (lambda (x)
                                      (/= (length x) node-count))
                                    *name-affixes*))
           (node-affix (nth (random (length node-affixes)) node-affixes))
           (city (make-instance 'independent-state :name (get-name node)))
           (children (loop repeat node-count
                           for aff in node-affix
                           for char = #\A then (code-char (1+ (char-code char)))
                           collect (make-base-node node aff :city city))))
      (when (member 'warp-point (node-flags node))
        (push 'warp-point (node-flags (nth (random (length children)) children))))
      (interconnect children)
      (setf (node-interior node) children)
      (mapc (lambda (x) (push x (state-parts city))) children)
      (mapc #'expand-node children))))

(defmethod expand-node ((node district-node))
  (unless (node-interior node)
    (let* ((node-count (min (+ 3 (round (/ (log (random 1.0)) -0.85)))
                            (loop for value in *name-affixes*
                                  maximize (length value))))
           (node-affixes (remove-if (lambda (x)
                                      (/= (length x) node-count))
                                    *name-affixes*))
           (node-affix (nth (random (length node-affixes)) node-affixes))
           (city (make-instance 'independent-state :name (get-name node)))
           (children (loop repeat node-count
                           for aff in node-affix
                           for char = #\A then (code-char (1+ (char-code char)))
                           collect (make-base-node node aff :city city))))
      (when (member 'warp-point (node-flags node))
        (push 'warp-point (node-flags (nth (random (length children)) children))))
      (interconnect children)
      (setf (node-interior node) children)
      (mapc (lambda (x) (push x (state-parts city))) children)
      (mapc #'expand-node children))))

(defmethod expand-node ((node country-node))
  (unless (node-interior node)
    (let* ((node-count (+ 3 (round (/ (log (random 1.0)) -0.85))))
           (country (make-instance 'independent-state :name (get-name node)))
           (children
            (loop repeat node-count
                  for typen = (random 2)
                  for type = (if (= typen 0) 'city-node 'district-node)
                  collect (let ((name (generate-name :trim t)))
                            (make-instance type
                                           :name (format nil "~A, ~A"
                                                         name (get-name node))
                                           :short-name name
                                           :meta (make-instance 'meta
                                                                :country country))))))
      (when (member 'warp-point (node-flags node))
        (push 'warp-point (node-flags (nth (random (length children)) children))))
      (interconnect children)
      (setf (node-interior node) children)
      (mapc (lambda (x) (push x (state-parts country))) children)
      (mapc #'expand-node children))))

(defun create-toplevel-node (pl)
  (let* ((node (make-instance (case (car (place-info pl))
                                (city 'city-node)
                                (district 'district-node)
                                (country 'country-node)
                                ; TODO Make this a different idea
                                ; (states could contain small numbers of cities)
                                (state 'country-node)
                                (t (warn "Invalid node type ~S"
                                         (place-info pl))
                                   'city-node))
                              :name (get-name pl) :short-name (get-name pl)
                              :flags '(warp-point)))
         (pos (position #\, (get-name node))))
    (cond (pos (setf (node-short-name node) (subseq (get-name node) 0 pos))
               (unless (meta-country (node-meta node))
                 (setf (meta-country (node-meta node))
                       (make-instance 'independent-state
                                      :name (subseq (get-name node)
                                                    (min (+ pos 2)
                                                         (length (get-name node))))))))
          ((not (meta-country (node-meta node)))
           (setf (meta-country (node-meta node))
                 (make-instance 'independent-state
                                :name (generate-name :trim t))))
          (t nil))
    node))

; Uses *world-info*
(defmethod expand-node ((node top-node))
  (unless (node-interior node)
    (let* ((places (remove-if (lambda (x)
                                (typecase x
                                  (place (not (member (car (place-info x))
                                                      '(city state district country))))
                                  (t t)))
                              *world-info*))
           (children (loop for pl in places
                           collect (create-toplevel-node pl))))
      (interconnect children)
      (setf (node-interior node) children)
      (mapc #'expand-node children)
      (setf *world-info* (remove-if (lambda (x) (member x places)) *world-info*)))))

(defun connect-locs (l1 l2 &key (name1 nil) (name2 nil))
  (unless (or (member l1 (mapcar #'exit-dest (location-exits l2)))
              (member l2 (mapcar #'exit-dest (location-exits l1)))
              (eq l1 l2))
    (setf name1 (or name1 (format nil "To ~A" (location-short-name l2))))
    (setf name2 (or name2 (format nil "To ~A" (location-short-name l1))))
    (push (make-instance 'loc-exit :name name1 :dest l2) (location-exits l1))
    (push (make-instance 'loc-exit :name name2 :dest l1) (location-exits l2))
    (list l1 l2)))

(defgeneric compile-node (node))

(defmethod compile-node ((node node))
  (if (null (node-interior node))
      (let ((loc (make-instance 'location
                                :name (get-name node)
                                :short-name (node-short-name node)
                                :meta (node-meta node))))
        (loop for fl in (node-flags node)
              do (case fl
                   (warp-point (push (make-instance 'warp-point :loc loc)
                                     (location-contents loc)))))
        (cons (list loc) node))
      (let ((nodes (mapcar (lambda (x) (compile-node (exit-node x)))
                           (node-interior node))))
        (loop for (l1s . n) in nodes
              do (loop for ex in (node-exits n)
                       for (l2s . n-) = (rassoc (exit-node ex) nodes)
                       for l1 = (nth (random (length l1s)) l1s)
                       for l2 = (nth (random (length l2s)) l2s)
                       do (connect-locs l1 l2)))
        (cons (mapcan #'car
                      (remove-if (lambda (x) (typep x 'building-node))
                                 nodes
                                 :key #'cdr))
              node))))
                                        ; TODO Something better than this obvious
                                        ; special case exception for buildings.

(defun show-nodes (stream node &optional col at)
  (format stream "(node ~S exits=~S interior=(~{~/show-nodes/~^ ~}))"
          (get-name node)
          (mapcar (lambda (x) (get-name (exit-node x))) (node-exits node))
          (node-interior node)))

(defun load-everything ()
  (load-world-info "world.txt" t)
  (let ((node (make-instance 'top-node)))
    (expand-node node)
    node))
