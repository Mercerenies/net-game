
; DEBUG CODE The couple of (format t) directives here

(defclass building-node (node)
  ((nature :accessor building-node-nature
           :initform nil
           :initarg :nature
           :type (or null symbol))
   (entrypoints :accessor building-node-entry
                :initform nil
                :initarg :entry
                :type list)))

(defmethod expand-node ((node building-node))
  (unless (node-interior node)
    (setf (node-interior node)
          (generate-building (building-node-nature node)))))

(defgeneric generate-building (node nature)
  (:documentation "Returns the node argument that was passed in, after modifying it
                   to have an applicable interior."))

(defmethod no-applicable-method ((gf (eql #'generate-building)) &rest args)
  (declare (ignore args))
  nil)

(defmethod generate-building (node (nature (eql 'tower)))
  (loop with height = (+ 4 (random 5))
        for index from 1 to height
        for prev = nil then curr
        for curr = (make-instance 'node
                                  :name (format nil "~A Floor ~D"
                                                (get-name node) index)
                                  :short-name (format nil "~A Floor ~D"
                                                      (node-short-name node) index)
                                  :meta (let ((cc (copy-meta (node-meta node))))
                                          (setf (meta-nature cc) 'building)
                                          cc))
        do (push curr (node-interior node))
        when (null prev)
            do (push curr (building-node-entry node))
        else
            do (connect curr prev)
        finally (return node)))

(defconstant +park-names+
  '(("Lower " . "") ("Upper " . "") ("" . " Flowers") ("" . " Path")
    ("" . " Circle") ("" . " Tunnel") ("" . " Grounds")))

(defun generate-ring-park (node parts)
  (loop for outer on parts
        when (> (length outer) 1)
            do (connect (first outer) (second outer))
        else
            do (connect (first outer) (first parts)))
  (loop repeat (random 4)
        do (connect (choose parts) (choose parts)))
  (setf (node-interior node) parts)
  (setf (building-node-entry node) (loop repeat (1+ (random 4))
                                         collect (choose parts)))
  node)

(defun generate-central-park (node parts)
  (let* ((affix (choose '(("" . " Center") ("" . " Great Tree") ("" . " Plaza")
                          ("Central " . ""))))
         (center (make-instance 'node
                               :name (apply-affix (get-name node) affix)
                               :short-name (apply-affix (node-short-name node) affix)
                               :meta (let ((cc (copy-meta (node-meta node))))
                                       (setf (meta-nature cc) 'building)
                                       cc))))
    (loop for outer in parts
          do (connect outer center))
    (loop repeat (random 8)
          for node1 = (choose parts)
          for node2 = (choose parts)
          unless (eq node1 node2)
              do (connect node1 node2))
    (setf (node-interior node) (cons center parts))
    (setf (building-node-entry node) (loop repeat (1+ (random 4))
                                           collect (choose parts)))
    node))

(defmethod generate-building (node (nature (eql 'park)))
  (let ((nodes (loop with park-names = (copy-list +park-names+)
                     repeat (+ 2 (random (- (length park-names) 2)))
                     collect (let ((affix (choose park-names)))
                               (setf park-names (remove affix park-names))
                               (make-instance 'node
                                              :name (apply-affix (get-name node) affix)
                                              :short-name (apply-affix
                                                           (node-short-name node)
                                                           affix)
                                              :meta (let ((cc
                                                           (copy-meta (node-meta node))))
                                                      (setf (meta-nature cc) 'building)
                                                      cc))))))
    (funcall (choose (list #'generate-ring-park #'generate-central-park)) node nodes)
    node))

; Uses *world-info*
(defun create-buildings ()
  (loop for pl in (remove-if (lambda (x) (not (typep x 'place))) *world-info*)
        for node = (make-instance 'building-node
                                  :name (get-name pl)
                                  :short-name (get-name pl)
                                  :nature (car (place-info pl)))
        do (generate-building node (building-node-nature node))
        when (node-interior node)
            collect pl into rem and
            collect node into result
        finally (progn
                  (setf *world-info* (set-difference *world-info* rem))
                  (return result))))

(defun integrate-buildings (toplevel entry-points)
  (check-type entry-points list "a list of buildings.")
  (check-type toplevel node "a node")
  (format t "~%It's integrating ~D buildings.~%" (length entry-points)) ; DEBUG CODE
  (loop for bld in entry-points
        collect (integrate-building bld toplevel)))

(defun integrate-building (bld node)
  (when (null bld)
    (return-from integrate-building nil))
  (cond ((null (node-interior node)) nil)
        ((some #'(lambda (x) (null (node-interior x)))
                         (node-interior node))
         (push bld (node-interior node))
         (let* ((index (random (length (node-interior node))))
                (connector (nth index (node-interior node))))
           (connect connector bld))
         (format t "Integrate ~S ~S~%"
                 (get-name bld)
                 (get-name node)))
        (t (let* ((interior (node-interior node))
                  (index (random (length interior)))
                  (choice (nth index interior)))
             (integrate-building bld choice)))))

(defmethod compile-node ((node building-node))
  (let ((nodes (mapcar (lambda (x) (compile-node (exit-node x)))
                       (node-interior node))))
    (loop for (l1s . n) in nodes
          do (loop for ex in (node-exits n)
                   for (l2s . n-) = (rassoc (exit-node ex) nodes)
                   for l1 = (nth (random (length l1s)) l1s)
                   for l2 = (nth (random (length l2s)) l2s)
                   do (connect-locs l1 l2)))
    (let* ((entry-points (remove-if (lambda (x) (not (member (cdr x)
                                                             (building-node-entry node))))
                                    nodes)))
      (format t "Compiling and giving me ~S~%" entry-points)
      (cons (mapcan #'car entry-points) node))))

; ///// Parks aren't integrating properly in some cases
