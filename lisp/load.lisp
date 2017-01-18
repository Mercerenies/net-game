(in-package #:net-game)

; ///// Migrating to load-formatted

(defparameter *origin*
  "???")

; load-formatted is the general macro for loading S-expression data (alpha or delta) from the Ruby layer.
; Syntax:
;  (load-formatted data-value symbol . clauses)
;  clauses ::= (normal-clause* epilogue)
;  normal-clause ::= (var-name . exprs)
;  epilogue ::= keyword-clause* | rest-clause
;  keyword-clause ::= (:keyword var-name . exprs)
;  rest-clause ::= ((var-name) . exprs)
; The value returned at the end is the data value that was passed in. Note also that the rest clause, if
; supplied, may be called multiple times, once for each additional argument.
; Examples:
; (setq value (location id name :data (1 2 3) :string "abc"))
; (setq value1 (creature-set (blah) (blah) (blah)))
; (setq value2 (data))
; (load-formatted value 'location
;                 (id do-stuff)
;                 (name do-stuff)
;                 (:string value do-stuff)
;                 (:data value do-stuff)
;                 (:arg value do-stuff))
; (load-formatted value1 'creature-set
;                 ((rest) do-stuff))
; (load-formatted value2 'data)
(defmacro load-formatted (var sym &rest clauses)
  (let ((ptr (gensym))
        (pos clauses)
        (seq nil))
    (flet ((err (spec) `(error "Flawed data (~S): ~A" ,sym ,spec)))
      ; Regular arguments
      (let ((next `(setf ,ptr (cdr ,ptr)))
            (prologue `((setf ,ptr ,var)
                        (unless (eq (car ,ptr) ,sym)
                          ,(err "wrong header")))))
        (multiple-value-bind (named len)
            (loop while (and pos
                             (not (listp (caar pos)))
                             (not (keywordp (caar pos))))
                  append `(,next
                           (let ((,(caar pos) (car ,ptr)))
                             ,@(cdar pos))) into named-clause
                  count t into length-part
                  do (setf pos (cdr pos))
                  finally (return (values named-clause length-part)))
          (let ((named-check `((unless (>= (length ,ptr) ,(1+ len))
                                 ,(err "not enough normal arguments")))))
            (if (listp (caar pos))
                ; Rest arguments
                (let* ((rest-var (caaar pos))
                       (rest-clause `(,next
                                      (loop for ,rest-var in ,ptr
                                            do (progn ,@(cdar pos)))))
                       (final-return `(,var)))
                  `(let ((,ptr nil))
                     ,@prologue
                     ,@named-check
                     ,@named
                     ,@rest-clause
                     ,@final-return))
                ; Keyword arguments
                (let* ((case-clauses (loop for elem in pos
                                           collect `(,(car elem) (let ((,(cadr elem) (second ,ptr)))
                                                                   ,@(cddr elem)))))
                       (keyword-clause `((loop until (or (null ,ptr) (null (cdr ,ptr)))
                                               do ,next
                                               do (case (first ,ptr)
                                                    ,@case-clauses
                                                    (t ,(err `(format nil "unrecognized keyword ~S"
                                                                      (first ,ptr)))))
                                               do ,next)))
                       (final-check `(,next
                                      (unless (null ,ptr)
                                        ,(err "garbage at end of data"))
                                      ,var)))
                  `(let ((,ptr nil))
                     ,@prologue
                     ,@named-check
                     ,@named
                     ,@keyword-clause
                     ,@final-check)))))))))

(defclass alpha-load (loaded)
  ((world :accessor alpha-world
          :initform nil)
   (creatures :accessor alpha-creatures
              :initform nil)
   (spawners :accessor alpha-spawners
             :initform nil)
   (quests :accessor alpha-quests
           :initform nil)
   (knowledge :accessor alpha-knowledge
              :initform nil)))

(defun make-alpha ()
  (make-instance 'alpha-load))

; The argument to this should be an alpha-load object
(defmacro alpha-bind (alpha &body body)
  (let ((temp (gensym)))
    `(let ((,temp ,alpha))
       (let ((*world* (alpha-world ,temp))
             (*creatures* (alpha-creatures ,temp))
             (*spawners* (alpha-spawners ,temp))
             (*quests* (alpha-quests ,temp))
             (*knowledge-base* (alpha-knowledge ,temp)))
         ,@body))))

; NOTE: Eventually, we would like all of the load-* and delta-load-* functions to be converted to
;       load-object and delta-load-object calls with the appropriate first parameter. This will be
;       a slow process but it should result in a more organized codebase at the end. Some of
;       the "dispatch" loaders like load-creature should be modified to have a whitelist for security. (/////)
(defgeneric load-object (header data))

(defmethod load-object ((header (eql 'location)) data)
  (let ((inst (make-location nil "")))
    (load-formatted data 'location
                    (id (setf (get-id inst) id))
                    (name (setf (get-name inst) name)
                          (setf (location-short-name inst) name))
                    (:country country (setf (get-name inst) (format nil "~A, ~A"
                                                                    (get-name inst)
                                                                    country)))
                    (:links links (setf (location-exits inst) links))
                    (:contents contents (mapc #'(lambda (x) (load-map-object inst x)) contents))
                    (:civilized civilized (when civilized
                                            (add-flag 'civilized inst)))
                    (:fitness fitness (setf (location-fitness inst) (load-object 'fitness fitness)))
                    (:water water (case water
                                    ((nil))
                                    ((sea) (add-flag 'sea inst))
                                    ((shore) (add-flag 'shore inst))))
                    (:meta meta)) ; Ignored
    inst))

(defmethod load-object ((header (eql 'map)) data)
  (let ((hash (make-hash-table)))
    (load-formatted data 'map
                    ((loc) (let ((loc-node (load-object 'location loc)))
                             (setf (gethash (get-id loc-node) hash)
                                   loc-node))))
    hash))

(defmethod load-object ((header (eql 'quest-set)) data)
  (let ((*quests* (make-hash-table)))
    (load-formatted data 'quest-set
                    ((quest) (let ((quest-data (funcall (whitelisted-load-1 #'load-object +quest-types+) quest)))
                               (add-quest quest-data))))
    *quests*))

(defmethod load-object ((header (eql 'creature-set)) data)
  (load-with data
             (whitelisted-load-1 #'load-object +creature-types+)
             'creature-set))

(defmethod load-object ((header (eql 'spawner-set)) data)
  (load-with data
             (whitelisted-load-1 #'load-object +spawner-types+)
             'spawner-set))

(defmethod load-object ((header (eql 'fitness)) data)
  (let ((plist nil))
    (load-formatted data 'fitness
                    (treasure (setf (getf plist :treasure) treasure))
                    (monster (setf (getf plist :monster) monster)))
    plist))

(defun whitelisted-load (method headers data)
  "Performs the given load method operation on the data using dynamic dispatch, but only if
   the header of the data matches one of the elements in the headers whitelist."
  (check-type data cons "a non-empty list")
  (unless (member (car data) headers)
    (error "Element ~S does not satisfy the whitelist ~S" (car data) headers))
  (funcall method (car data) data))

(defun whitelisted-load-1 (method headers)
  "This is a curried form of whitelisted-load, provided for convenience. The behavior is
   identical."
  (lambda (x) (whitelisted-load method headers x)))

(defun load-with (data func header)
  (let ((list nil))
    (load-formatted data header
                    ((extra) (push (funcall func extra) list)))
    (reverse list))) ; TODO Can we avoid the reverse while maintaining the order?

; Returns an alpha-load instance
(defun load-data (&key (file *standard-input*))
  (let ((data (with-scheme-notation (read file)))
        (alpha (make-alpha))
        (*world* nil))
    (load-formatted data 'alpha
                    (world (setf *world* (load-object 'map world))
                           (setf (alpha-world alpha) *world*))
                    (creatures (setf (alpha-creatures alpha) (load-object 'creature-set creatures)))
                    (spawners (setf (alpha-spawners alpha) (load-object 'spawner-set spawners)))
                    (quests (setf (alpha-quests alpha) (load-object 'quest-set quests)))
                    (knowledge (setf (alpha-knowledge alpha) (load-object 'knowledge-base knowledge)))
                    (meta))
    alpha))

(defun load-map-object (node obj)
  (apply #'load-object-with-type node (car obj) (cdr obj)))

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
                    (setf (get-flags wpn) flags))
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
