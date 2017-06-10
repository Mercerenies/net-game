(in-package #:net-game)

(defparameter *origin*
  "???")

(defconstant +map-object-types+
  '(player warp-point item weapon plant npc neo-spawner))

(defmacro load-formatted (var sym &rest clauses)
  "load-formatted is the general macro for loading S-expression data (alpha or delta) from the
   Ruby layer.
    Syntax:
    (load-formatted data-value symbol . clauses)
    clauses ::= (normal-clause* epilogue)
    normal-clause ::= (var-name . exprs)
    epilogue ::= keyword-clause* | rest-clause
    keyword-clause ::= (:keyword var-name . exprs)
    rest-clause ::= ((var-name) . exprs)
   The value returned at the end is the data value that was passed in. Note also that the rest
   clause, if supplied, may be called multiple times, once for each additional argument.
   Examples:
   (setq value '(location id name :data (1 2 3) :string \"abc\"))
   (setq value1 '(creature-set (blah) (blah) (blah)))
   (setq value2 '(data))
   (load-formatted value 'location
                   (id do-stuff)
                   (name do-stuff)
                   (:string value do-stuff)
                   (:data value do-stuff)
                   (:arg value do-stuff))
   (load-formatted value1 'creature-set
                   ((rest) do-stuff))
   (load-formatted value2 'data)"
  (let ((ptr (gensym))
        (pos clauses)
        (seq nil))
    (flet ((err (spec) `(error "Flawed data (~S): ~A" ,sym ,spec)))
      ;; Regular arguments
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
                             ,@(cdar pos)))
                      into named-clause
                  count t into length-part
                  do (setf pos (cdr pos))
                  finally (return (values named-clause length-part)))
          (let ((named-check `((unless (>= (length ,ptr) ,(1+ len))
                                 ,(err "not enough normal arguments")))))
            (if (listp (caar pos))
                ;; Rest arguments
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
                ;; Keyword arguments
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
              :initform nil)
   (key :accessor alpha-key
        :initform nil)
   (pool :accessor alpha-pool
         :initform nil)
   (debug :accessor alpha-debug
          :initform 0)))

(defun make-alpha ()
  (make-instance 'alpha-load))

;; The argument to this should be an alpha-load object
(defmacro alpha-bind (alpha &body body)
  (let ((temp (gensym)))
    `(let ((,temp ,alpha))
       (let ((*world* (alpha-world ,temp))
             (*creatures* (alpha-creatures ,temp))
             (*spawners* (alpha-spawners ,temp))
             (*quests* (alpha-quests ,temp))
             (*knowledge-base* (alpha-knowledge ,temp))
             (*key* (alpha-key ,temp))
             (*debug-level* (alpha-debug ,temp))
             (*object-pool* (alpha-pool ,temp)))
         ,@body))))

(defgeneric load-object (header data)
  (:documentation "Load the object with the nature given by the header, which should be a symbol and is
                   often equal to (car data). The data argument should be the actual data to load,
                   including the header. It is load-object's responsibility to verify that the
                   object being loaded from data is in fact the object specified by the header
                   symbol; no input validation need be done by the caller. Assuming the data
                   is valid, load-object should return an object of the appropriate nature and
                   should not have any observable side effects on the game world."))

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
                    (:contents contents (mapc #'(lambda (x) (load-then-position x inst)) contents))
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

(defmethod load-object ((header (eql 'request-set)) data)
  nil) ; TODO Yeah, this needs to be implemented ////

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
  (collecting
   (load-formatted data header
                   ((extra) (push-back (funcall func extra))))))

;; Returns an alpha-load instance
(defun load-data (&key (file *standard-input*))
  (let ((data (with-scheme-notation (read file)))
        (alpha (make-alpha))
        (*world* nil))
    (load-formatted data 'alpha
                    (key (setf (alpha-key alpha) key))
                    (world (setf *world* (load-object 'map world))
                           (setf (alpha-world alpha) *world*))
                    (creatures (setf (alpha-creatures alpha) (load-object 'creature-set creatures)))
                    (spawners (setf (alpha-spawners alpha) (load-object 'spawner-set spawners)))
                    (quests (setf (alpha-quests alpha) (load-object 'quest-set quests)))
                    (knowledge (setf (alpha-knowledge alpha) (load-object 'knowledge-base knowledge)))
                    (pool (setf (alpha-pool alpha)
                                (load-with pool
                                           (whitelisted-load-1 #'load-object +map-object-types+)
                                           'pool)))
                    (debug (setf (alpha-debug alpha) debug))
                    (reqs (load-object 'request-set reqs)) ; ////
                    (meta))
    alpha))

(defun load-then-position (data node)
  (let ((loaded (whitelisted-load #'load-object +map-object-types+ data)))
    (move-object loaded node)))

(defmethod load-object ((header (eql 'player)) data)
  (declare (ignore data))
  (make-player))

(defmethod load-object ((header (eql 'warp-point)) data)
  (declare (ignore data))
  (make-warp-point))

(defmethod load-object ((header (eql 'item)) data)
  (let ((item (make-item "")))
    (load-formatted data 'item
                    (name (setf (get-name item) name))
                    (:weight weight (setf (item-weight item) weight))
                    (:flags flags (setf (get-flags item) flags)))
    item))

(defmethod load-object ((header (eql 'weapon)) data)
  ;; TODO Can we make the weapon up front and apply the modifier later? This is sort of backwards-feeling now.
  (let ((name nil)
        (type nil)
        (mod nil)
        (flags nil))
    (load-formatted data 'weapon
                    (name-1 (setf name name-1))
                    (:mod mod-1 (setf mod mod-1))
                    (:type type-1 (setf type type-1))
                    (:flags flags-1 (setf flags flags-1)))
    (let ((wpn (make-weapon name type mod)))
      (setf (get-flags wpn) flags)
      wpn)))

(defmethod load-object ((header (eql 'plant)) data)
  ;; TODO See the note for weapon's load-object above; same thing applies here
  (let ((name nil)
        (type nil)
        (food nil)
        (time 5))
    (load-formatted data 'plant
                    (name-1 (setf name name-1))
                    (:type type-1 (setf type type-1))
                    (:food food-1 (progn
                                    (unless (eq (car food-1) 'food) (error "Flawed data - food"))
                                    (setf food (apply #'make-food-data (cdr food-1))))) ; TODO load-formatted
                    (:growth-time time-1 (setf time time-1)))
    (make-plant name :type type :food food :growth-time time)))

(defmethod load-object ((header (eql 'npc)) data)
  (let ((person (make-person nil "")))
    (load-formatted data 'npc
                    (id (setf (get-id person) id))
                    (name (setf (get-name person) name))
                    (:short-name x (setf (person-nickname person) x))
                    (:gender x (setf (person-gender person) x))
                    (:job x (setf (person-job person) x))
                    (:job-name x (setf (person-job-name person) x))
                    (:old-job x (setf (person-old-job person) x))
                    (:old-job-name x (setf (person-old-job-name person) x))
                    (:casual-dialogue x (setf (person-casual-dialogue person) x)))
    person))
