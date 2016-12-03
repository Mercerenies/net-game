(in-package #:net-game)

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

(defun load-loc (loc)
  (let ((inst (make-location nil "")))
    (load-formatted loc 'location
                    (id (setf (get-id inst) id))
                    (name (setf (get-name inst) name)
                          (setf (location-short-name inst) name))
                    (:country country (setf (get-name inst) (format nil "~A, ~A"
                                                                    (get-name inst)
                                                                    country)))
                    (:links links (setf (location-exits inst) links))
                    (:contents contents (mapc #'(lambda (x) (load-object inst x)) contents))
                    (:civilized civilized (when civilized
                                            (add-flag 'civilized inst)))
                    (:water water (case water
                                    ((nil))
                                    ((sea) (add-flag 'sea inst))
                                    ((shore) (add-flag 'shore inst))))
                    (:meta meta)) ; Ignored
    inst))

(defun load-with (data func header)
  (let ((list nil))
    (load-formatted data header
                    ((extra) (push (apply func extra) list)))
    list))

; Returns (values map creatures spawners quests kb)
(defun load-data (&key (file *standard-input*))
  (let ((data (with-scheme-notation (read file))))
    ; Note that the sixth element of data is meta and is intentionally ignored by this segment of the program
    (unless (eq (first data) 'alpha)
      (error "Flawed data - alpha"))
    (let ((*world* (destructuring-bind (map-sym . locs) (second data)
                     (unless (eq map-sym 'map) (error "Flawed data - map"))
                     (loop with hash = (make-hash-table)
                           for loc in locs
                           for loc-node = (load-loc loc)
                           do (setf (gethash (get-id loc-node) hash)
                                    loc-node)
                           finally (return hash)))))
      (values
       *world*
       (destructuring-bind anims (third data)
         (load-with anims #'load-creature 'creature-set))
       (destructuring-bind spawners (fourth data)
         (load-with spawners #'load-spawner 'spawner-set))
       (destructuring-bind quests (fifth data)
         (let ((*quests* (make-hash-table)))
           (mapc #'add-quest (load-with quests #'load-quest 'quest-set))
           *quests*))
       (destructuring-bind (kb-sym . kb) (sixth data)
         (unless (eq kb-sym 'knowledge-base) (error "Flawed data - knowledge-base"))
         (load-knowledge-base kb))))))

(defun load-object (node obj)
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
