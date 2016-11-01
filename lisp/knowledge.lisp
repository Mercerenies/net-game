(in-package #:net-game)

(defparameter *knowledge-base* nil)

(defstruct (human-knowledge (:conc-name know-))
  quests)

(defun delta-load-knowledge-base (data)
  (unless (eq (car data) 'knowledge-base) (error "Flawed data - knowledge-base"))
  (loop for arg = (cdr data) then (cddr arg)
        for key = (first arg)
        for value = (second arg)
        while arg
        do (case key
             (:new (loop for dd = value then (cddr dd)
                         while dd
                         do (setf (gethash (first dd) *knowledge-base*) (load-brain (second dd)))))
             (:mod (loop for dd = value then (cddr dd)
                         for dkey = (first dd)
                         for dval = (second dd)
                         while dd
                         unless (eq (car dval) 'npc-brain) do (error "flawed data - npc-brain")
                         do (let ((knowledge (gethash dkey *knowledge-base*)))
                              (setf (know-quests knowledge)
                                    (append (know-quests knowledge) (cdr dval) nil))))))))

(defun load-brain (data)
  (unless (eq (car data) 'npc-brain) (error "Flawed data - npc-brain"))
  (let* ((args (cdr data))
         (quests (getf args :quests)))
    (make-human-knowledge :quests quests)))

(defun load-knowledge-base (data)
  (loop with hash = (make-hash-table)
        for dd = data then (cddr dd)
        for key = (first dd)
        for value = (second dd)
        while dd
        do (setf (gethash key hash) (load-brain value))
        finally (return hash)))

(defun get-quest-list (id)
  (know-quests (gethash id *knowledge-base*)))
