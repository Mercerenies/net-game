(in-package #:net-game)

(defparameter *creatures* nil)

(defparameter *spawners* nil)

(defconstant +spawner-types+
  '(spawner))

(defclass spawner ()
  ((creature :accessor spawner-creature
             :initform nil
             :initarg :creature)
   (area :accessor spawner-area
         :initform nil
         :initarg :area
         :type list)
   (alive :accessor spawner-creature-instance
          :initform nil
          :type (or null creature))))

(defmethod load-object ((header (eql 'spawner)) data)
  (apply #'make-instance 'spawner (cdr data)))

(defgeneric choose-spawn-point (spawner))

(defmethod choose-spawn-point ((spawner spawner))
  (check-type *world* hash-table)
  (gethash (choose (spawner-area spawner)) *world*))

(defun do-spawn (spawner)
  (check-type *creatures* list)
  (unless (and (spawner-creature-instance spawner)
               (get-loc (spawner-creature-instance spawner)))
    (let ((loc (choose-spawn-point spawner))
          (inst (make-creature (find (spawner-creature spawner) *creatures*
                                     :key #'get-id))))
      (when inst
        (setf (spawner-creature-instance spawner) inst)
        (move-object inst loc)))))

(defun active-spawner-set (&key (player-object *player*) (radius +active-radius+))
  (loop with nearby = (mapcar #'get-id
                              (remove-if #'(lambda (x) (check-flag 'civilized x))
                                         (halo (get-loc player-object) radius)))
        for spawner in *spawners*
        when (not (null (intersection nearby (spawner-area spawner))))
            collect spawner))

;; This function is temporary and acts as a shim while moving the old spawner interface over to the new one
(defun do-neo-spawner-migration ()
  (check-type *world* hash-table)
  (check-type *spawners* list)
  (loop for sp in *spawners*
        do (loop for loc in (spawner-area sp)
                 for inst = (make-instance 'neo-spawner
                                           :creature (spawner-creature sp)
                                           :time 5) ; TODO Pick this intelligently
                 do (move-object inst (gethash loc *world*))))
  (setf *spawners* nil))

(defclass neo-spawner (located hideable)
  ((creature :accessor neo-spawner-creature
             :initform nil
             :initarg :creature)
   (alive :accessor neo-spawner-creature-instance
          :initform nil
          :type (or null creature))
   (counter :accessor neo-spawner-counter
            :initarg :counter
            :initform 0
            :type integer)
   (time :accessor neo-spawner-time
         :initarg :time
         :initform 0
         :type integer))
  (:default-initargs :hidden t))

; TODO Spawns waaaaaaaay too often compared to the old system
(defmethod entity-turn ((obj neo-spawner))
  (with-accessors ((creature neo-spawner-creature)
                   (instance neo-spawner-creature-instance)
                   (counter neo-spawner-counter)
                   (time neo-spawner-time)
                   (loc get-loc))
      obj
    (unless (and instance (get-loc instance))
      (if (<= counter 0)
          (let ((new-inst (make-creature (find creature *creatures* :key #'get-id))))
            (setf counter time)
            (setf instance new-inst)
            (move-object new-inst loc))
          (decf counter)))))

;; It's an invisible object; examination probably won't appear but still
(defmethod do-action ((act (eql 'examine)) (obj neo-spawner) preps)
  (declare (ignore preps))
  (format t "A spawner that creates creatures."))

;; TODO System keys for neo-spawner
