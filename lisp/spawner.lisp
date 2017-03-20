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

(defun do-spawn (spawner) ; TODO Localize the spawning /////
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
