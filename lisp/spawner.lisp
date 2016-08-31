(in-package #:net-game)

(defparameter *creatures* nil)

(defparameter *spawners* nil)

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

(defgeneric load-spawner (type &rest data))

(defmethod load-spawner ((type (eql 'spawner)) &rest data)
  (apply #'make-instance 'spawner data))

(defgeneric choose-spawn-point (spawner))

; Uses *world*
(defmethod choose-spawn-point ((spawner spawner))
  (find (choose (spawner-area spawner)) *world* :key #'get-id))

; Uses *creatures*
(defun do-spawn (spawner) ; TODO Localize the spawning
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
                              (remove-if #'location-civilized
                                         (halo (get-loc player-object) radius)))
        for spawner in *spawners*
        when (not (null (intersection nearby (spawner-area spawner))))
            collect spawner))
