(in-package #:net-game)

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
(defun do-spawn (spawner) ; TODO Not all spawners should spawn; only the ones near the player
  (unless (and (spawner-creature-instance spawner)
               (get-loc (spawner-creature-instance spawner)))
    (let ((loc (choose-spawn-point spawner))
          (inst (make-creature (find (spawner-creature spawner) *creatures*
                                     :key #'get-id))))
      (when inst
        (setf (spawner-creature-instance spawner) inst)
        (move-object inst loc)))))


