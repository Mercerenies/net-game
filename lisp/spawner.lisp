(in-package #:net-game)

;; TODO Getting lots of duplicates in here for some odd reason...
(defparameter *creatures* nil)

(defparameter *spawners* nil)

(defconstant +spawner-types+
  '(global-spawner))

(defclass global-spawner ()
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

;; TODO Change the header in Ruby to say global-spawner
(defmethod load-object ((header (eql 'spawner)) data)
  (apply #'make-instance 'global-spawner (cdr data)))

(defgeneric choose-spawn-point (spawner))

(defmethod choose-spawn-point ((spawner global-spawner))
  (check-type *world* hash-table)
  (gethash (choose (spawner-area spawner)) *world*))

(defun do-spawn (spawner)
  (check-type *creatures* list)
  (unless (and (spawner-creature-instance spawner)
               (get-loc (spawner-creature-instance spawner)))
    (let ((loc (choose-spawn-point spawner))
          (inst (make-creature (find-by-id (spawner-creature spawner) *creatures*))))
      (when inst
        (setf (spawner-creature-instance spawner) inst)
        (move-object inst loc)))))

(defun active-spawner-set (&key (player-object *player*) (radius +active-radius+))
  (let ((global (loop with nearby = (mapcar #'get-id
                                            (remove-if #'(lambda (x) (check-flag 'civilized x))
                                                       (halo (get-loc player-object) radius)))
                      for spawner in *spawners*
                      when (not (null (intersection nearby (spawner-area spawner))))
                          collect spawner))
        (local (loop for loc in (halo (get-loc player-object) radius)
                     append (remove-if-not (lambda (x) (typep x 'neo-spawner))
                                           (location-contents loc)))))
    (append global local)))

;; This function is temporary and acts as a shim while moving the old spawner interface over to the
;; new one
(defun do-neo-spawner-migration ()
  (check-type *world* hash-table)
  (check-type *spawners* list)
  (loop for sp in *spawners*
        do (loop for loc in (spawner-area sp)
                 for inst = (make-instance 'neo-spawner
                                           :creature (spawner-creature sp)
                                           :time 5
                                           :counter 5) ; TODO Pick time/counter intelligently
                 do (move-object inst (gethash loc *world*))
                 do (warn 'net-game-warning
                          :level 1
                          :text "Old-style spawner detected...")))
  (setf *spawners* nil))

;; TODO It should be safe to rename this to simply "spawner" soon.
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

(defmethod load-object ((header (eql 'neo-spawner)) data)
  (let ((inst (make-instance 'neo-spawner :time 5 :counter 5)))
    (load-formatted data 'neo-spawner
                    (creature (setf (neo-spawner-creature inst) creature))
                    (:time time
                           (setf (neo-spawner-time inst) time)
                           (setf (neo-spawner-counter inst) time)))
    inst))

;; TODO Spawns waaaaaaaay too often compared to the old system
(defmethod entity-turn ((obj neo-spawner))
  (with-accessors ((creature neo-spawner-creature)
                   (instance neo-spawner-creature-instance)
                   (counter neo-spawner-counter)
                   (time neo-spawner-time)
                   (loc get-loc))
      obj
    (unless (and instance (get-loc instance))
      (if (<= counter 0)
          (let ((new-inst (make-creature (find-by-id creature *creatures*))))
            (setf counter time)
            (setf instance new-inst)
            (move-object new-inst loc))
          (decf counter)))))

;; It's an invisible object; examination probably won't appear but still
(defmethod do-action ((act (eql 'examine)) (obj neo-spawner) preps)
  (declare (ignore preps))
  (format t "A spawner that creates creatures."))

;; TODO System keys for neo-spawner
