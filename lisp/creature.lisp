(in-package #:net-game)

(defconstant +creature-types+
  '(animal monster))

;; ///// Monster data into *creatures*

(defclass animal-data (identifiable named loaded)
  ((pack :accessor anim-pack
         :initform 1
         :initarg :pack)
   (speed :accessor anim-speed
          :initform 1
          :initarg :speed)
   (threat :accessor anim-threat
           :initform 1
           :initarg :threat)
   (air :accessor anim-air
        :initform nil
        :initarg :air)
   (sea :accessor anim-sea
        :initform nil
        :initarg :sea)
   (size :accessor anim-size
         :initform 1
         :initarg :size)
   (meat-type :accessor anim-meat-type
              :initform nil
              :initarg :meat-type)))

(defmethod load-object ((header (eql 'animal)) data)
  (let ((inst (make-instance 'animal-data)))
    (load-formatted data 'animal
                    (id (setf (get-id inst) id))
                    (name (setf (get-name inst) name))
                    (:pack pack (setf (anim-pack inst) pack))
                    (:speed speed (setf (anim-speed inst) speed))
                    (:threat threat (setf (anim-threat inst) threat))
                    (:size size (setf (anim-size inst) size))
                    (:air air (setf (anim-air inst) air))
                    (:sea sea (setf (anim-sea inst) sea))
                    (:meat-type meat (setf (anim-meat-type inst) (apply #'make-food-data (cdr meat)))))
    inst))

(defclass monster-data (identifiable named loaded)
  ((affinity :accessor mon-affinity
             :initform 'neutral
             :initarg :affinity)
   (chaos :accessor mon-chaos
          :initform 'neutral
          :initarg :chaos)
   (type :accessor mon-type
         :initform nil
         :initarg :type)
   (type-name :accessor mon-type-name
              :initform nil
              :initarg :type-name)))

(defun make-monster-data (id name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'monster-data :id id :name name keys))

(defmethod load-object ((header (eql 'monster)) data)
   (destructuring-bind (mon-sym id name . rest) data
     (apply #'make-monster-data id name rest)))

(defgeneric make-creature (data))

(defmethod make-creature ((data animal-data))
  (make-animal data))

(defclass creature (named located)
  ())

#|
 | Moods
 | * Passive - Moves randomly, if approached by a player, behaves according to attitude.
 | * Hunting - Chases the player. If the player is too far, moves randomly.
 | * Stalking - Chases the player at a distance. Moves in to Hunting eventually.
 | * Sneaky - Preparing to stalk when the player leaves.
 | Attitudes
 | * Passive - Does not strike unless struck.
 | * Hunting - Moves to Hunting mood when a player is spotted.
 | * Stalking - Moves to Sneaky mood when a player moves away.
 |#
(defclass animal (creature damageable loaded moody attacking)
  ((data :accessor anim-data
         :initform nil
         :initarg :data)
   (speed :accessor anim-speed
          :initform 1
          :initarg :speed)
   (air :accessor anim-air
        :initform nil
        :initarg :air)
   (sea :accessor anim-sea
        :initform nil
        :initarg :sea)
   (drops :accessor anim-drops
          :initform nil)))

(defmethod entity-spoils append ((obj animal))
  (anim-drops obj))

(defun handle-animal-drops (anim)
  (setf (anim-drops anim)
        (loop for i from 1 to (anim-size (anim-data anim)) ; TODO Should not be a 1-1 relationship here; need to drop less meat
              collect (make-food (anim-meat-type (anim-data anim))))))

(defun make-animal (data)
  (let ((anim (make-instance 'animal
                             :data data
                             :name (get-name data)
                             :speed (anim-speed data)
                             :mood 'passive
                             :attitude (case (anim-threat data)
                                         (1 'passive)
                                         (2 (choose '(passive passive passive hunting)))
                                         (3 (choose '(passive hunting stalking stalking)))
                                         (4 (choose '(hunting hunting stalking stalking)))
                                         (5 (choose '(hunting hunting hunting stalking)))
                                         (t 'passive))
                             :hp (case (anim-size data)
                                   (1 (random-range 0.20 0.40))
                                   (2 (random-range 0.25 0.59))
                                   (3 (random-range 0.45 0.91))
                                   (4 (random-range 0.51 1.10))
                                   (5 (random-range 1.00 1.88))
                                   (t 0.10))
                             :atk (case (anim-size data)
                                    (1 (random-range 0.04 0.05))
                                    (2 (random-range 0.10 0.15))
                                    (3 (random-range 0.10 0.24))
                                    (4 (random-range 0.15 0.34))
                                    (5 (random-range 0.30 0.59))
                                    (t 0.01))
                             :sea (anim-sea data)
                             :air (anim-air data))))
    (prog1 anim
      (handle-animal-drops anim))))

(defclass monster (creature damageable loaded moody)
  ((data :accessor mon-id
         :initform nil
         :initarg :id)))

(defun mon-data (mon)
  (check-type *creatures* list)
  (find-by-id (mon-id mon) *creatures*))

(defun make-monster (data) ; The data must be in *creatures*
  (make-instance 'monster
                 :id (get-id data)))

(defmethod load-object ((header (eql 'monster-instance)) data)
  (let ((inst (make-instance 'monster)))
    (load-formatted data 'monster-instance
                    (id (setf (mon-id inst) id)))
    inst))

(defmethod get-origin ((obj animal))
  (get-origin (anim-data obj)))

(defmethod entity-turn ((obj animal)) ; ///// Test heavily
  ;;(format t "The ~A (~A / ~A) at ~A is going to go now.~%"
  ;;        (get-name obj) (get-mood obj) (get-attitude obj) (get-name (get-loc obj)))
  (mood-check obj)
  (mood-action obj)
  (mood-check obj))

(defun entity-turn-legacy (obj)
  ;;(format t "The ~A (~A / ~A) at ~A is going to go now.~%"
  ;;        (get-name obj) (get-mood obj) (get-attitude obj) (get-name (get-loc obj)))
  (if (not (is-desirable-square obj (get-loc obj)))
      (move-object obj (choose (halo (get-loc obj) 1)))
      (case (get-mood obj)
        (passive (cond
                   ((member *player* (location-contents (get-loc obj)))
                    (case (get-attitude obj)
                      (passive nil)
                      (hunting (setf (get-mood obj) 'hunting)
                               (entity-turn obj))
                      (stalking (setf (get-mood obj) 'sneaky))))
                   ((<= (random 6) (anim-speed obj))
                    (wander obj)
                    (when (and (eql (get-attitude obj) 'stalking)
                               (member *player* (location-contents (get-loc obj))))
                      (setf (get-mood obj) 'sneaky)))
                   (t nil)))
        (sneaky (cond
                  ((member *player* (location-contents (get-loc obj))) nil)
                  ((some (lambda (x) (member *player* (location-contents x)))
                         (halo (get-loc obj) 1))
                   (setf (get-mood obj) 'stalking))
                  (t (setf (get-mood obj) 'passive))))
        (hunting (cond
                   ((member *player* (location-contents (get-loc obj)))
                    (format t "The ~A attacks.~%" (get-name obj))
                    (do-attack obj *player* (atk obj)))
                   ((adjacent-pursue obj *player*)
                    (entity-turn obj))
                   ((<= (random 6) (anim-speed obj))
                    (wander obj))
                   (t nil)))
        (stalking (cond
                    ((member *player* (location-contents (get-loc obj)))
                     (setf (get-mood obj) 'hunting)
                     (entity-turn obj))
                    ((some (lambda (x) (member *player* (location-contents x)))
                           (halo (get-loc obj) 1))
                     nil)
                    ((simple-stalk obj *player*))
                    (t (setf (get-mood obj) 'passive)))))))

(defmethod is-desirable-square ((obj animal) (loc location))
  (cond
    ;; If sea-based and location is a land tile, undesirable.
    ((and (anim-sea obj)
          (not (or (check-flag 'shore loc)
                   (check-flag 'sea loc)))) nil)
    ;; If passive mood, civilized tile, and not a passive bird, then undesirable.
    ((and (eql (get-mood obj) 'passive)
          (check-flag 'civilized loc)
          (not (and (anim-air obj)
                    (eql (get-attitude obj) 'passive)))) nil)
    ;; Otherwise, desirable.
    (t)))

(defmethod do-action ((act (eql 'examine)) (obj animal) preps)
  (declare (ignore preps))
  ;; TODO Better observation with a level up
  ;;      Ideally, the player would have an "observation" stat which allows him to better
  ;;      detect when an animal is stalking them
  (format t "A ~[~;tiny ~;small ~;~;large ~;enormous ~]~(~A~) ~
             ~[flying through the air ~;swimming in the sea ~:;wandering by ~]~
             ~[casually~:;glaring at you~].~%"
          (anim-size (anim-data obj))
          (get-name obj)
          (cond
            ((anim-air obj) 0)
            ((anim-sea obj) 1)
            (t 2))
          (case (get-attitude obj)
            ((passive sneaky stalking) 0)
            (t 1))))

(defmethod object-nature ((obj creature))
    'creature)

(defmethod system-keys append ((obj animal))
  `((get-mood "Current Mood" ,(get-mood obj))
    (get-attitude "Attitude" ,(get-attitude obj))
    (anim-air "Flying" ,(anim-air obj))
    (anim-sea "Swimming" ,(anim-sea obj))
    (anim-pack "Pack Mentality" ,(anim-pack (anim-data obj)))
    (anim-speed "Speed" ,(anim-speed (anim-data obj)))
    (anim-size "Size" ,(anim-size (anim-data obj)))
    (anim-threat "Threat" ,(anim-threat (anim-data obj)))
    (anim-meat-type "Meat" ,(get-name (anim-meat-type (anim-data obj))))))
