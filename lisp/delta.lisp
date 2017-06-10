(in-package #:net-game)

;; TODO delta-map and delta-modify should both be delta-load-object with load-formatted inside.

;; Modifies *world*
(defun delta-map (map)
  (unless (eq (first map) 'map)
    (error "Flawed data - map"))
  (loop for arg = (rest map) then (cddr arg)
        for key = (first arg)
        for value = (second arg)
        while arg
        do (case key
             (:new (loop for elem in value
                         for loc = (load-object 'location elem)
                         do (setf (gethash (get-id loc) *world*) loc)))
             (:mod (loop for elem in value
                         do (delta-modify elem))))))

(defun delta-modify (dloc)
  (unless (eq (first dloc) 'location)
    (error "Flawed data - location"))
  (let ((loc (gethash (second dloc) *world*)))
    (unless loc
      (error "Invalid ID ~D in delta location" (second dloc)))
    (loop for arg = (cddr dloc) then (cddr arg)
          for key = (first arg)
          for value = (second arg)
          while arg
          do (case key
               (:remove-links (setf (location-exits loc)
                                    (remove value (location-exits loc))))
               (:add-links (setf (location-exits loc)
                                 (append value (location-exits loc))))
               (:add-contents (mapc #'(lambda (x) (load-then-position x loc)) value))))))

(defgeneric delta-load-object (header data)
  (:documentation "Like load-object, delta-load-object validates that the data matches the header and
                   then loads the data. However, unlike load-object, delta-load-object is allowed to,
                   and in fact is expected to, carry side effects, as it should integrate the delta
                   changes into the game world. Note that delta-load-object should only be used for
                   pre-existing objects which are being modified with new data. Even in the delta
                   reintegration stage, new objects which are being added to the world should be
                   loaded with load-object."))

;; Directly modifies the game world; call at the appropriate time
(defun load-and-integrate-delta (&key (file *standard-input*))
  ;; TODO load-formatted
  (destructuring-bind (delta-sym key dmap creatures spawners quests kb pool reqs)
      (with-scheme-notation (read file))
    (unless (eq delta-sym 'delta)
      (error "Flawed data - delta"))
    (unless (= key (1+ *key*)) ; Wrong key
      (return-from load-and-integrate-delta nil))
    ;; Report the integration
    (echo 1 "Integrating... (key: ~D)" key)
    ;; Key
    (setf *key* key)
    ;; Map
    (delta-map dmap)
    ;; Lists
    (setf *creatures*
          (append (load-with creatures
                             (whitelisted-load-1 #'load-object +creature-types+)
                             'creature-set)
                  *creatures*))
    (setf *spawners*
          (append (load-with spawners
                             (whitelisted-load-1 #'load-object +spawner-types+)
                             'spawner-set)
                  *spawners*))
    (mapc #'add-quest (load-with quests
                                 (whitelisted-load-1 #'load-object +quest-types+)
                                 'quest-set))
    (delta-load-object 'knowledge-base kb)
    ;; Neo Spawner Migration
    (do-neo-spawner-migration)
    ;; Object pool
    (pool-add-list (load-with pool
                              (whitelisted-load-1 #'load-object +map-object-types+)
                              'pool))
    ;; Request list
    (load-object 'request-set reqs) ; ////
    t))
