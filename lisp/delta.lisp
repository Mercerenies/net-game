(in-package #:net-game)

; Modifies *world*
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
               (:add-contents (mapc #'(lambda (x) (load-map-object loc x)) value))))))

(defgeneric delta-load-object (header data))

; Directly modifies the game world; call at the appropriate time
(defun load-and-integrate-delta (&key (file *standard-input*))
  (destructuring-bind (delta-sym dmap creatures spawners quests kb) (with-scheme-notation (read file))
    (unless (eq delta-sym 'delta)
      (error "Flawed data - delta"))
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
    (delta-load-object 'knowledge-base kb)))
