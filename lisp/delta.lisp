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
                         do (push (load-loc elem) *world*)))
             (:mod (loop for elem in value
                         do (delta-modify elem))))))

(defun delta-modify (dloc)
  (unless (eq (first dloc) 'location)
    (error "Flawed data - location"))
  (let ((loc (find (second dloc) *world* :key #'get-id)))
    (unless loc
      (error "Invalid ID in delta location"))
    (loop for arg = (cddr dloc) then (cddr arg)
          for key = (first arg)
          for value = (second arg)
          while arg
          do (case key
               (:remove-links (setf (location-exits loc)
                                    (remove value (location-exits loc))))
               (:add-links (setf (location-exits loc)
                                 (append value (location-exits loc))))
               (:add-contents (mapc #'(lambda (x) (load-object loc x)) value))))))

; Returns (values map creatures spawners quests)
; Directly modifies the game world; call at the appropriate time
(defun load-delta (&key (file *standard-input*))
  (destructuring-bind (delta-sym dmap creatures spawners quests) (with-scheme-notation (read file))
    (unless (eq delta-sym 'delta)
      (error "Flawed data - delta"))
    ; Map
    (delta-map dmap)
    ; Lists
    (setf *creatures*
          (append (load-with creatures #'load-creature 'creature-set) *creatures*))
    (setf *spawners*
          (append (load-with spawners #'load-spawner 'spawner-set) *spawners*))
    (loop initially (unless (eq (first quests) 'quest-set)
                      (error "Flawed data - quest-set"))
          for data in (rest quests)
          for quest = (apply #'load-quest data)
          do (add-quest quest))))
