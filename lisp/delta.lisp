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
                         for loc = (load-loc elem)
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
               (:add-contents (mapc #'(lambda (x) (load-object loc x)) value))))))

; Returns (values map creatures spawners quests kb)
; Directly modifies the game world; call at the appropriate time
(defun load-delta (&key (file *standard-input*))
  (destructuring-bind (delta-sym dmap creatures spawners quests kb) (with-scheme-notation (read file))
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
          do (add-quest quest))
    (delta-load-knowledge-base kb)))
