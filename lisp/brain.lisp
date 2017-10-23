(in-package #:net-game)

(defgeneric is-desirable-square (obj loc))

(defmethod is-desirable-square ((obj located) (loc location))
  t)

(defgeneric wander (obj))

(defmethod wander ((obj located))
  "Moves the entity randomly, to a desirable square, or possibly not at all.
   Returns truthy if a move was performed."
  (let* ((nearby (remove-if #'(lambda (x) (not (is-desirable-square obj x)))
                            (halo (get-loc obj) 1)))
         (going (choose nearby)))
    (when (and going
               (not (eql (get-loc obj) going)))
      (move-object obj going)
      t)))

(defun simple-stalk (obj target)
  "Moves the object to a square adjacent to the target, if such a valid move exists.
   Returns truthy if successful."
  (let* ((candidates (intersection (halo (get-loc obj) 1) (halo (get-loc target) 1)))
         (path (find-if #'(lambda (x) (is-desirable-square obj x)) candidates)))
    (when path
      (move-object obj path)
      t)))

(defun adjacent-pursue (obj target)
  "If adjacent to the target, pursue the target directly. Returns truthy if the move was performed."
  (when (and (member-if (lambda (loc1) (member target (location-contents loc1)))
                        (halo (get-loc obj) 1))
             (is-desirable-square obj (get-loc target)))
    (move-object obj (get-loc target))
    t))

(defgeneric respond-to-attack (target))

(defmethod respond-to-attack ((target moody))
  (setf (get-mood target) 'hunting))

(defgeneric mood-check-impl (obj attitude mood)
  (:method-combination progn))

(defmethod mood-check-impl progn (obj (attitude t) (mood t))
  nil)

(defmethod mood-check-impl progn (obj (attitude (eql 'hunting)) mood)
  (when (member *player* (location-contents (get-loc obj)))
    (setf (get-mood obj) 'hunting)))

(defmethod mood-check-impl progn (obj (attitude (eql 'stalking)) (mood (eql 'passive)))
  (when (member *player* (location-contents (get-loc obj)))
    (setf (get-mood obj) 'sneaky)))

(defmethod mood-check-impl progn (obj attitude (mood (eql 'stalking)))
  (when (member *player* (location-contents (get-loc obj)))
    (setf (get-mood obj) 'hunting)))

(defmethod mood-check-impl progn (obj attitude (mood (eql 'sneaky)))
  (unless (member *player* (location-contents (get-loc obj)))
    (setf (get-mood obj) 'stalking)))

(defun mood-check (obj)
  (check-type obj moody)
  (mood-check-impl obj (get-attitude obj) (get-mood obj)))

(defgeneric mood-action-impl (obj attitude mood))

(defmethod mood-action-impl (obj attitude mood)
  nil)

(defmethod mood-action-impl (obj attitude (mood (eql 'passive)))
  (wander obj))

(defmethod mood-action-impl (obj attitude (mood (eql 'hunting)))
  (if (or (member *player* (location-contents (get-loc obj)))
          (adjacent-pursue obj *player*))
      (progn
        (format t "The ~A attacks.~%" (get-name obj))
        (do-attack obj *player* (atk obj)))
      (wander obj)))

(defmethod mood-action-impl (obj attitude (mood (eql 'stalking)))
  (unless (member *player* (location-contents (get-loc obj)))
    (simple-stalk obj *player*)))

(defmethod mood-action-impl (obj attitude (mood (eql 'sneaky)))
  nil)

(defun mood-action (obj)
  (check-type obj moody)
  (mood-action-impl obj (get-attitude obj) (get-mood obj)))

;; N.B.: This function is linear in its first keyword argument (steps)
;; but exponential in the second (step-size).
(defun find-somewhere-nearby-binom (loc &key (steps 5) (step-size 2))
  (loop for i from 0 to steps
        for curr = loc then (choose near)
        for near = (halo curr step-size)
        finally (return curr)))

(defun find-somewhere-nearby (loc &key (range 10))
  (choose (halo loc range)))
