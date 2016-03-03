(in-package #:net-game)

(defgeneric do-action (act obj))

(defmethod do-action (act (obj t))
  (declare (ignore act))
  (format t "Nothing happened...~%"))

(defmethod do-action ((act (eql 'examine)) (obj warp-point))
  (format t "A teleport point that can be used to warp to different ~
             areas of the map. ~:[Must be activated before it can be ~
             used~;Is currently active.~]~%" (warp-active obj)))

(defmethod do-action ((act (eql 'activate)) (obj warp-point))
  (if (warp-active obj)
      (format t "It's already active.~%")
      (progn (format t "The warp point activates!~%")
             (setf (warp-active obj) t)
             (push obj *warps*))))

(defmethod do-action ((act (eql 'use)) (obj warp-point))
  (unless (warp-active obj)
    (do-action 'activate obj))
  (when (warp-active obj)
    (push 'warp *state*)))

(defmethod do-action ((act (eql 'collect)) (obj item))
  (if (find obj (inventory *player*))
      (format t "You're already holding the ~A...~%" (get-name obj))
      (progn
        (format t "You pick up the ~A.~%" (get-name obj))
        (move-object obj nil)
        (push obj (inventory *player*)))))

(defmethod do-action ((act (eql 'drop)) (obj item))
  (if (find obj (inventory *player*))
      (progn
        (format t "You drop the ~A.~%" (get-name obj))
        (setf (inventory *player*) (remove obj (inventory *player*)))
        (move-object obj (get-loc *player*)))
      (format t "But you're not holding the ~A...~%" (get-name obj))))

(defmethod do-action ((act (eql 'examine)) (obj weapon))
  (format t "~A~@
             * Usability: ~D%~@
             * Damage: ~D%~%"
          (get-name obj)
          (floor (* (weapon-wieldy obj) 100))
          (floor (* (weapon-damage obj) 100))))
