
(defgeneric do-action (act obj))

(defmethod do-action (act (obj t))
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
