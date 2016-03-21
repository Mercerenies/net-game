(in-package #:net-game)

(defgeneric do-action (act obj preps))

(defmethod do-action (act (obj t) preps)
  (declare (ignore act preps))
  (format t "Nothing happened...~%"))

(defmethod do-action ((act (eql 'examine)) (obj warp-point) preps)
  (declare (ignore preps))
  (format t "A teleport point that can be used to warp to different ~
             areas of the map. ~:[Must be activated before it can be ~
             used~;Is currently active.~]~%" (warp-active obj)))

(defmethod do-action ((act (eql 'activate)) (obj warp-point) preps)
  (declare (ignore preps))
  (if (warp-active obj)
      (format t "It's already active.~%")
      (progn (format t "The warp point activates!~%")
             (setf (warp-active obj) t)
             (push obj *warps*))))

(defmethod do-action ((act (eql 'use)) (obj warp-point) preps)
  (declare (ignore preps))
  (unless (warp-active obj)
    (do-action 'activate obj preps))
  (when (warp-active obj)
    (push 'warp *state*)))

(defgeneric is-trivial (act obj preps))

(defmethod is-trivial ((act symbol) obj preps)
  (declare (ignore obj preps))
  t)

(defmethod is-trivial ((act (eql 'examine)) obj preps)
  (declare (ignore obj preps))
  t)

(defmethod is-trivial ((act (eql 'drop)) obj preps)
  (declare (ignore obj preps))
  nil)

(defmethod is-trivial ((act (eql 'collect)) obj preps)
  (declare (ignore obj preps))
  nil)

(defmethod is-trivial ((act (eql 'help)) obj preps)
  (declare (ignore obj preps))
  t)

(defmethod is-trivial ((act (eql 'activate)) obj preps)
  (declare (ignore obj preps))
  nil)

(defmethod is-trivial ((act (eql 'go)) obj preps)
  (declare (ignore obj preps))
  nil)

(defmethod is-trivial ((act (eql 'use)) obj preps)
  (declare (ignore obj preps))
  nil)

(defmethod is-trivial ((act (eql 'quit)) obj preps)
  (declare (ignore obj preps))
  t)
