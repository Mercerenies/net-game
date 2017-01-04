(in-package #:net-game)

(defparameter *warps* nil)

(defclass warp-point (named located loaded)
  ((active :accessor warp-active
           :initform nil
           :type boolean))
  (:default-initargs :name "Warp Point"))

(defun make-warp-point ()
  (make-instance 'warp-point))

(defmethod do-action ((act (eql 'examine)) (obj warp-point) preps)
  (declare (ignore preps))
  (format t "A teleport point that can be used to warp to different ~
             areas of the map. ~:[Must be activated before it can be ~
             used.~;Is currently active.~]~%" (warp-active obj)))

(defmethod object-nature ((obj warp-point))
    'entity)

(defmethod system-keys append ((obj warp-point))
  `((warp-active "Activated" ,(warp-active obj))))

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
    (do-action-safely 'activate obj preps))
  (when (warp-active obj)
    (push 'warp *state*)))

(defmethod mode-text ((state (eql 'warp)))
  (let ((nums (loop for e in *warps*
                    for i upfrom 1
                    collect i)))
    (format nil "Choose a warp point:~%~{~A. ~A~%~}0. Cancel~%"
            (mapcan (lambda (x y) (list x (location-short-name (get-loc y))))
                    nums *warps*))))

(defmethod do-command ((state (eql 'warp)) arg)
  (let* ((int (parse-integer arg :junk-allowed t))
         (match (find arg *warps*
                      :key (lambda (x) (location-short-name (get-loc x)))
                      :test #'string-equal))
         (opt (cond ((and int (zerop int)) 'cancel)
                    ((and int
                          (typep int
                                 `(integer 1 ,(length *warps*))))(nth (1- int) *warps*))
                    (match match)
                    ((string-equal arg "cancel") 'cancel)
                    (t nil))))
    (etypecase opt
      (null (format t "Invalid warp point.~%"))
      ((eql cancel) (pop *state*))
      (warp-point (format t "Warping!~%")
                  (move-object *player* (get-loc opt))
                  (pop *state*)))))
