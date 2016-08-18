(in-package #:net-game)

(defparameter *god-mode* nil)

(defmethod do-action ((act (eql 'nuke)) (obj damageable) preps)
  (declare (ignore preps))
  (format t "Nuking the ~A... you monster.~%"
          (if (typep obj 'named)
              (get-name obj)
              "object"))
  (setf (hp obj) 0))

(defmethod do-action ((act (eql 'probe)) obj preps)
  (declare (ignore preps))
  (format t "~:[Object~;~:*~A~]~@[ (~A)~]~@
             ~{ * ~{~*~A~2:* (~A) - ~*~A~}~%~}"
          (and (typep obj 'named) (get-name obj))
          (and (typep obj 'identifiable) (get-id obj))
          (system-keys obj)))
