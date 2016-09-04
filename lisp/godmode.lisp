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

(defconstant +god-apple+
  (make-food-data "Golden Apple"
                  :full-name "Divine Fruit"
                  :plant-type 'tree
                  :nutritional-value 999.0
                  :poison-chance 0.0))

(defmethod do-action ((act (eql 'summon)) (obj symbol) preps)
  (declare (ignore preps))
  (let ((entity (case obj
                  (|GOLDEN APPLE| (make-food +god-apple+)))))
    (if entity
        (progn (format t "A ~A fell from the sky.~%" (get-name entity))
               (move-object entity (get-loc *player*)))
        (format t "Unrecognized entity. You can summon the following:~@
                   ~{ * ~A~%~}"
                '("Golden Apple")))))

(defmethod is-admin-only ((act (eql 'probe)) obj preps)
  (declare (ignore obj preps))
  t)

(defmethod is-admin-only ((act (eql 'nuke)) obj preps)
  (declare (ignore obj preps))
  t)

(defmethod is-admin-only ((act (eql 'summon)) obj preps)
  (declare (ignore obj preps))
  t)
