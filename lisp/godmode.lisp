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
  (let ((padding 8)
        (keys (system-keys obj))
        (top-line (format nil " ~:[Object~;~:*~A~]~@[ (~A)~] "
                          (and (typep obj 'named) (get-name obj))
                          (and (typep obj 'identifiable) (get-id obj)))))
    (destructuring-bind (sym-length name-length val-length)
        (loop for (sym name val) in keys
              maximizing (length (string sym)) into sym-length
              maximizing (length name) into name-length
              maximizing (length (format nil "~A" val)) into val-length
              finally (return (list sym-length name-length val-length)))
      (let* ((total-length (max (+ sym-length name-length val-length padding)
                                (length top-line)))
             (name-length-new (- total-length ; Stretch name-length to fit total-length
                                 sym-length
                                 val-length
                                 padding))
             (new-keys (mapcan (lambda (x)
                                 (list sym-length (first x)
                                       name-length-new (second x)
                                       val-length (third x)))
                               keys)))
        (format t "+~V@{-~}+~:*~@
                   |~VA|~@
                   +~0@*~V@{-~}+~*~@
                   ~{| ~*~:[~2:*~V@{ ~}~*~;~2:*~VA~] | ~VA | ~V@A |~%~}~
                   +~0@*~V@{-~}+~%"
                total-length top-line new-keys)))))

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
