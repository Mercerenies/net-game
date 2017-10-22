(in-package #:net-game)

(defun do-attack (obj &key (target *player*) (atk (atk obj)))
  (subf (hp target) atk))

(defmethod do-action ((act (eql 'attack)) (obj animal) preps)
  (typecase (getf preps 'with)
    (null (format t "Attack with what?~%"))
    ((eql fists)
     (do-attack *player* :target obj :atk 0.12)
     (respond-to-attack obj)
     (format t "You punch the ~A.~%" (get-name obj)))
    (weapon
     ;; TODO Weapon wieldiness (or possibly change it to be a durability stat, not wieldiness)
     (do-attack *player* :target obj :atk (weapon-damage (getf preps 'with)))
     (respond-to-attack obj)
     (format t "You attack the ~A.~%" (get-name obj)))
    (t (call-next-method))))
