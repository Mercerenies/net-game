(in-package #:net-game)

(defgeneric do-attack (obj target atk))

(defmethod do-attack ((obj t) (target damageable) atk)
  (subf (hp target) atk))

(defmethod do-action ((act (eql 'attack)) (obj animal) preps)
  (typecase (getf preps 'with)
    (null (format t "Attack with what?~%"))
    ((eql fists)
     (do-attack *player* obj 0.12)
     (format t "You punch the ~A.~%" (get-name obj)))
    (weapon
     ;; TODO Weapon wieldiness (or possibly change it to be a durability stat, not wieldiness)
     (do-attack *player* obj (weapon-damage (getf preps 'with)))
     (format t "You attack the ~A.~%" (get-name obj)))
    (t (call-next-method))))

(defmethod do-attack :after (obj (target moody) atk)
  (respond-to-attack target))
