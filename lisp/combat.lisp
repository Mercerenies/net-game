(in-package #:net-game)

(defun do-attack (obj &key (target *player*) (atk (atk obj)))
  (setf (hp target) (- (hp target) atk))
  (when (<= (hp target) 0)
    (move-object target nil))) ; TODO Make sure death of *player* won't crash everything

(defmethod is-trivial ((act (eql 'attack)) (obj animal) preps)
  (typecase (getf preps 'with)
    ((eql fists) nil)
    (weapon nil)
    (t (call-next-method))))

(defmethod do-action ((act (eql 'attack)) (obj animal) preps)
  (typecase (getf preps 'with)
    (null (format t "Attack with what?~%"))
    ((eql fists)
     (do-attack *player* :target obj :atk 0.02)
     (format t "You punch the ~A.~%" (get-name obj)))
    (weapon
     ; TODO Weapon wieldiness
     (do-attack *player* :target obj :atk (weapon-damage (cdr (assoc 'with preps))))
     (format t "You attack the ~A.~%" (get-name obj)))
    (t (call-next-method))))
; ///// Animals need be no longer passive when attacked
