(in-package #:net-game)

(defun do-attack (obj &key (target *player*) (atk (atk obj)))
  (setf (hp target) (- (hp target) atk))
  (when (<= (hp target) 0)
    (move-object target nil)))
; TODO Proper death sequence; game crashes on player death, currently

(defmethod is-trivial ((act (eql 'attack)) (obj animal) preps)
  (typecase (getf preps 'with)
    ((eql fists) nil)
    (weapon nil)
    (t (call-next-method))))

(defmethod do-action ((act (eql 'attack)) (obj animal) preps)
  (typecase (getf preps 'with)
    (null (format t "Attack with what?~%"))
    ((eql fists)
     (do-attack *player* :target obj :atk 0.12)
     (when (eq (anim-mood obj) 'passive)
       (setf (anim-mood obj) 'hunting))
     (format t "You punch the ~A.~%" (get-name obj)))
    (weapon
     ; TODO Weapon wieldiness
     (do-attack *player* :target obj :atk (weapon-damage (cdr (assoc 'with preps))))
     (when (eq (anim-mood obj) 'passive)
       (setf (anim-mood obj) 'hunting))
     (format t "You attack the ~A.~%" (get-name obj)))
    (t (call-next-method))))
