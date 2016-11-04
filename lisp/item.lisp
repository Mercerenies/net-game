(in-package #:net-game)

(defclass item (named located flagged)
  ((weight :accessor item-weight
           :initarg :weight
           :initform 1
           :type integer)))

(defun make-item (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'item :name name keys))

(defun item-match (match item)
  (typecase match
    (symbol (check-flag match item))
    (string (equalp match (get-name item)))
    (integer (= match (get-id item)))
    (t nil)))

(defclass weapon (item)
  ((type :accessor weapon-type
         :initarg :type
         :initform nil
         :type symbol)
   (mod :accessor weapon-mod
        :initarg :mod
        :initform nil
        :type symbol)
   (wieldy :accessor weapon-wieldy
           :initarg :wieldy
           :initform 0.0
           :type real)
   (damage :accessor weapon-damage
           :initarg :damage
           :initform 0.0
           :type real)))

(defun lookup-weapon-stats (type)
  (case type
    (club '(0.12 0.86))
    (hammer '(0.18 0.79))
    (spear '(0.21 0.78))
    (whip '(0.18 0.55)) ; TODO Make whips do something special since they are weaker
    (polearm '(0.22 0.51)) ; TODO Make polearms do something special too
    (missile '(0.65 0.90))
    (longsword '(0.55 0.53))
    (shortsword '(0.60 0.49))
    (sword '(0.58 0.50))
    (knife '(0.77 0.41))
    (sickle '(0.70 0.10)) ; TODO Sickles need something special too
    (t '(0.00 0.00))))

(defun thrown-weapon-p (weapon)
  (case (weapon-type weapon)
    (missile t)
    (t nil)))

(defun apply-modifiers (mod args)
  (destructuring-bind (wld dmg) args
    (case mod
      (unwieldy (list (- wld (random 0.10)) dmg))
      (wieldy   (list (+ wld (random 0.10)) dmg))
      (weak     (list wld (- dmg (random 0.10))))
      (strong   (list wld (+ dmg (random 0.10))))
      (t        (list wld dmg                  )))))

(defun make-weapon (name type mod)
  (let ((wpn (make-instance 'weapon :name name :type type :mod mod))
        (stats (apply-modifiers mod (lookup-weapon-stats type))))
    (setf (item-weight wpn) (floor (* 30 (- 1 (first stats)))))
    (setf (weapon-wieldy wpn) (first stats))
    (setf (weapon-damage wpn) (second stats))
    wpn))

(defmethod do-action ((act (eql 'collect)) (obj item) preps)
  (declare (ignore preps))
  (cond
    ((has-item obj *player*)
     (format t "You're already holding the ~A...~%" (get-name obj)))
    ((can-carry-item obj *player*)
     (format t "You pick up the ~A.~%" (get-name obj))
     (move-object obj nil)
     (add-item obj *player*))
    (t
     (format t "You're already carrying too much.~%"))))

(defmethod do-action ((act (eql 'drop)) (obj item) preps)
  (declare (ignore preps))
  (if (has-item obj *player*)
      (progn
        (format t "You drop the ~A.~%" (get-name obj))
        (remove-item obj *player*)
        (move-object obj (get-loc *player*)))
      (format t "But you're not holding the ~A...~%" (get-name obj))))

; TODO More user-friendly text
(defmethod do-action ((act (eql 'examine)) (obj item) preps)
  (declare (ignore preps))
  (format t "An item weighing about ~A units.~%"
          (item-weight obj)))

(defmethod system-keys append ((obj item))
  `((item-weight "Weight" ,(item-weight obj))))

; TODO More user-friendly text
(defmethod do-action ((act (eql 'examine)) (obj weapon) preps)
  (declare (ignore preps))
  (format t "A weapon weighing about ~A units.~%"
          (item-weight obj)))

(defmethod system-keys append ((obj weapon))
  `((weapon-wieldy "Wieldiness" ,(* 100 (weapon-wieldy obj)))
    (weapon-damage "Damage" ,(* 100 (weapon-damage obj)))
    (weapon-mod "Modifier" ,(weapon-mod obj)))) ; TODO Make weapon-mod be an item flag, possibly?
