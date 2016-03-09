(in-package #:net-game)

(defclass item (named located)
  ())

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
    (setf (weapon-wieldy wpn) (first stats))
    (setf (weapon-damage wpn) (second stats))
    wpn))

(defmethod do-action ((act (eql 'collect)) (obj item))
  (if (find obj (inventory *player*))
      (format t "You're already holding the ~A...~%" (get-name obj))
      (progn
        (format t "You pick up the ~A.~%" (get-name obj))
        (move-object obj nil)
        (push obj (inventory *player*)))))

(defmethod do-action ((act (eql 'drop)) (obj item))
  (if (find obj (inventory *player*))
      (progn
        (format t "You drop the ~A.~%" (get-name obj))
        (setf (inventory *player*) (remove obj (inventory *player*)))
        (move-object obj (get-loc *player*)))
      (format t "But you're not holding the ~A...~%" (get-name obj))))

(defmethod do-action ((act (eql 'examine)) (obj weapon))
  (format t "~A~@
             * Usability: ~D%~@
             * Damage: ~D%~%"
          (get-name obj)
          (floor (* (weapon-wieldy obj) 100))
          (floor (* (weapon-damage obj) 100))))
