(in-package #:net-game)

(defun do-action-safely (act obj preps)
  (if (and (is-admin-only act obj preps)
           (not *god-mode*))
      (format t "You don't have permission to do that.~%")
    (do-action act obj preps)))

(defgeneric do-action (act obj preps))

(defmethod do-action (act (obj t) preps)
  (declare (ignore act preps))
  (format t "Nothing happened...~%"))

(defmethod do-action ((act (eql 'examine)) (obj warp-point) preps)
  (declare (ignore preps))
  (format t "A teleport point that can be used to warp to different ~
             areas of the map. ~:[Must be activated before it can be ~
             used~;Is currently active.~]~%" (warp-active obj)))

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

;(defmethod do-action :before (act obj preps)
;  (format t " >>> ~A ~A ~A <<< " act obj preps))

(defmethod do-action ((act (eql 'go)) (obj location) preps)
  (declare (ignore preps))
  (if (eq obj (get-loc *player*))
      (format t "You're already there...~%")
      (progn
        (format t "Going...~%")
        (move-object *player* obj))))

(defmethod do-action ((act (eql 'help)) obj preps)
  (declare (ignore obj preps))
  (format t "Valid Player Actions:~@
             \"go <place>\" - Go to the area listed~@
             \"examine <object>\" - Take a closer look at the object~@
             \"use <object>\" - Interact with a tool or object~@
             \"activate <object>\" - Turn the object on if it is currently inactive~@
             \"collect <object>\" - Pick up the object in question~@
             \"drop <object>\" - Drop the object from your inventory~@
             \"attack <object> with <object>\" - Attack the entity with a weapon~@
             \"attack <object> with fists\" - Attack the entity unarmed~@
             \"eat <object>\" - Eat the object~@
             ~:[~;~
             \"probe <object>\" - (GOD) Get developer specs on an entity or object~@
             \"nuke <object>\" - (GOD) Insta-kill any entity which has health~@
             \"summon <object>\" - (GOD) Force an object into existence at the current location~@
             ~]~
             \"help\" - Display this message~@
             \"quit\" - Exit the game~%"
          *god-mode*))

(defconstant +trivial-actions+
  '(probe examine help quit))

(defconstant +admin-actions+
  '(probe nuke summon))

(defgeneric is-admin-only (act obj preps))

(defmethod is-admin-only ((act symbol) obj preps)
  (declare (ignore obj preps))
  (member act +admin-actions+))

(defgeneric is-trivial (act obj preps))

(defmethod is-trivial ((act symbol) obj preps)
  (declare (ignore obj preps))
  (member act +trivial-actions+))
