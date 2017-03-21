(in-package #:net-game)

(defun do-action-safely (act obj preps)
  "Conditionally performs the action given. The arguments should be the same arguments that would
   be passed to do-action. If the player's permissions are sufficient to perform the action, then
   do-action will be called, forwarding the arguments exactly. Otherwise, a generic message will
   be displayed. In general, do-action-safely should be called in place of do-action unless there
   is a good reason to need to bypass standard security protocols."
  (if (and (is-admin-only act obj preps)
           (not *god-mode*))
      (format t "You don't have permission to do that.~%")
    (do-action act obj preps)))

(defgeneric do-action (act obj preps))

(defmethod do-action (act (obj t) preps)
  (declare (ignore act preps))
  (format t "Nothing happened...~%"))

;;(defmethod do-action :before (act obj preps)
;;  (format t " >>> ~A ~A ~A <<< " act obj preps))

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
             \"console\" - (GOD) Drop into the developer console to directly type Lisp code~@
             ~]~
             \"help\" - Display this message~@
             \"quit\" - Exit the game~%"
          *god-mode*))

(defconstant +trivial-actions+
  '(probe examine help quit))

(defconstant +admin-actions+
  '(probe nuke summon console))

(defgeneric is-admin-only (act obj preps))

(defmethod is-admin-only ((act symbol) obj preps)
  (declare (ignore obj preps))
  (member act +admin-actions+))

(defgeneric is-trivial (act obj preps))

(defmethod is-trivial ((act symbol) obj preps)
  (declare (ignore obj preps))
  (member act +trivial-actions+))
