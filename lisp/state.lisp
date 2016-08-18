(in-package #:net-game)

(defconstant +active-radius+
  5)

(defgeneric do-command (state arg))

(defgeneric mode-name (state))

(defgeneric mode-text (state))

(defmethod mode-name ((state t))
  (format nil "~S" state))

(defmethod mode-text ((state t))
  "")

(defmethod do-command ((state (eql 'global)) arg)
  (let ((parse (enhanced-parse arg)))
    (if parse
        (do-action-safely (sentence-verb parse) (sentence-noun parse) (sentence-preps parse))
        (do-action-safely nil nil nil))
    (unless (or (null parse) (is-trivial (sentence-verb parse)
                                         (sentence-noun parse)
                                         (sentence-preps parse)))
      (loop with halo = (halo (get-loc *player*) +active-radius+)
            for loc in halo
            do (mapc #'entity-turn (location-contents loc)))
      (loop for spawner in *spawners*
            do (do-spawn spawner)))))

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
             ~]~
             \"help\" - Display this message~@
             \"quit\" - Exit the game~%"
          *god-mode*))

(defmethod mode-text ((state (eql 'warp)))
  (let ((nums (loop for e in *warps*
                    for i upfrom 1
                    collect i)))
    (format nil "Choose a warp point:~%~{~A. ~A~%~}0. Cancel~%"
            (mapcan (lambda (x y) (list x (location-short-name (get-loc y))))
                    nums *warps*))))

(defmethod do-command ((state (eql 'warp)) arg)
  (let* ((int (parse-integer arg :junk-allowed t))
         (match (find arg *warps*
                      :key (lambda (x) (location-short-name (get-loc x)))
                      :test #'string-equal))
         (opt (cond ((and int (zerop int)) 'cancel)
                    ((and int
                          (typep int
                                 `(integer 1 ,(length *warps*))))(nth (1- int) *warps*))
                    (match match)
                    ((string-equal arg "cancel") 'cancel)
                    (t nil))))
    (etypecase opt
      (null (format t "Invalid warp point.~%"))
      ((eql cancel) (pop *state*))
      (warp-point (format t "Warping!~%")
                  (move-object *player* (get-loc opt))
                  (pop *state*)))))
