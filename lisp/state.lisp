(in-package #:net-game)

(defgeneric do-command (state arg))

(defgeneric mode-name (state))

(defgeneric mode-text (state))

(defmethod mode-name ((state t))
  (format nil "~S" state))

(defmethod mode-text ((state t))
  "")

(defmethod do-command ((state (eql 'global)) arg)
  (let* ((args (word-split arg))
         (args-str (format nil "~{~A~^ ~}" (rest args))))
    (when (or (not (listp args))
              (< (length args) 1))
      (format t "Invalid user command!~%")
      (return-from do-user nil))
    (case (read-from-string (first args))
      (go (user-go args-str))
      (help (user-help))
      (t (let ((obj (or
                     (find args-str (location-contents (get-loc *player*))
                           :key #'get-name :test #'string-equal)
                     (find args-str (inventory *player*)
                           :key #'get-name :test #'string-equal))))
           (do-action (read-from-string (first args)) (or obj args-str)))))))

(defun user-go (exit-name)
  (let ((match (find exit-name (location-exits (get-loc *player*))
                     :key #'(lambda (x) (let ((temp (find x *world*
                                                          :key #'get-id)))
                                          (and temp (location-short-name temp))))
                     :test #'string-equal)))
    (if match
        (let ((match1 (find match *world* :key #'get-id)))
          (format t "Going...~%")
          (move-object *player* match1))
        (format t "Can't go that way.~%"))))

(defun user-help ()
  (format t "Valid Player Actions:~@
             \"go <place>\" - Go to the area listed~@
             \"examine <object>\" - Take a closer look at the object~@
             \"use <object>\" - Interact with a tool or object~@
             \"activate <object>\" - Turn the object on if it is currently inactive~@
             \"collect <object>\" - Pick up the object in question~@
             \"drop <object>\" - Drop the object from your inventory~@
             \"help\" - Display this message~@
             \"quit\" - Exit the game~%"))

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
