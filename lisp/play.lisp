(in-package #:net-game)

; TODO Random seed
; TODO Put states in for interactive objects (something like the state design pattern)
; TODO People in the world

; ///// Everything so far works. Add people, I guess.

(defclass player (named located)
  ()
  (:default-initargs :name "Sandy"))

(defparameter *do-exit*
  (lambda () (error "Nothing to exit!")))

; TODO Put player in the location contents
(defparameter *player*
  (make-instance 'player))

(defparameter *world* nil)

(defparameter *state*
  (list 'global))

(defparameter *warps*
  nil)

(defun word-split (string &optional (token #\SPACE))
  (loop for start = 0 then (1+ finish)
        for finish = (position token string :start start)
        collect (subseq string start finish)
        until (null finish)))

(defun run-game (&optional (filename "./temp/system.txt"))
  (let ((*world* (with-open-file (file filename)
                   (load-data :file file))))
    (move-object *player* (nth (random (length *world*)) *world*))
    (loop named game-loop
          with *read-eval* = nil
          with *do-exit* = (lambda () (return-from game-loop nil))
          with cmd = nil
          with acmd = nil
          do (progn
               (format t "~%=== ~A ===~%~
                            Exits: ~S~%~
                            Objects: ~:[(None)~;~:*~{~A~^, ~}~]~%~
                            Mode: ~A~%~
                            ~A"
                       (get-name (get-loc *player*))
                       (mapcar (lambda (x)
                                 (location-short-name (find x *world* :key #'get-id)))
                               (location-exits (get-loc *player*)))
                       (mapcar #'get-name (location-contents (get-loc *player*)))
                       (mode-name (first *state*))
                       (mode-text (first *state*)))
               (setf cmd (read))
               ; DEBUG CODE
               (when (stringp cmd)
                 (setf cmd (list 'user cmd))))
          when (listp cmd)
              do (setf acmd (assoc (car cmd) *commands*)) and
              if acmd
                  do (apply (cdr acmd) (cdr cmd))
              else
                  do (format t "Invalid command!~%"))))

(defun do-quit ()
  (funcall *do-exit*))

(defun do-system (arg)
  (declare (ignore arg))
  (error "TBA")) ; TODO This

(defun do-user (arg)
  (do-command (first *state*) arg))

(defparameter *commands*
  (list (cons 'quit #'do-quit)
        (cons 'system #'do-system)
        (cons 'user #'do-user)))
