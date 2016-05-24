(in-package #:net-game)

; TODO Put states in for interactive objects (something like the state design pattern)
; TODO People in the world

(defclass player (named located)
  ((hp :accessor hp
       :initarg :hp
       :initform 1.00)
   (inventory :accessor inventory
              :initarg :inventory
              :initform nil
              :type list))
  (:default-initargs :name "Sandy"))

(defparameter *do-exit*
  (lambda () (error "Nothing to exit!")))

(defparameter *player* nil)

(defparameter *world* nil)

(defparameter *creatures* nil)

(defparameter *spawners* nil)

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
  (multiple-value-bind (*world* *creatures* *spawners*) (with-open-file (file filename)
                                                                        (load-data :file file))
    (let ((*player* (some (lambda (x)
                            (find-if (lambda (y) (typep y 'player))
                                     (location-contents x)))
                          *world*)))
      (unless *world*
        (error "The world is empty."))
      (unless *player*
        (error "The player object does not exist."))
      (loop named game-loop
            with *read-eval* = nil
            with *do-exit* = (lambda () (return-from game-loop nil))
            with cmd = nil
            with acmd = nil
            do (progn
                 (format t "~%=== ~A ===~%~
                              Stats: ~,1F HP~%~
                              Exits: ~S~%~
                              Objects: ~:[(None)~;~:*~{~A~^, ~}~]~%~
                              Inventory: ~:[(None)~;~:*~{~A~^, ~}~]~%~
                              Mode: ~A~%~
                              ~A~
                              > "
                         (get-name (get-loc *player*))
                         (* 100 (hp *player*))
                         (mapcar (lambda (x)
                                   (location-short-name (find x *world* :key #'get-id)))
                                 (location-exits (get-loc *player*)))
                         (mapcar #'get-name (location-contents (get-loc *player*)))
                         (mapcar #'get-name (inventory *player*))
                         (mode-name (first *state*))
                         (mode-text (first *state*)))
                 (setf cmd (read-line)))
            if (string-equal cmd "quit")
                do (funcall *do-exit*)
            else
                do (do-command (first *state*) cmd)))))

