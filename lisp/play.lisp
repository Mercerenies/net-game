(in-package #:net-game)

; ///// Quest system

; TODO We need some more check-type calls; they're nice and self-documenting

(defclass player (named located)
  ((hp :accessor hp
       :initarg :hp
       :initform 1.00)
   (inventory :accessor inventory
              :initarg :inventory
              :initform nil
              :type list)
   (quest-list :accessor quest-list
               :initarg :quest-list
               :initform nil
               :type list))
  (:default-initargs :name "Sandy"))

(defmethod do-action ((act (eql 'nuke)) (obj player) preps)
  (declare (ignore preps))
  (format t "Nuking self...~%")
  (setf (hp obj) 0)
  (check-for-death obj))

(defparameter *do-exit*
  (lambda () (error "Nothing to exit!")))

(defparameter *god-mode* nil)

(defparameter *player* nil)

(defparameter *world* nil)

(defparameter *state* (list 'global))

(defparameter *warps* nil)

(defun word-split (string &optional (token #\SPACE))
  (loop for start = 0 then (1+ finish)
        for finish = (position token string :start start)
        collect (subseq string start finish)
        until (null finish)))

(defun run-game (&optional (filename "./temp/system.txt"))
  (multiple-value-bind (*world* *creatures* *spawners* *quests*) (with-open-file (file filename)
                                                                   (load-data :file file))
    (let ((*player* (some (lambda (x)
                            (find-if (lambda (y) (typep y 'player))
                                     (location-contents x)))
                          *world*)))
      (unless *world*
        (error "The world is empty."))
      (unless *player*
        (error "The player object does not exist."))
      (load-speeches)
      (loop named game-loop
            with *read-eval* = nil
            with *do-exit* = (lambda () (return-from game-loop nil))
            with *god-mode* = t ; TODO Remove this; it's for debugging purposes only
            with cmd = nil
            with acmd = nil
            do (assign-numbers (mapcar (lambda (x) (find x *world* :key #'get-id))
                                       (location-exits (get-loc *player*)))
                               (location-contents (get-loc *player*))
                               (inventory *player*))
            do (progn
                 (format t "~%=== ~A ===~%~
                              Stats: ~,1F HP~%~
                              Exits: ~:[(None)~;~:*~{~A~^, ~}~]~%~
                              Objects: ~:[(None)~;~:*~{~A~^, ~}~]~%~
                              Inventory: ~:[(None)~;~:*~{~A~^, ~}~]~%~
                              Quests: ~:[(None)~;~:*~{~A~@[ (Done)~*~]~^, ~}~]~%~
                              Mode: ~A~%~
                              ~A~
                              > "
                         (get-name (get-loc *player*))
                         (* 100 (hp *player*))
                         (mapcar (lambda (x)
                                   (get-numbered-name (find x *world* :key #'get-id)))
                                 (location-exits (get-loc *player*)))
                         (mapcar #'get-numbered-name (location-contents (get-loc *player*)))
                         (mapcar #'get-numbered-name (inventory *player*))
                         (mapcan (lambda (x)
                                   (list (get-name x)
                                         (is-quest-completed x)))
                                 (quest-list *player*))
                         (mode-name (first *state*))
                         (mode-text (first *state*)))
                 (setf cmd (read-line)))
            if (string-equal cmd "quit")
                do (funcall *do-exit*)
            else
                do (do-command (first *state*) cmd)))))

