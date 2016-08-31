(in-package #:net-game)

; TODO We need some more check-type calls; they're nice and self-documenting

(defclass player (named located damageable carrying)
  ((active-quests :accessor active-quests
                  :initarg :active-quests
                  :initform nil
                  :type list)
   (visited-locs :accessor visited-locs
                 :initform (make-hash-table)
                 :type hash-table))
  (:default-initargs :name "Sandy"))

(defparameter *do-exit*
  (lambda () (error "Nothing to exit!")))

(defparameter *player* nil)

(defparameter *world* nil)

(defparameter *state* (list 'global))

(defparameter *warps* nil)

(defun make-player ()
  (make-instance 'player))

(defun visited-count (player)
  (hash-table-count (visited-locs player)))

(defmethod move-object :after ((obj player) (new-loc location))
  (setf (gethash (get-id new-loc) (visited-locs obj)) t))

(defmethod move-object :after ((obj player) (new-loc null))
  (format t "~%** GAME OVER **~%You have died.~%~%")
  (funcall *do-exit*))

(defmethod system-keys append ((obj player))
           `((nil "Percent Explored" ,(if (plusp (hash-table-count *world*))
                                          (* 100.0 (/ (visited-count *player*) (hash-table-count *world*)))
                                          0.0))))

(defun word-split (string &optional (token #\SPACE))
  (loop for start = 0 then (1+ finish)
        for finish = (position token string :start start)
        collect (subseq string start finish)
        until (null finish)))

(defun run-game (&key (filename "./temp/system.txt"))
  (multiple-value-bind (*world* *creatures* *spawners* *quests*) (with-open-file (file filename)
                                                                   (load-data :file file))
    (let ((*player* (loop for loc being the hash-values in *world*
                          for player = (find-if (lambda (y) (typep y 'player))
                                                (location-contents loc))
                          when player return player)))
      (unless (plusp (hash-table-count *world*))
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
            do (assign-numbers (mapcar (lambda (x) (gethash x *world*))
                                       (location-exits (get-loc *player*)))
                               (location-contents (get-loc *player*))
                               (inv-items *player*))
            do (progn
                 (format t "~%=== ~A ===~%~
                              Stats: ~,1F HP~%~
                              Exits: ~:[(None)~;~:*~{~A~^, ~}~]~%~
                              Objects: ~:[(None)~;~:*~{~A~^, ~}~]~%~
                              Inventory: ~:[(None)~;~:*~{~A~^, ~}~]~%~
                              Carrying: ~D/~D units~%~
                              Quests: ~:[(None)~;~:*~{~A~@[ (Done)~*~]~^, ~}~]~%~
                              Mode: ~A~%~
                              ~A~
                              > "
                         (get-name (get-loc *player*))
                         (* 100 (hp *player*))
                         (mapcar (lambda (x)
                                   (get-numbered-name (gethash x *world*)))
                                 (location-exits (get-loc *player*)))
                         (mapcar #'get-numbered-name (remove-if-not #'(lambda (x) (typep x 'named))
                                                                    (location-contents (get-loc *player*))))
                         (mapcar #'get-numbered-name (inv-items *player*))
                         (inv-current-weight *player*)
                         (inv-max-weight *player*)
                         (mapcan (lambda (x)
                                   (list (get-name x)
                                         (is-quest-completed x)))
                                 (active-quests *player*))
                         (mode-name (first *state*))
                         (mode-text (first *state*)))
                 (setf cmd (read-line)))
            if (string-equal cmd "quit")
                do (funcall *do-exit*)
            else
                do (do-command (first *state*) cmd)))))

