(in-package #:net-game)

;; TODO We need some more check-type calls; they're nice and self-documenting

(defclass player (named located damageable carrying loaded)
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

(defparameter *key* nil)

(defun make-player ()
  "Makes an instance of the player object, with no active quests and an empty visited list."
  (make-instance 'player))

(defun visited-count (player)
  "Returns the number of unique location nodes the player has visited."
  (hash-table-count (visited-locs player)))

(defmethod move-object :after ((obj player) (new-loc location))
  (setf (gethash (get-id new-loc) (visited-locs obj)) t))

(defmethod move-object :after ((obj player) (new-loc null))
  (format t "~%** GAME OVER **~%You have died.~%~%")
  (funcall *do-exit*))

(defmethod object-nature ((obj player))
    'person)

(defmethod system-keys append ((obj player))
           `((nil "Percent Explored" ,(if (plusp (hash-table-count *world*))
                                          (* 100.0 (/ (visited-count *player*) (hash-table-count *world*)))
                                          0.0))
             (*key* "File Key" ,*key*)))

(defun word-split (string &optional (token #\SPACE))
  "Splits a string into words, tokenized by the given token, which defaults to #\SPACE. If provided, the
   token should be a single character, not an arbitrary string."
  (loop for start = 0 then (1+ finish)
        for finish = (position token string :start start)
        collect (subseq string start finish)
        until (null finish)))

(defun run-game (&key (filename "./temp/system.txt") (callback nil))
  "Runs the game in full, loading the world data from the given file. Note that the run-game function
   itself makes no effort to interact with external servers, such as the Lua interface. If such
   interaction is desired, it should be implemented as a hook on do-action or a similar method.
   The optional callback argument, if supplied, should be a nullary function which will be called
   immediately after the world is loaded and before the player begins interaction."
  (alpha-bind (let ((*origin* filename))
                (with-open-file (file filename)
                  (load-data :file file)))
    (let ((*player* (loop for loc being the hash-values in *world*
                          for player = (find-if (lambda (y) (typep y 'player))
                                                (location-contents loc))
                          when player return player)))
      (do-neo-spawner-migration)
      (unless (plusp (hash-table-count *world*))
        (error "The world is empty."))
      (unless *player*
        (error "The player object does not exist."))
      (when callback
        (funcall callback))
      (loop named game-loop
            with *read-eval* = nil
            with *do-exit* = (lambda () (return-from game-loop nil))
            with *god-mode* = t ; TODO Remove this; it's for debugging purposes only
            with cmd = nil
            with acmd = nil
            do (assign-numbers (mapcar (lambda (x) (gethash x *world*))
                                       (location-exits (get-loc *player*)))
                               (remove-hidden (location-contents (get-loc *player*)))
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
                         (mapcar #'get-numbered-name
                                 (remove-if-not #'(lambda (x) (typep x 'named))
                                                (remove-hidden (location-contents (get-loc *player*)))))
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

