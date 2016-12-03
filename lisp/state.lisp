(in-package #:net-game)

(defconstant +active-radius+
  5)

(defgeneric entity-turn (obj))

(defgeneric do-command (state arg))

(defgeneric mode-name (state))

(defgeneric mode-text (state))

(defmethod mode-name ((state t))
  (format nil "~S" state))

(defmethod mode-text ((state t))
  "")

(defmethod entity-turn ((obj t))
  ; By default, do nothing
  nil)

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
