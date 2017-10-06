(in-package #:net-game)

#|
 | Unless otherwise stated, arguments are actual objects of the appropriate type, so
 |#

#|
 | These quest evaluation directives evolve into a
 | quest object.
 | !! (collect-object obj)
 | !! (goto-location loc)
 | (talk-to npc text)
 | (give-object-to obj npc)
 |#

#|
 | These quest establishment directives are executed when the
 | quest is executed.
 | (put-object obj loc)
 |#

;; /////
;; 1. Write this function: Given directives, generate the quest and get the world ready for it
;; 2. Generating the directives is... another matter...

(defstruct quest-stub
  (establishment nil)
  (evaluation nil))

(defun quest-est-impl (cmd)
  (case (first cmd)
    (put-object
     (move-object (second cmd) (third cmd)))))

(defun quest-establish (stub)
  (check-type stub quest-stub)
  (loop for cmd in (quest-stub-establishment stub)
        do (quest-est-impl cmd)))
