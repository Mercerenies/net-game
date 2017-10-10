
(defpackage #:net-game-quest-gen
  (:use :common-lisp #:net-game)
  (:nicknames :ng-quest-gen)
  (:export :generate))
(in-package #:net-game-quest-gen)

(defconstant +quest-association+
  '((:knowledge . #((knowledge-1-a . knowledge-1-b)))))

;; ///// The next thing is to make the quest stubs for each different motive
;; (We'll have to force the NPCs to have the right priorities for whatever we're
;; trying to do in order to test it)

(defun knowledge-1-a (npc)
  nil)

(defun knowledge-1-b (npc test)
  nil)

(defun generate (npc motive)
  (loop with possible = (cdr (assoc motive +quest-association+))
        for (a . b) across (shuffle possible)
        for test = (funcall a npc)
        when test
            return (funcall b npc test)
        finally (return nil)))
