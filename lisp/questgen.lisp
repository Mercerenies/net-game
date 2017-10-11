(in-package #:net-game)

(defpackage #:net-game-quest-gen
  (:use :common-lisp #:net-game)
  (:nicknames :ng-quest-gen)
  (:export :generate :+quest-association+))

(defconstant ng-quest-gen:+quest-association+
  '((:knowledge . #((ng-quest-gen::knowledge-1-a . ng-quest-gen::knowledge-1-b)))))

;; ///// The next thing is to make the quest stubs for each different motive
;; (We'll have to force the NPCs to have the right priorities for whatever we're
;; trying to do in order to test it)

;; ///// Expand halo to be able to return an annulus, not just a ball.

(defun ng-quest-gen::knowledge-1-a (npc)
  (flet ((fitness (x)
           (location-fitness-for x :treasure)))
    (let ((loc (weighted-random (mapcar (lambda (x) (cons x (fitness x)))
                                        (halo (get-loc npc) 5))))
          (book (make-item (format nil "~A's Book" (person-nickname npc))
                           :weight 3))
          (flag (gensym)))
      (add-flag flag book)
      (list :location loc :book book :flag flag))))

(defun ng-quest-gen::knowledge-1-b (npc test)
  (make-quest-stub
   :name "Generated Quest"
   :establishment `((put-object ,(getf test :book) ,(getf test :location)))
   :evaluation `((initiate-with ,npc "Hey, I'm looking for a certain book."
                                "I'll help." "I've got to go.")
                 (and-then (speak "Okay, it should be nearby."))
                 (give-object-to (flag ,(getf test :flag)) ,npc "Your book?"
                                 "Oh, excellent!" "Sorry for all the trouble."))))

(defun ng-quest-gen:generate (npc motive)
  (loop with possible = (cdr (assoc motive ng-quest-gen:+quest-association+))
        for (a . b) across (shuffle possible)
        for test = (funcall a npc)
        when test
            return (funcall b npc test)
        finally (return nil)))
