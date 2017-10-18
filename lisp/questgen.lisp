(in-package #:net-game)

(defpackage #:net-game-quest-gen
  (:use :common-lisp #:net-game)
  (:nicknames :ng-quest-gen)
  (:export :generate :+quest-association+))

(defconstant ng-quest-gen:+quest-association+
  '((:knowledge . #((ng-quest-gen::knowledge-1-a . ng-quest-gen::knowledge-1-b)
                    (ng-quest-gen::knowledge-2-a . ng-quest-gen::knowledge-2-b)
                    (ng-quest-gen::knowledge-3-a . ng-quest-gen::knowledge-3-b)
                    (ng-quest-gen::knowledge-4-a . ng-quest-gen::knowledge-4-b)))))

;; TODO Immersion. Some more generic varied responses, something for the NPCs
;; to say if the player talks to them during the quest, etc.

;; ///// The next thing is to make the quest stubs for each different motive
;; (We'll have to force the NPCs to have the right priorities for whatever we're
;; trying to do in order to test it)

;; ///// "Complications"

(defun ng-quest-gen::knowledge-1-a (npc)
  (flet ((fitness (x)
           (location-fitness-for x :treasure)))
    (let ((loc (weighted-random (mapcar (lambda (x) (cons x (fitness x)))
                                        (or (halo-annulus (get-loc npc) 3 6)
                                            (halo (get-loc npc) 5)))))
          (book (make-item (format nil "~A's Book" (person-nickname npc))
                           :weight 3))
          (flag (gensym)))
      (add-flag flag book)
      (list :location loc :book book :flag flag))))

(defun ng-quest-gen::knowledge-1-b (npc test)
  (make-quest-stub
   :name "Generated Quest"
   :establishment `((put-object ,(getf test :book) ,(getf test :location)))
   :evaluation `((initiate-with ,npc
                                :prompt "Hey, I'm looking for a certain book."
                                :yes-prompt "I'll help."
                                :no-prompt "I've got to go.")
                 (and-then (speak "Okay, it should be nearby."))
                 (give-object-to (flag ,(getf test :flag)) ,npc "Your book?"
                                 "Oh, excellent!" "Sorry for all the trouble."))))

(defun ng-quest-gen::knowledge-2-a (npc)
  (flet ((get-animals (loc)
           (loop for obj in (location-contents loc)
                 when (typep obj 'animal)
                     collect (anim-data obj)
                 when (and (typep obj 'neo-spawner)
                           (typep (find-by-id (neo-spawner-creature obj) *creatures*)
                                  'animal-data))
                     collect (find-by-id (neo-spawner-creature obj) *creatures*))))
    (let* ((objs (mapcan #'get-animals (halo (get-loc npc) 7)))
           (obj (choose objs)))
      (when obj
        (list :animal obj)))))

(defun ng-quest-gen::knowledge-2-b (npc test)
  (let* ((flag (gensym))
         (item `(item ,(format nil "~A's Camera" (get-name npc)) :weight 4 :flags (,flag)))
         (animal (getf test :animal))
         (instructions (format nil "I just need a photograph of a ~A. Just use the camera if ~
                                    you see one."
                               (get-name animal)))
         (narration (format nil "You photographed the ~A!" (get-name animal))))
    (make-quest-stub
     :name "Generated Quest"
     :establishment ()
     :evaluation `((initiate-with ,npc
                                  :action (branch "Would you be able to help me get some photographs?"
                                                  "Sure!" (if-cond (give-item ,item)
                                                                   (advance)
                                                                   (speak "You're carrying too much."))
                                                  "Not right now." (begin)))
                   (and-then (speak ,instructions))
                   (use-item-on (flag ,flag) (animal-of-type ,(get-id animal)) ,narration)
                   (give-object-to (flag ,flag) ,npc "I have your photograph."
                                   "Perfect! Thank you!" "That's not funny.")))))

(defun ng-quest-gen::knowledge-3-a (npc)
  (flet ((eligiblep (loc)
           (not (check-flag 'civilized loc))))
    (let* ((locs (remove-if (complement #'eligiblep) (halo-annulus (get-loc npc) 5 8)))
           (loc (choose locs)))
      (when loc
        (list :location loc)))))

(defun ng-quest-gen::knowledge-3-b (npc test)
  (let* ((loc (getf test :location))
         (loc-name (get-name loc)))
    (make-quest-stub
     :name "Generated Quest"
     :establishment ()
     :evaluation `((initiate-with ,npc
                                  :prompt ,(format nil "I would love to know about ~A." loc-name)
                                  :yes-prompt "I'll go check it out."
                                  :no-prompt "I don't have time.")
                   (and-then (speak ,(format nil
                                             "Okay. Just go visit ~A and then come tell me about it."
                                             loc-name)))
                   (goto-location ,loc ,(format nil
                                                "You take a good long look at the ~A."
                                                loc-name))
                   (talk-to ,npc "I saw the area." "Really? Perfect!")))))

(defun ng-quest-gen::knowledge-4-a (npc)
  (flet ((fitness (x)
           (float (/ (length (intersection (person-jobs npc) (person-jobs x)))))))
    (let* ((objs (loop for loc in (halo (get-loc npc) 8)
                       append (loop for obj in (location-contents loc)
                                    when (and (typep obj 'person)
                                              (not (eql obj npc)))
                                        collect (cons obj (fitness obj)))))
           (obj (weighted-random objs)))
      (when obj
        (list :person obj)))))

;; TODO Make some quest directives more general (so, for instance,
;; talk-to could take a pattern to match rather than just an ID).
(defun ng-quest-gen::knowledge-4-b (npc test)
  (let* ((target (getf test :person))
         (target-name (get-name target))
         (target-job (person-job-name target)))
    (make-quest-stub
     :name "Generated Quest"
     :establishment ()
     :evaluation `((initiate-with ,npc
                                  :prompt ,(format nil "I want to learn about the work of a ~A."
                                                   target-job)
                                  :yes-prompt "How can I help?"
                                  :no-prompt "Sorry. I'm the wrong person to ask.")
                   (and-then (speak ,(format nil
                                             "You'll help? Alright, just go ask ~A about their job."
                                             target-name)))
                   (talk-to ,target "Tell me about your job." "Well, where do I begin? ...")
                   (and-then (narrate ,(format nil
                                               "~A tells you all about it."
                                               target-name)))
                   (talk-to ,npc "I learned something." "Oh, wonderful!")))))

(defun ng-quest-gen:generate (npc motive)
  (setq motive :knowledge) ; TODO Manual override for debugging
  (loop with possible = (cdr (assoc motive ng-quest-gen:+quest-association+))
        for (a . b) across (shuffle possible)
        for test = (funcall a npc)
        when test
            return (funcall b npc test)
        finally (return nil)))
