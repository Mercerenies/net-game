(in-package #:net-game)

(defpackage #:net-game-quest-gen
  (:use :common-lisp #:net-game)
  (:nicknames :ng-quest-gen)
  (:export :choose-motive :generate-bind :generate :+quest-association+))

;; TODO get-origin for quest-generated objects

(defconstant ng-quest-gen:+quest-association+
  '((:subquest .  #((ng-quest-gen::subquest-1-a  . ng-quest-gen::subquest-1-b )))
    (:knowledge . #((ng-quest-gen::knowledge-1-a . ng-quest-gen::knowledge-1-b)
                    (ng-quest-gen::knowledge-2-a . ng-quest-gen::knowledge-2-b)
                    (ng-quest-gen::knowledge-3-a . ng-quest-gen::knowledge-3-b)
                    (ng-quest-gen::knowledge-4-a . ng-quest-gen::knowledge-4-b)))))

;; TODO Immersion. Some more generic varied responses, something for the NPCs
;; to say if the player talks to them during the quest, etc.

;; ///// The next thing is to make the quest stubs for each different motive
;; (We'll have to force the NPCs to have the right priorities for whatever we're
;; trying to do in order to test it)

;; ///// "Complications"
;; (Collecting item complications, first, probably)

;; TODO data == (npc hold-text) but hold-text is currently unused...
(defun ng-quest-gen::subquest-1-a (data)
  (destructuring-bind (npc hold-text) data
    (declare (ignore hold-text))
    (let ((motive (ng-quest-gen:choose-motive npc)))
      (multiple-value-bind (b test)
          (ng-quest-gen:generate-bind npc motive)
        (list :b b :test test)))))

(defun ng-quest-gen::subquest-1-b (data test)
  (destructuring-bind (npc hold-text) data
    (declare (ignore hold-text))
    (let ((b1 (getf test :b))
          (test1 (getf test :test)))
      (let ((q (funcall b1 npc test1)))
        (with-accessors ((eval quest-stub-evaluation)) q
          (setf (car eval)
                (initiate-subquest (car eval) "You needed help?"))
          (setf eval (cons '(and-then (speak "Hold on. I'm really in the middle of something"))
                           eval)))
        q))))

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
                                (branch "Hey, I'm looking for a certain book."
                                        "I'll help." (>advance<)
                                        "I've got to go." (begin)))
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
                                  (branch "Would you be able to help me get some photographs?"
                                          "Sure!" (if-cond (give-item ,item)
                                                           (>advance<)
                                                           (speak "You're carrying too much."))
                                          "Not right now." (begin)))
                   (and-then (speak ,instructions))
                   (trigger (use-on (flag ,flag) (animal-of-type ,(get-id animal)))
                            (narrate ,narration)
                            (>advance<))
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
                                  (branch ,(format nil "I would love to know about ~A." loc-name)
                                          "I'll go check it out." (>advance<)
                                          "I don't have time." (begin)))
                   (and-then (speak ,(format nil
                                             "Okay. Just go visit ~A and then come tell me about it."
                                             loc-name)))
                   (trigger (visit ,(get-id loc))
                            (narrate ,(format nil "You take a good long look at the ~A."
                                              loc-name))
                            (>advance<))
                   (trigger (talk-to ,(get-id npc) "I saw the area.")
                            (speak "Really? Perfect!")
                            (>advance<))))))

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

;; TODO Sanity checks (type checks) for triggers (like if talk-to is given a non-id, error!)
;;      (Check these when compiling AND when evaluating triggers)

;; TODO Make some quest directives more general (so, for instance,
;; talk-to could take a pattern to match rather than just an ID).
(defun ng-quest-gen::knowledge-4-b (npc test)
  (let* ((target (getf test :person))
         (target-name (get-name target))
         (target-job (person-job-name target))
         (complication (if (<= (random 1.0) 0.8)
                           (ng-quest-gen:generate (list target "(UNUSED)") :subquest)
                           nil)))
    (make-quest-stub
     :name "Generated Quest"
     :establishment (and complication (quest-stub-establishment complication))
     :evaluation `((initiate-with ,npc
                                  (branch ,(format nil "I want to learn about the work of a ~A."
                                                   target-job)
                                          "How can I help?" (>advance<)
                                          "Sorry. I'm the wrong person to ask." (begin)))
                   (and-then (speak ,(format nil
                                             "You'll help? Alright, just go ask ~A about their job."
                                             target-name)))
                   (trigger (talk-to ,(get-id target) "Tell me about your job.")
                            (>advance<))
                   ,@(and complication (quest-stub-evaluation complication))
                   (and-then (speak "Well, where do I begin? ...")
                             (narrate ,(format nil
                                               "~A tells you all about their job."
                                               target-name)))
                   (trigger (talk-to ,(get-id npc) "I learned something.")
                            (speak "Oh, wonderful!")
                            (>advance<))))))

(defun ng-quest-gen:choose-motive (npc)
  "Given an NPC, this function randomly selects a motivation of that
   NPC, skewed toward the motivations that the NPC favors."
  (let* ((motives (know-motives (knowledge-get *knowledge-base* (get-id npc))))
         (skewed-motives (mapcar (lambda (x) (cons (car x) (* (cdr x) (cdr x)))) motives)))
    (weighted-random skewed-motives)))

(defun ng-quest-gen:generate-bind (obj motive)
  (unless (eql motive :subquest) ; Just keep painting yourself that corner there...
    (setq motive :knowledge)) ; TODO Manual override for debugging
  (loop with possible = (cdr (assoc motive ng-quest-gen:+quest-association+))
        for (a . b) across (shuffle possible)
        for test = (funcall a obj)
        when test
            return (values b test)
        finally (return (values nil nil))))

(defun ng-quest-gen:generate (obj motive)
  (multiple-value-bind (b test)
      (ng-quest-gen:generate-bind obj motive)
    (funcall b obj test)))

;; /////
;; 1. Make initiate-with always use :action (Done)
;; 2. Add "primitive" directives (instead of (talk-to ...), use (triggers (talk-to ...)))
;;    (No reason for the added complexity when a simple trigger will do)
;; 3. Remove a lot of the higher-level directives in favor of more "primitive" ones
;; 4. Complications
