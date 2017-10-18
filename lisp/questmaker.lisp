(in-package #:net-game)

#|
 | Unless otherwise stated, arguments are actual objects of the appropriate type.
 |#

#|
 | These quest evaluation directives evolve into a
 | quest object.
 | (collect-object match response)
 | (goto-location loc response)
 | (initiate-with npc text &key yes-prompt no-prompt prompt)
 | (talk-to npc prompt response)
 | (give-object-to match npc prompt yes-response no-response)
 | (and-then commands ...)
 | (use-item match response)
 | (use-item-on match target-match response)
 |#

#|
 | These quest establishment directives are executed when the
 | quest is executed.
 | (put-object obj loc)
 |#

#|
 | These quest macros will expand into quest commands when put into
 | an evaluation context.
 | (advance) ;; ///// Test me (use :prompt so it doesn't have to be a lambda any more)
 |#

;; 1. Write this function: Given directives, generate the quest and get the world ready for it
;; 2. Generating the directives is... another matter...

;; TODO It generates names blindly right now; add some structure in the future
(defconstant +default-quest-state-queue+
  '(alpha beta gamma delta epsilon
    state0 state1 state2 state3 state4 state5))

(defconstant +quest-evaluation-directives+
  '(collect-object goto-location initiate-with talk-to give-object-to and-then
    use-item use-item-on))

(defstruct quest-stub
  (name "")
  (establishment nil)
  (evaluation nil))

(defclass quest-state-generator ()
  ((used :initform nil
         :type list)
   (counter :initform 0
            :type integer)
   (queue :initarg queue
          :initform (copy-list +default-quest-state-queue+)
          :type list)))

(defclass quest-base-commands ()
  ())

(defstruct (quest-eval-state (:conc-name quest-state-))
  generator
  data
  state0
  state1
  final-p)

;; TODO This function doesn't currently check whether its argument
;; contains no duplicates.
(defun make-quest-state-generator (&key (queue nil queue?))
  ;; The argument, if supplied, must contain no duplicates.
  (if queue?
      (make-instance 'quest-state-generator :queue queue)
      (make-instance 'quest-state-generator)))

(defun produce-quest-state (gen nature &key final)
  (check-type gen quest-state-generator)
  (with-slots (used counter queue) gen
    (let ((name (if final
                    'completed
                    (ecase nature
                      ((nil) (if (not (null queue))
                                 (pop queue)
                                 (gensym)))
                      (linear (incf counter))))))
      (push name used)
      name)))

(defun quest-goto-cmd (state)
  (if (eql (quest-state-state1 state) 'completed)
      '(complete)
      `(goto ,(quest-state-state1 state))))

(defgeneric quest-macro-cmd (base stmt state &key default))

(defmethod quest-macro-cmd ((base quest-base-commands) (stmt string) state &key (default 'speak))
  (list default stmt))

(defmethod quest-macro-cmd ((base quest-base-commands) (stmt list) state &key default)
  (case (car stmt)
    (advance (quest-goto-cmd state))
    (t stmt)))

(defgeneric quest-eval-cmd (base cmd args state))

;; TODO The text argument is unused if :prompt is supplied, so it should be optional
(defmethod quest-eval-cmd ((base quest-base-commands) (cmd (eql 'initiate-with)) args state)
  (destructuring-bind (npc text &key yes-prompt no-prompt prompt) args
    ;; Add the quest to the NPC's knowledge base
    (push (get-id (quest-state-data state))
          (get-quest-list (get-id npc)))
    ;; And add the information to the quest itself
    (let ((trigger (if prompt
                       `(initiate ,(funcall prompt (quest-state-state1 state)))
                       `(initiate
                         (branch ,text
                                 ,yes-prompt (accept ,(quest-state-state1 state))
                                 ,no-prompt (begin))))))
      (push trigger (gethash (quest-state-state0 state)
                             (quest-states (quest-state-data state)))))))

(defmethod quest-eval-cmd ((base quest-base-commands) (cmd (eql 'talk-to)) args state)
  (destructuring-bind (npc prompt response) args
    (let ((trigger `((talk-to ,(get-id npc) ,prompt)
                     ,(quest-macro-cmd base response state)
                     ,(quest-goto-cmd state))))
      (push trigger (gethash (quest-state-state0 state)
                             (quest-states (quest-state-data state)))))))

(defmethod quest-eval-cmd ((base quest-base-commands) (cmd (eql 'give-object-to)) args state)
  (destructuring-bind (match npc prompt yes-response no-response) args
    (let ((trigger `((talk-to ,(get-id npc) ,prompt)
                     (if-has-item ,match
                                  (begin
                                   (remove-item ,match)
                                   ,(quest-macro-cmd base yes-response state)
                                   ,(quest-goto-cmd state))
                                   ,(quest-macro-cmd base no-response state)))))
      (push trigger (gethash (quest-state-state0 state)
                             (quest-states (quest-state-data state)))))))

(defmethod quest-eval-cmd ((base quest-base-commands) (cmd (eql 'goto-location)) args state)
  (destructuring-bind (loc response) args
    (let ((trigger `((visit ,(get-id loc))
                     ,(quest-macro-cmd base response state :default 'narrate)
                     ,(quest-goto-cmd state))))
      (push trigger (gethash (quest-state-state0 state)
                             (quest-states (quest-state-data state)))))))

(defmethod quest-eval-cmd ((base quest-base-commands) (cmd (eql 'collect-object)) args state)
  (destructuring-bind (match response) args
    (let ((trigger `((collect ,match)
                     ,(quest-macro-cmd base response state :default 'narrate)
                     ,(quest-goto-cmd state))))
      (push trigger (gethash (quest-state-state0 state)
                             (quest-states (quest-state-data state)))))))

(defmethod quest-eval-cmd ((base quest-base-commands) (cmd (eql 'and-then)) args state)
  (let ((trigger `((auto)
                   ,@(loop for arg in args
                           collect (quest-macro-cmd base arg state))
                   ,(quest-goto-cmd state))))
    (push trigger (gethash (quest-state-state0 state)
                           (quest-states (quest-state-data state))))))

(defmethod quest-eval-cmd ((base quest-base-commands) (cmd (eql 'use-item)) args state)
  (destructuring-bind (match response) args
    (let ((trigger `((use ,match)
                     ,(quest-macro-cmd base response state :default 'narrate)
                     ,(quest-goto-cmd state))))
      (push trigger (gethash (quest-state-state0 state)
                             (quest-states (quest-state-data state)))))))

(defmethod quest-eval-cmd ((base quest-base-commands) (cmd (eql 'use-item-on)) args state)
  (destructuring-bind (match target-match response) args
    (let ((trigger `((use-on ,match ,target-match)
                     ,(quest-macro-cmd base response state :default 'narrate)
                     ,(quest-goto-cmd state))))
      (push trigger (gethash (quest-state-state0 state)
                             (quest-states (quest-state-data state)))))))

(defun quest-eval-impl (base gen quest state0 cmd final)
  (with-accessors ((states quest-states)) quest
    (flet ((goto (sym)
             (if (eql sym 'completed)
                 '(complete)
                 `(goto ,sym))))
      (let ((state1 (produce-quest-state gen 'linear :final final))
            (head (first cmd))
            (args (rest cmd)))
        ;; If we're in State 0, the directive MUST be initiate-with.
        (when (and (eql state0 0) (not (eql head 'initiate-with)))
          (error "Quest stub does not start with initiation"))
        (when (and (eql head 'initiate-with) (not (eql state0 0)))
          (error "Initiation at non-start of quest stub"))
        (unless (member head +quest-evaluation-directives+)
          (error "Invalid quest stub directive ~S" head))
        (quest-eval-cmd base
                        head
                        args
                        (make-quest-eval-state :generator gen
                                               :data quest
                                               :state0 state0
                                               :state1 state1
                                               :final-p final))
        state1))))

(defun quest-evaluate (stub &key (base (make-instance 'quest-base-commands)))
  (check-type stub quest-stub)
  (loop with gen = (make-quest-state-generator)
        with quest = (make-instance 'quest-data :id (gensym) :name (quest-stub-name stub))
        with state = 0
        for cmds on (quest-stub-evaluation stub)
        for cmd = (car cmds)
        for final = (null (cdr cmds))
        do (setf state (quest-eval-impl base gen quest state cmd final))
        finally (progn (add-quest quest)
                       (return quest))))

(defun quest-est-impl (cmd)
  (case (first cmd)
    (put-object
     (move-object (second cmd) (third cmd)))))

;; TODO Make this work with quest-base-commands too.
(defun quest-establish (stub)
  (check-type stub quest-stub)
  (loop for cmd in (quest-stub-establishment stub)
        do (quest-est-impl cmd)))

;; TODO This is just for debugging
(defun generate-filler-quest (npc)
  (make-quest-stub
   :name "Generated Quest" ; TODO Make actual names for these
   :establishment ()
   :evaluation `((initiate-with ,npc "Want a free quest?" :yes-prompt "Yes" :no-prompt "No")
                 (and-then (speak "Just talk to me again!"))
                 (talk-to ,npc "I'm done" "Good job!"))))

(defun generate-quest (npc)
  (let* ((motives (know-motives (knowledge-get *knowledge-base* (get-id npc))))
         (skewed-motives (mapcar (lambda (x) (cons (car x) (* (cdr x) (cdr x)))) motives))
         (motive (weighted-random skewed-motives)))
    (or (ng-quest-gen:generate npc motive)
        (generate-filler-quest npc))))

(defun generate-and-integrate-quest (npc)
  (let ((q (generate-quest npc)))
    (quest-establish q)
    (quest-evaluate q)))

;;;; TODO DEBUG PROCEDURES ;;;;

(defun sample-quest-! (npc)
  (let ((item (make-item "Giant Pepperoni Pizza"))
        (flag (gensym))
        (loc (get-loc *player*)))
    (add-flag flag item)
    (make-quest-stub
     :name "[Test]"
     :establishment `((put-object ,item ,loc))
     :evaluation `((initiate-with ,npc "Help!"
                                  :prompt ,(lambda (state1)
                                             `(branch "Help!"
                                                      "Yes!" (if-cond (give-item (item "Mini Pepperoni Pizza"
                                                                                 :weight 9
                                                                                 :flags ()))
                                                                      (begin (speak "Yes")
                                                                             (accept ,state1))
                                                                      (speak "No"))
                                                      "No!" (speak "Sorry!"))))
                   (give-object-to (flag ,flag) ,npc "Give me stuff." "Hey, you helped!" "Meh.")))))

(defun sample-quest-encode-! ()
  (let* ((person (make-person (gensym) "Steve"))
         (q (sample-quest-! person)))
    (move-object person (get-loc *player*))
    (quest-establish q)
    (setf (knowledge-get *knowledge-base* (get-id person)) (make-human-knowledge :quests nil))
    (let ((q1 (quest-evaluate q)))
      (format t "~S~%" q1)
      q1)))

;;;; ;;;;
