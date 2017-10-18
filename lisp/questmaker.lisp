(in-package #:net-game)

#|
 | Unless otherwise stated, arguments are actual objects of the appropriate type.
 |#

#|
 | These quest evaluation directives evolve into a
 | quest object.
 | (collect-object match response)
 | (goto-location loc response)
 | (initiate-with npc &key prompt yes-prompt no-prompt action)
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

(defclass quest-base-state ()
  ((generator :initform (make-quest-state-generator)
              :initarg :generator
              :accessor quest-state-generator)
   (data :initform nil
         :initarg :data
         :accessor quest-state-data)
   (state0 :initform 0
           :initarg :state0
           :accessor quest-state-state0)
   (state1 :initform 0
           :initarg :state1
           :accessor quest-state-state1)
   (final :initform nil
          :initarg :final
          :accessor quest-state-final-p)))

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

(defgeneric quest-macro-cmd (base stmt &key default))

(defmethod quest-macro-cmd ((base quest-base-state) (stmt string) &key (default 'speak))
  (list default stmt))

;; NOTE: The quest macros that are defined here use the convention
;; that custom commands are wrapped in >angled-brackets< to make clear
;; that they are custom commands. I recommend that other custom
;; commands use the same convention.
(defmethod quest-macro-cmd ((base quest-base-state) (stmt list) &key default)
  (case (car stmt)
    (>advance< (quest-goto-cmd base))
    (t (walk-quest-command (lambda (cmd) (quest-macro-cmd base cmd :default default)) stmt))))

(defgeneric quest-eval-cmd (base cmd args))

(defmethod quest-eval-cmd ((base quest-base-state) (cmd (eql 'initiate-with)) args)
  (destructuring-bind (npc &key prompt yes-prompt no-prompt action) args
    ;; Add the quest to the NPC's knowledge base
    (push (get-id (quest-state-data base))
          (get-quest-list (get-id npc)))
    ;; And add the information to the quest itself
    (let ((trigger (if action
                       `(initiate ,(quest-macro-cmd base action))
                       `(initiate
                         (branch ,prompt
                                 ,yes-prompt (accept ,(quest-state-state1 base))
                                 ,no-prompt (begin))))))
      (push trigger (gethash (quest-state-state0 base)
                             (quest-states (quest-state-data base)))))))

(defmethod quest-eval-cmd ((base quest-base-state) (cmd (eql 'talk-to)) args)
  (destructuring-bind (npc prompt response) args
    (let ((trigger `((talk-to ,(get-id npc) ,prompt)
                     ,(quest-macro-cmd base response)
                     ,(quest-goto-cmd base))))
      (push trigger (gethash (quest-state-state0 base)
                             (quest-states (quest-state-data base)))))))

(defmethod quest-eval-cmd ((base quest-base-state) (cmd (eql 'give-object-to)) args)
  (destructuring-bind (match npc prompt yes-response no-response) args
    (let ((trigger `((talk-to ,(get-id npc) ,prompt)
                     (if-has-item ,match
                                  (begin
                                   (remove-item ,match)
                                   ,(quest-macro-cmd base yes-response)
                                   ,(quest-goto-cmd base))
                                   ,(quest-macro-cmd base no-response)))))
      (push trigger (gethash (quest-state-state0 base)
                             (quest-states (quest-state-data base)))))))

(defmethod quest-eval-cmd ((base quest-base-state) (cmd (eql 'goto-location)) args)
  (destructuring-bind (loc response) args
    (let ((trigger `((visit ,(get-id loc))
                     ,(quest-macro-cmd base response :default 'narrate)
                     ,(quest-goto-cmd base))))
      (push trigger (gethash (quest-state-state0 base)
                             (quest-states (quest-state-data base)))))))

(defmethod quest-eval-cmd ((base quest-base-state) (cmd (eql 'collect-object)) args)
  (destructuring-bind (match response) args
    (let ((trigger `((collect ,match)
                     ,(quest-macro-cmd base response :default 'narrate)
                     ,(quest-goto-cmd base))))
      (push trigger (gethash (quest-state-state0 base)
                             (quest-states (quest-state-data base)))))))

(defmethod quest-eval-cmd ((base quest-base-state) (cmd (eql 'and-then)) args)
  (let ((trigger `((auto)
                   ,@(loop for arg in args
                           collect (quest-macro-cmd base arg))
                   ,(quest-goto-cmd base))))
    (push trigger (gethash (quest-state-state0 base)
                           (quest-states (quest-state-data base))))))

(defmethod quest-eval-cmd ((base quest-base-state) (cmd (eql 'use-item)) args)
  (destructuring-bind (match response) args
    (let ((trigger `((use ,match)
                     ,(quest-macro-cmd base response :default 'narrate)
                     ,(quest-goto-cmd base))))
      (push trigger (gethash (quest-state-state0 base)
                             (quest-states (quest-state-data base)))))))

(defmethod quest-eval-cmd ((base quest-base-state) (cmd (eql 'use-item-on)) args)
  (destructuring-bind (match target-match response) args
    (let ((trigger `((use-on ,match ,target-match)
                     ,(quest-macro-cmd base response :default 'narrate)
                     ,(quest-goto-cmd base))))
      (push trigger (gethash (quest-state-state0 base)
                             (quest-states (quest-state-data base)))))))

(defun quest-eval-impl (base cmd)
  (with-accessors ((states quest-states)) (quest-state-data base)
    (with-accessors ((state0 quest-state-state0) (state1 quest-state-state1)) base
      (flet ((goto (sym)
               (if (eql sym 'completed)
                   '(complete)
                   `(goto ,sym))))
        (let ((head (first cmd))
              (args (rest cmd)))
          (setf state1 (produce-quest-state (quest-state-generator base) 'linear
                                            :final (quest-state-final-p base)))
          ;; If we're in State 0, the directive MUST be initiate-with.
          (when (and (eql state0 0) (not (eql head 'initiate-with)))
            (error "Quest stub does not start with initiation"))
          (when (and (eql head 'initiate-with) (not (eql state0 0)))
            (error "Initiation at non-start of quest stub"))
          (unless (member head +quest-evaluation-directives+)
            (error "Invalid quest stub directive ~S" head))
          (quest-eval-cmd base head args)
          state1)))))

(defun quest-evaluate (stub &key (base (make-instance 'quest-base-state)))
  (check-type stub quest-stub)
  (with-accessors ((state0 quest-state-state0)) base
    (loop initially (progn
                      (setf (quest-state-data base)
                            (make-instance 'quest-data :id (gensym) :name (quest-stub-name stub)))
                      (setf state0 0))
          for cmds on (quest-stub-evaluation stub)
          for cmd = (car cmds)
          do (setf (quest-state-final-p base) (null (cdr cmds)))
          do (setf state0 (quest-eval-impl base cmd))
          finally (progn
                    (add-quest (quest-state-data base))
                    (return (quest-state-data base))))))

(defun quest-est-impl (cmd)
  (case (first cmd)
    (put-object
     (move-object (second cmd) (third cmd)))))

;; TODO Make this work with quest-base-state too.
(defun quest-establish (stub)
  (check-type stub quest-stub)
  (loop for cmd in (quest-stub-establishment stub)
        do (quest-est-impl cmd)))

;; TODO This is just for debugging
(defun generate-filler-quest (npc)
  (make-quest-stub
   :name "Generated Quest" ; TODO Make actual names for these
   :establishment ()
   :evaluation `((initiate-with ,npc :prompt "Want a free quest?" :yes-prompt "Yes" :no-prompt "No")
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
     :evaluation `((initiate-with ,npc
                                  :action (branch "Help!"
                                                  "Yes!" (if-cond (give-item (item "Mini Pepperoni Pizza"
                                                                                   :weight 9
                                                                                   :flags ()))
                                                                  (begin (speak "Yes")
                                                                         (>advance<))
                                                                  (speak "No"))
                                                  "No!" (speak "Sorry!")))
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
