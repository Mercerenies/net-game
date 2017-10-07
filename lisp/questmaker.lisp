(in-package #:net-game)

#|
 | Unless otherwise stated, arguments are actual objects of the appropriate type, so
 |#

#|
 | These quest evaluation directives evolve into a
 | quest object.
 | !! (collect-object obj)
 | (goto-location loc response)
 | (initiate-with npc text yes no)
 | (talk-to npc prompt response)
 | (give-object-to item-flag npc prompt yes-response no-response)
 |#

#|
 | These quest establishment directives are executed when the
 | quest is executed.
 | (put-object obj loc)
 |#

;; ///// THE QUEST EVALUATE PROCEDURE DOES NOT HAVE ALL DIRECTIVES (!!)
;; 1. Write this function: Given directives, generate the quest and get the world ready for it
;; 2. Generating the directives is... another matter...

;; TODO It generates names blindly right now; add some structure in the future
(defconstant +default-quest-state-queue+
  '(alpha beta gamma delta epsilon
    state0 state1 state2 state3 state4 state5))

(defconstant +quest-evaluation-directives+
  '(goto-location initiate-with talk-to give-object-to))

(defstruct quest-stub
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

(defgeneric quest-eval-cmd (cmd args state))

(defmethod quest-eval-cmd ((cmd (eql 'initiate-with)) args state)
  (destructuring-bind (npc text yes no) args
    ;; Add the quest to the NPC's knowledge base
    (push (get-id (quest-state-data state))
          (get-quest-list (get-id npc)))
    ;; And add the information to the quest itself
    (let ((trigger `(initiate
                     (branch ,text
                             ,yes (accept ,(quest-state-state1 state))
                             ,no (begin)))))
      (push trigger (gethash (quest-state-state0 state)
                             (quest-states (quest-state-data state)))))))

(defmethod quest-eval-cmd ((cmd (eql 'talk-to)) args state)
  (destructuring-bind (npc prompt response) args
    (let ((trigger `((talk-to ,(get-id npc) ,prompt)
                     (speak ,response)
                     ,(quest-goto-cmd state))))
      (push trigger (gethash (quest-state-state0 state)
                             (quest-states (quest-state-data state)))))))

(defmethod quest-eval-cmd ((cmd (eql 'give-object-to)) args state)
  (destructuring-bind (item-flag npc prompt yes-response no-response) args
    (let ((trigger `((talk-to ,(get-id npc) ,prompt)
                     (if-has-item ,item-flag
                                  (begin
                                   (remove-item ,item-flag)
                                   (speak ,yes-response)
                                   ,(quest-goto-cmd state))
                                  (speak ,no-response)))))
      (push trigger (gethash (quest-state-state0 state)
                             (quest-states (quest-state-data state)))))))

(defmethod quest-eval-cmd ((cmd (eql 'goto-location)) args state)
  (destructuring-bind (loc response) args
    (let ((trigger `((visit ,(get-id loc))
                     (narrate ,response)
                     ,(quest-goto-cmd state))))
      (push trigger (gethash (quest-state-state0 state)
                             (quest-states (quest-state-data state)))))))

(defun quest-eval-impl (gen quest state0 cmd final)
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
        (quest-eval-cmd head
                        args
                        (make-quest-eval-state :generator gen
                                               :data quest
                                               :state0 state0
                                               :state1 state1
                                               :final-p final))
        state1))))

(defun quest-evaluate (stub)
  (check-type stub quest-stub)
  (loop with gen = (make-quest-state-generator)
        with quest = (make-instance 'quest-data :id (gensym) :name "[Test]")
        with state = 0
        for cmds on (quest-stub-evaluation stub)
        for cmd = (car cmds)
        for final = (null (cdr cmds))
        do (setf state (quest-eval-impl gen quest state cmd final))
        finally (progn (add-quest quest)
                       (return quest))))

(defun quest-est-impl (cmd)
  (case (first cmd)
    (put-object
     (move-object (second cmd) (third cmd)))))

(defun quest-establish (stub)
  (check-type stub quest-stub)
  (loop for cmd in (quest-stub-establishment stub)
        do (quest-est-impl cmd)))

;;;; TODO DEBUG PROCEDURES ;;;;

(defun sample-quest-! (npc)
  (let ((item (make-item "Giant Pepperoni Pizza"))
        (flag (gensym))
        (loc (get-loc *player*)))
    (add-flag flag item)
    (make-quest-stub
     :establishment `((put-object ,item ,loc))
     :evaluation `((initiate-with ,npc "Help!" "Sure!" "Meh.")
                   (goto-location ,loc "You got it!")
                   (talk-to ,npc "Give me stuff." "Not yet.")
                   (give-object-to ,flag ,npc "Give me stuff." "Hey, you helped!" "Meh.")))))

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
