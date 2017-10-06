(in-package #:net-game)

(defparameter *quests*
  nil)

(defconstant +quest-types+
  '(quest))

;; TODO Quests should reward the player upon completion; currently they just sort of... finish

#|
 | A quest consists of a collection (alist) of states, indexed by integers or symbols. Each state
 | consists of an alist of triggers associated with lists of commands to execute when the trigger
 | is tripped. State 0 of a quest is a special state, in that it is the state an un-accepted quest
 | is in. A quest that has not been accepted yet will only trigger "initiate" triggers and only
 | when the quest in question is in the *knowledge-base* associated with the NPC the player has
 | spoken to.
 |#

#|
 | Quest triggers:
 |    (Note that most quest triggers are lists; some special ones, such as 'initiate, are symbols)
 |    (Note also that triggers only trip if the quest has been accepted; 'initiate is the one and only
 |     exception to this rule)
 |    (This alist associates each quest trigger with its arity)
 |  * initiate - When an un-accepted quest is in the knowledge base for an NPC and the player talks
 |    to that NPC and asks if he/she can help with anything ("requests a quest", in a sense), The
 |    initiate trigger is tripped, usually introducing the quest and allowing the player to accept
 |    it voluntarily. Note that initiate will ONLY trigger in State 0 and only if the
 |    *knowledge-base* associates the NPC with the quest in question.
 |  * (talk-to <npc-id> <prompt>) - When the player talks to the NPC with ID <npc-id>, in the
 |    NPC's menu, there will be an option with the text <prompt>. If the player chooses this
 |    option, the trigger will trip.
 |  * (talk-to! <npc-id>) - This is the more "urgent" version of the talk-to trigger. If the
 |    player talks with the NPC with ID <npc-id>, this trigger is tripped immediately and, if
 |    it exists, overrides the normal NPC menu. This trigger should be used sparingly, for if
 |    there are multiple talk-to! triggers from different quests, the order of precedence is
 |    arbitrary.
 |#
(defparameter *quest-triggers*
  '((initiate . 0)
    (talk-to . 2)
    (talk-to! . 1)))

#|
 | Quest commands:
 |  * (begin &rest <commands>) - Execute the commands in order.
 |  * (goto <state>) - Change the current quest state to <state>, which should be either a symbol
 |    or an integer. Remember that the integer 0 is a special state that is reserved for un-accepted
 |    quests and should not be used for anything else.
 |  * (complete) - Complete the current quest. This also sends the quest into the 'completed state.
 |    This is a no-op if the quest has already been completed.
 |  * (accept <state>) - Accept the current quest. This is a no-op if the quest has already
 |    been accepted. Since State 0 is reserved for un-accepted quests, this will also send the
 |    quest into state <state>.
 |  * (speak <text>) - Causes the given text to be output as though spoken in dialogue.
 |  * (narrate <text>) - Causes the given text to be output verbatim.
 |  * (branch <prompt> &rest <text> <command>) - Display a branching dialogue choice, executing
 |    the given command based on the response given by the player.
 |  * (narrate-branch <prompt> &rest <text> <command>) - Display a branching dialogue choice
 |    verbatim, executing the given command based on the response given by the player.
 |  * (if-has-item <item-match> <true> <false>) - Checks whether the player has an item
 |    matching <item-match>. If he/she does, execute the <true> branch. Otherwise, execute
 |    the <false> branch.
 |  * (remove-item <item-match>) - Remove the first item matching <item-match> from the
 |    player's inventory, or no items if none match.
 |#
(defparameter *quest-commands*
  ;; Implementation Note: q is a temporarily created object for un-accepted quests
  `((begin . ,(lambda (g q &rest commands) (mapc g commands)))
    (goto . ,(lambda (g q state) (quest-goto q state)))
    (complete . ,(lambda (g q) (quest-mark-complete q)))
    (accept . ,(lambda (g q state) (quest-accept q state)))
    (speak . ,(lambda (g q text) (speak-line text)))
    (narrate . ,(lambda (g q text) (narrate-line text)))
    (branch . ,(lambda (g q prompt &rest cmds)
                       (apply #'speak-branch prompt
                              (loop for arg = cmds then (cddr arg)
                                    while arg
                                    collect (let ((arg1 arg))
                                              (cons (first arg1)
                                                    (lambda () (funcall g (second arg1)))))))))
    (narrate-branch . ,(lambda (g q prompt &rest cmds)
                         (apply #'narrate-branch prompt
                                (loop for arg = cmds then (cddr arg)
                                      while arg
                                      collect (let ((arg1 arg))
                                                (cons (first arg1)
                                                      (lambda () (funcall g (second arg1)))))))))
    (if-has-item . ,(lambda (g q match true false) (if (some (lambda (x) (item-match match x))
                                                             (inv-items *player*))
                                                       (funcall g true)
                                                       (funcall g false))))
    (remove-item . ,(lambda (g q match) (setf (inv-items *player*)
                                              (remove-if (lambda (x) (item-match match x))
                                                         (inv-items *player*) :count 1))))))

(defgeneric run-quest-command (quest cmd))

;; These are the read-only objects that are stored in *quests*
(defclass quest-data (identifiable named)
  ((states :accessor quest-states
           :initarg :states
           :initform (make-hash-table))))

;; These are the mutable structures that the player modifies locally
(defclass quest-instance (identifiable named)
  ((state :accessor quest-state
          :initarg :state
          :initform 0)
   (completed :accessor is-quest-completed
              :initarg :completed
              :initform nil)))

(defmethod run-quest-command ((quest quest-instance) cmd)
  (let ((func (cdr (assoc (car cmd) *quest-commands*)))
        (recurse (lambda (cmd1) (run-quest-command quest cmd1))))
    (unless func
      (error "Malformed quest command - ~S" cmd))
    (apply func recurse quest (cdr cmd))))

;; No-op if trying to go to State 0, since that state is reserved
(defun quest-goto (quest state)
  (check-type state (or integer symbol) "a state identifer (symbol / integer)")
  (unless (eql state 0)
    (setf (quest-state quest) state)))

(defun quest-mark-complete (quest)
  (unless (is-quest-completed quest)
    (setf (is-quest-completed quest) t)
    (quest-goto quest 'completed)))

;; TODO Make it so triggers can be "matched" in more sophisticated ways than equality
;; (here and quest-has-trigger)
(defun do-quest-trigger (quest trigger)
  (let* ((quest-data (get-quest-data (get-id quest)))
         (state (quest-state quest))
         (triggers (gethash state (quest-states quest-data)))
         (cmd (cdr (assoc trigger triggers :test #'equal))))
    (when cmd
      (mapc (lambda (x) (run-quest-command quest x)) cmd))))

(defun quest-has-trigger (quest trigger)
  (let* ((quest-data (get-quest-data (get-id quest)))
         (state (quest-state quest))
         (triggers (gethash state (quest-states quest-data)))
         (cmd (cdr (assoc trigger triggers :test #'equal))))
    cmd))

;; Triggers for all active quests (do not use this for 'initiate)
(defun do-trigger (trigger)
  (check-type *player* player)
  (mapc (lambda (q) (do-quest-trigger q trigger))
        (active-quests *player*)))

;; Triggers for the first active quest which has the appropriate trigger
(defun do-first-trigger (trigger)
  (check-type *player* player)
  (loop for q in (active-quests *player*)
        when (quest-has-trigger q trigger)
            do (do-quest-trigger q trigger)
            and return t
        finally (return nil)))

(defun has-trigger (trigger)
  (check-type *player* player)
  (some (lambda (q) (quest-has-trigger q trigger))
        (active-quests *player*)))

(defun do-initiate-quest (quest-data)
  (let ((temp (make-quest-instance quest-data)))
    (do-quest-trigger temp 'initiate)))

(defun quest-accept (quest state)
  (check-type *player* player)
  (unless (has-started-quest (get-id quest))
    (quest-goto quest state)
    (push quest (active-quests *player*))))

(defun get-quest-data (qid)
  (check-type *quests* hash-table)
  (gethash qid *quests*))

(defun add-quest (data)
  (check-type *quests* hash-table)
  (setf (gethash (get-id data) *quests*) data))

(defun make-quest-instance (data)
  (make-instance 'quest-instance
                 :id (get-id data)
                 :name (get-name data)))

(defmethod load-object ((header (eql 'quest)) data)
  (destructuring-bind (quest-sym id name . args) data
    (make-instance 'quest-data
                   :id id
                   :name name
                   :states (loop with hash = (make-hash-table)
                                 for arg = (getf args :states) then (cddr arg)
                                 for key = (first arg)
                                 for value = (second arg)
                                 while arg
                                 do (setf (gethash key hash) value)
                                 finally (return hash)))))

(defun has-started-quest (quest-id &key ((:player *player*) *player*))
  (member quest-id (active-quests *player*) :key #'get-id))

(defun has-finished-quest (quest-id &key ((:player *player*) *player*))
  (and (has-started-quest quest-id)
       (is-quest-completed (find-by-id quest-id (active-quests *player*)))))

(defun total-quest-count ()
  (check-type *quests* hash-table)
  (hash-table-count *quests*))

(defun started-quests ()
  ;; Returns quests which have been started, including those that are completed
  (check-type *player* player)
  (active-quests *player*))

(defun finished-quests ()
  (check-type *player* player)
  (remove-if (complement #'is-quest-completed)
             (started-quests)))

(defun percent-started-quests ()
  (check-type *player* player)
  (check-type *quests* hash-table)
  (if (zerop (total-quest-count))
      1
      (/ (length (started-quests)) (total-quest-count))))

(defun percent-finished-quests ()
  (check-type *player* player)
  (check-type *quests* hash-table)
  (if (zerop (total-quest-count))
      1
      (/ (length (finished-quests)) (total-quest-count))))
