(in-package #:net-game)

; TODO This file depends on client.lisp, which may not be loaded; make that dependency neater and more obvious
; TODO Consider a weapon trigger; weapons probably won't randomly spawn in the top-level anymore

(defun make-update-requests ()
  (let ((active-halo (halo (get-loc *player*) +active-radius+)))
    ; Completed Quests Trigger
    (when (and (> (percent-started-quests) 0.5)
               (> (percent-finished-quests) 0.35))
      (client-request 'quests 'q1))
    ; Few Quests Remaining Trigger
    (when (< (- (length (finished-quests)) (total-quest-count)) 5) ; TODO Probably increase this number
      (client-request 'quests 'q2))
    ; No Active Spawner Trigger
    (when (and (member-if (complement #'location-civilized)
                          active-halo)
               (null (active-spawner-set :player-object *player* :radius (1+ +active-radius+))))
      (client-request 'wildlife))
    ; Explored Map Trigger
    (when (and (plusp (hash-table-count *world*))
               (>= (/ (visited-count *player*) (hash-table-count *world*)) 0.6))
      (client-request 'map))
    ; Lack of Trees Trigger
    (when (> (count-if #'(lambda (loc) (and (not (location-civilized loc))
                                            (not (member-if #'(lambda (x) (typep x 'plant))
                                                            (location-contents loc)))))
                       active-halo)
             2)
      (client-request 'foliage))))
