(in-package #:net-game)

;; TODO This file depends on client.lisp, which may not be loaded; make that dependency neater and more obvious

(defun make-update-requests ()
  (let ((active-halo (halo (get-loc *player*) +active-radius+)))
    ;; Low Actor Ratio Trigger
    (when (< (/ (knowledge-count *knowledge-base* 'human-knowledge)
                (knowledge-count *knowledge-base* 'city-knowledge))
             3/2)
      ;; NOTE: The name q1 is for legacy purposes, from when there
      ;; used to be two actor triggers and they were called "quest"
      ;; triggers (hence the q).
      (client-request 'actors 'q1))
    ;; No Active Spawner Trigger
    (when (and (member-if #'(lambda (x) (not (check-flag 'civilized x)))
                          active-halo)
               (null (active-spawner-set :player-object *player* :radius (1+ +active-radius+))))
      (client-request 'wildlife))
    ;; Explored Map Trigger
    (when (and (plusp (hash-table-count *world*))
               (>= (/ (visited-count *player*) (hash-table-count *world*)) 0.6))
      (client-request 'map))
    ;; Lack of Trees Trigger
    (when (> (count-if #'(lambda (loc) (and (not (check-flag 'civilized loc))
                                            (not (member-if #'(lambda (x) (typep x 'plant))
                                                            (location-contents loc)))))
                       active-halo)
             2)
      (client-request 'foliage))
    ;; Not Enough Weapons Trigger
    (when (< (pool-count (lambda (x) (typep x 'weapon))) 10)
      (client-request 'equipment))
    ;; Not Enough Monsters Trigger
    (when (< (pool-count (lambda (x) (typep x 'monster))) 5)
      (client-request 'mythical))))
