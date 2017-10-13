(in-package #:net-game)

#|
 | Match Predicates:
 |  * (and <args> ...) - Matches any object that all of the predicates match.
 |  * (or <args> ...) - Matches any object matched by at least one of the predicates.
 |  * (animal-of-type <id>) - Matches only animals whose data has the given ID.
 |  * (flag <flag>) - Matches any flagged object which has the given flag.
 |  * (name <name>) - Matches any named object which has the given name (case insensitive).
 |  * (id <id>) - Matches any identifiable object which has the given ID.
 |#
(defparameter *match-predicates*
  `((and . ,(lambda (g obj &rest preds) (every g preds)))
    (or . ,(lambda (g obj &rest preds) (some g preds)))
    (animal-of-type . ,(lambda (g obj id) (and (typep obj 'animal)
                                               (eql (get-id (anim-data obj)) id))))
    (flag . ,(lambda (g obj flag) (and (typep obj 'flagged)
                                       (check-flag flag obj))))
    (name . ,(lambda (g obj name) (and (typep obj 'named)
                                       (equalp name (get-name obj)))))
    (id . ,(lambda (g obj id) (and (typep obj 'identifiable)
                                   (eql id (get-id obj)))))))

(defun matches-p (obj pred)
  (let ((func (cdr (assoc (car pred) *match-predicates*)))
        (recurse (lambda (pred1) (matches-p pred1 obj))))
    (unless func
      (error "Malformed match predicate - ~S" pred))
    (apply func recurse obj (cdr pred))))

;; TODO Deprecate and remove item-match and check-quest-predicate, as they are both
;; rendered obsolete by this functionality.
