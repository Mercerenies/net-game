(in-package #:net-game)

;; Ruby outputs S-expressions in Scheme notation, so it is necessary to define
;; a couple of dispatch macros to deal with the differences when loading the
;; expressions from Ruby.

(defmacro with-scheme-notation (&body body)
  "Ruby outputs S-expressions in Scheme notation, so it is necessary to define
   a couple of dispatch macros to deal with the differences when loading
   expressions from Ruby. This macro sets up a new readtable that is
   identical to the enclosing readtable with the exception that three new
   dispatch macros are added: #T for true, #F for false, and #N for nil.
   Note that, due to the semantics of Common Lisp, #F and #N evaluate to
   the same object."
  `(let ((*readtable* (copy-readtable *readtable*)))
     (set-dispatch-macro-character #\# #\T
                                   (lambda (s c n)
                                     (declare (ignore s c n))
                                     t))
     (set-dispatch-macro-character #\# #\F
                                   (lambda (s c n)
                                     (declare (ignore s c n))
                                     nil))
     ; False and nil are distinct in Ruby; not so in Common Lisp
     (set-dispatch-macro-character #\# #\N
                                   (lambda (s c n)
                                     (declare (ignore s c n))
                                     nil))
     (progn ,@body)))

