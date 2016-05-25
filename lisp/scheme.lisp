(in-package #:net-game)

; Ruby outputs S-expressions in Scheme notation, so it is necessary to define
; a couple of dispatch macros to deal with the differences when loading the
; expressions from Ruby.

(defmacro with-scheme-notation (&body body)
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

