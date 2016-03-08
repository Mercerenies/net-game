(in-package #:net-game)

; This is really an awful hack. Ruby outputs S-expressions in Scheme notation,
; so it is necessary to define a couple of dispatch macros to deal with the
; differences.
; (TODO Look into read tables and see if this little hack can be restricted to
;  when reading in user data, not Lisp code)

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
