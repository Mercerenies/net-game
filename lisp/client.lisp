(load "./lisp/package.lisp")

(in-package #:net-game)

(defparameter *port* 9321) ; 27001

(defparameter *socket* nil)

(defun handle-args-and-play (argv &aux (*port* *port*) (*socket* *socket*))
  (let ((filename "./temp/system.txt"))
    (loop for rest = argv then (cddr rest)
          for key = (first rest)
          for value = (second rest)
          while rest
          do (cond
               ((string-equal key "-port") (setf *port*
                                                 (or (parse-integer value :junk-allowed t)
                                                     *port*)))
               ((string-equal key "-file") (setf filename
                                                 value))
               (t (format *error-output* "Invalid command line flag '~S'" key)
                  (full-exit 1))))
    (with-open-stream (*socket* (ng-os:connect-to-socket *port*))
      (unwind-protect
           (run-game :filename filename)
        (when (open-stream-p *socket*)
          (format *socket* "quit~%"))))))

(handle-args-and-play (ng-os:argv))
