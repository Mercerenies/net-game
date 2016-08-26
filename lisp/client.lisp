(load "./lisp/package.lisp")

(in-package #:net-game)

(defparameter *port* 9321) ; 27001

(defparameter *socket* nil)

(defparameter *client-loc* nil)

(defparameter *client-pending* nil)

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

(defun client-waiting-on (sym)
  (member sym *client-pending*))

(defun client-request (sym)
  (format *socket* "need ~(~A~)~%" sym)
  (push sym *client-pending*))

(defun report-checkin ()
  (format *socket* "ter~%"))

(defun report-updates ()
  (when (and (not (client-waiting-on 'quests))
             (> (percent-started-quests) 0.5)
             (> (percent-finished-quests) 0.35))
    (client-request 'quests)))
; TODO More requests

(defmethod do-action :after (act obj preps)
  (declare (ignore act obj preps))
  (report-checkin)
  (report-updates)
  (when (and *client-loc* (not (eql *client-loc* (get-loc *player*))))
    (setf *client-lock* (get-loc *player*))
    ; //// Check for updates
    nil))
; /////
; TODO Check for updates in *socket* to update the world
; TODO Send a check-in signal in any case

(handle-args-and-play (ng-os:argv))
