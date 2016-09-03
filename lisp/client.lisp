(load "./lisp/package.lisp")

(in-package #:net-game)

(defparameter *port* 9321) ; 27001

(defparameter *socket* nil)

(defparameter *client-loc* nil)

(defparameter *client-pending* nil)

(defparameter *client-fname-n* 0)

(defun client-short-fname (n)
  (format nil "dfile~3,'0D" n))

(defun client-long-fname (n)
  (format nil "./temp/~A.txt" (client-short-fname n)))

(defun client-make-fname ()
  (let ((name (client-long-fname *client-fname-n*)))
    (incf *client-fname-n*)
    name))

(defun client-cleanup-fnames ()
  (loop for i from 0 below *client-fname-n*
        do (delete-file (client-long-fname i))))

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
        (client-cleanup-fnames)
        (when (open-stream-p *socket*)
          (format *socket* "quit~%"))))))

(defun client-waiting-on (sym)
  (assoc sym *client-pending*))

(defun client-request (sym &optional name)
  (setf name (or name sym))
  (unless (client-waiting-on name)
    (let ((worldname (client-make-fname))
          (donename (client-make-fname)))
      (format *socket* "need ~(~A~) ~A ~A~%" sym worldname donename)
      (push (list name worldname donename) *client-pending*))))

(defun report-checkin ()
  (format *socket* "ter~%"))

(defun report-updates ()
  (make-update-requests))

(defun check-for-updates ()
  (loop for (sym delta sentinel) in *client-pending*
        when (probe-file sentinel)
            do (with-open-file (file delta)
                 (load-delta :file file))
            and collect sym into removing
        finally (loop for rr in removing
                      do (setf *client-pending*
                               (remove rr *client-pending* :key #'first)))))

(defmethod do-action :after (act obj preps)
  (declare (ignore act obj preps))
  (report-checkin)
  (report-updates)
  (when (and *client-loc* (not (eql *client-loc* (get-loc *player*))))
    (setf *client-loc* (get-loc *player*))
    (check-for-updates))
  (when (not *client-loc*)
    (setf *client-loc* (get-loc *player*))))

(handle-args-and-play (ng-os:argv))
