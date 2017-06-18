(load "./lisp/package.lisp")

; TODO Catch errors at the top level and print them through the logging interface

(in-package #:net-game)

(defparameter *port* 9321) ; 27001

(defparameter *socket* nil)

(defparameter *client-loc* nil)

(defparameter *client-pending* nil)

(defparameter *client-concluding* nil)

(defparameter *client-fname-n* 0)

(defun client-short-fname (n)
  "Computes the nth \"short name\" of files to use."
  (format nil "dfile~3,'0D" n))

(defun client-long-fname (n)
  "Computes the nth \"long name\" of files to use. This should be a relative path from the base Net
   directory."
  (format nil "./temp/~A.txt" (client-short-fname n)))

(defun client-make-fname ()
  "Uses the global *client-fname-n* counter to construct a unique filename, incrementing the global
   value afterward. Note that the name returned by this function is only guaranteed to be unique as
   long as *client-fname-n* has not been tampered with."
  (check-type *client-fname-n* integer)
  (let ((name (client-long-fname *client-fname-n*)))
    (incf *client-fname-n*)
    name))

(defun client-cleanup-fnames ()
  "Cleans up any filenames that were produced by this program, from 0 up to *client-fname-n*. If
   *client-fname-n* has been artificially modified, this may not clean up all of the files."
  (check-type *client-fname-n* integer)
  (loop for i from 0 below *client-fname-n*
        do (delete-file (client-long-fname i))))

(defun handle-args-and-play (argv &aux (*port* *port*) (*socket* *socket*))
  "Prepares the system for execution based on the command line arguments given through
   argv and calls run-game when all preparations have been made. An invocation of report-updates
   is injected into the standard game callback, so that the client can begin running immediately
   rather than waiting on the user to make a move."
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
           (run-game :filename filename
                     :callback (lambda () (report-updates))
                     :gamemode 'client)
        (when (open-stream-p *socket*)
          (format *socket* "quit~%"))
        (client-cleanup-fnames)))))

(defun client-waiting-on (tag)
  "Returns true if the client is waiting on an update associated with the given tag and
   false otherwise."
  (assoc tag *client-pending* :test #'eql))

(defun client-request (sym &optional tag)
  "Makes a request for sym, associating the request globally with the given tag. If tag is
   not provided, it defaults to sym. Note that if a request going by the same tag is still
   pending, a new one will not be made. Tags are always compared by eql and are usually, but
   not always, interned symbols."
  (setf tag (or tag sym))
  (unless (client-waiting-on tag)
    (let ((worldname (client-make-fname)))
      (echo 2 "Making update request for ~A...~@[~* (tag: ~A)~]" sym (not (eql sym tag)) tag)
      (format *socket* "need ~(~A~) ~A~%" sym worldname)
      (push (list tag worldname) *client-pending*)
      (client-lock tag))))

(defun client-request-custom (expr tag)
  "Makes a request to run the expression given, using the crawing engine's command parser. The value
   of tag is used to track this request locally in the system and should be eql-comparable. If a
   request with the same tag is pending, a new one will not be made. Tags are usually symbols but
   can be any eql-comparable object."
  (unless (client-waiting-on tag)
    (let ((worldname (client-make-fname)))
      (echo 2 "Making custom request for '~A'... (tag: ~A)" expr tag) ; TODO Escape the expr for this?
      (format *socket* "goget ~A ~A~%" worldname expr)
      (push (list tag worldname) *client-pending*)
      (client-lock tag))))

(defun read-updates-from-socket (&key ((:socket *socket*) *socket*))
  "Reads as much as possible from the socket connection and uses the information to prepare
   for what to do next. Currently, the only valid information passed through this socket is
   a (completed \"filename\"), which indicates that the cycle is complete."
  (loop while (listen *socket*)
        do (let ((result (read *socket*)))
             (case (first result)
               (completed (push (second result) *client-concluding*)) ; TODO Error handling
               (t (error "Malformed update response ~A" result))))))

(defun report-checkin ()
  "Sends a checkin request to the update procedures. Note that this is superfluous but harmless if
   the update process is using the newer timeout system, in which case these requests will get
   queued with the ones generated by the timeout."
  (format *socket* "ter~%"))

(defun report-updates ()
  "Performs the various trigger checks, as described in updates.lisp, to send information to the
   update process."
  (make-update-requests))

(defun check-for-updates ()
  (loop with removing = nil
        for (sym delta) in *client-pending*
        when (member delta *client-concluding* :test #'string=)
            do (let ((*origin* delta))
                 (with-open-file (file delta)
                   (let ((result (load-and-integrate-delta :file file)))
                     (when result
                       (flush-request-pool)
                       (setf *client-concluding* (delete delta *client-concluding*))
                       (push sym removing)))))
        finally (progn
                  (loop for rr in removing
                        do (setf *client-pending*
                                 (remove rr *client-pending* :key #'first))
                        do (client-unlock rr))
                  (when removing
                    (check-for-updates)))))

(defmethod do-action :after (act obj preps)
  (declare (ignore act obj preps))
  (report-checkin)
  (report-updates)
  (when (and *client-loc* (not (eql *client-loc* (get-loc *player*))))
    (setf *client-loc* (get-loc *player*))
    (read-updates-from-socket)
    (check-for-updates))
  (when (not *client-loc*)
    (setf *client-loc* (get-loc *player*))))

(handle-args-and-play (ng-os:argv))
