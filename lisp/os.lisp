
(defpackage #:net-game-os
  (:use :common-lisp)
  (:nicknames :ng-os)
  (:export :argv
           :full-exit
           :connect-to-socket))
(in-package #:net-game-os)

(defun argv ()
  (or #+clisp (return-from argv ext:*args*)
      (error "Unsupported CL - argv")))

(defun full-exit (status)
  (or #+clisp (ext:quit status)
      (error "Unsupported CL - full-exit")))

(defun connect-to-socket (port)
  (or #+clisp (socket:socket-connect port) ; TODO Handle the OS-ERROR if the connection fails
      (error "Unsupported CL - connect-to-socket")))
