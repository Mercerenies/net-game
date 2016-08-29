
(defpackage #:net-game-os
  (:use :common-lisp)
  (:nicknames :ng-os)
  (:export :argv
           :full-exit
           :connect-to-socket))
(in-package #:net-game-os)

#|
 | This file consists of all of the GNU-specific code required to run the game. If you
 | wish to use another Common Lisp implementation, all you need to do is add a line to
 | each of these functions mapping the function to its implementation-specific name
 | in the implementation of your choice.
 |#

(defun argv ()
  (or #+clisp (return-from argv ext:*args*)
      (error "Unsupported CL - argv")))

(defun full-exit (status)
  (or #+clisp (ext:quit status)
      (error "Unsupported CL - full-exit")))

(defun connect-to-socket (port)
  (or #+clisp (socket:socket-connect port) ; TODO Handle the OS-ERROR if the connection fails
      (error "Unsupported CL - connect-to-socket")))

