
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
  "Returns the standard ARGV list of arguments passed into the program from the command line."
  (or #+clisp (return-from argv ext:*args*)
      (error "Unsupported CL - argv")))

(defun full-exit (status)
  "Fully exits the program immediately, using the given status value as the exit status of the program."
  (check-type status integer)
  (or #+clisp (ext:quit status)
      (error "Unsupported CL - full-exit")))

(defun connect-to-socket (port)
  "Opens a socket on the given port. After opening the socket, the standard CL functions for streams can
   be used to manage and close the socket stream."
  (check-type port integer)
  (or #+clisp (socket:socket-connect port)
      (error "Unsupported CL - connect-to-socket")))

