(in-package #:net-game)
; This is a temporary solution. I'm hoping to find a library (asdf or something) that
; does this for me.

(defun get-argv ()
  (or
   #+clisp ext:*args*
   #+sbcl sb-ext:*posix-argv*
   #+abcl ext:*command-line-argument-list*
   #+clozure (ccl::command-line-arguments)
   #+gcl si:*command-args*
   #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
   #+cmu extensions:*command-line-strings*
   #+allegro (sys:command-line-arguments)
   #+lispworks sys:*line-arguments-list*
   nil))
