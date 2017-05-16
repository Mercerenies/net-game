(in-package #:net-game)

(defparameter *debug-level* 0)

(defun echo (level text)
  (when (>= *debug-level* level)
    (format *error-output* "~D [4] ~A~%" (ng-os:pid) text)))
