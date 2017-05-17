(in-package #:net-game)

(defparameter *debug-level* 0)

(defun echo (level text &rest format)
  (when (>= *debug-level* level)
    (format *error-output* "~D [4] ~?~%" (ng-os:pid) text format)))
