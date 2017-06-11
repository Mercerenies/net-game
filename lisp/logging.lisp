(in-package #:net-game)

(defparameter *debug-level* 0)

(define-condition net-game-warning (warning)
  ((level :reader warning-level
          :initarg :level
          :initform 1
          :type integer)
   (text :reader warning-text
         :initarg :text
         :initform ""
         :type string)))

(defmacro handling-warnings (&body body)
  (let ((cnd (gensym)))
    `(handler-bind ((net-game-warning (lambda (,cnd)
                                        (echo (warning-level ,cnd) "Warning: ~A"
                                              (warning-text ,cnd))
                                        (muffle-warning ,cnd))))
       ,@body)))

(defun echo (level text &rest format)
  (when (>= *debug-level* level)
    (format *error-output* "~D [4] ~?~%" (ng-os:pid) text format)))
