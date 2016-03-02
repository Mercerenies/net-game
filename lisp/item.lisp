(in-package #:net-game)

(defclass item (named located)
  ())

(defclass weapon (item)
  ((type :accessor weapon-type
         :initarg :type
         :initform nil
         :type symbol)))
