(in-package #:net-game)

(defclass animal-data (named)
  ((pack :accessor anim-pack
         :initform 1
         :initarg :pack)
   (speed :accessor anim-speed
          :initform 1
          :initarg :speed)
   (threat :accessor anim-threat
           :initform 1
           :initarg :threat)
   (air :accessor anim-air
        :initform nil
        :initarg :air)
   (sea :accessor anim-sea
        :initform nil
        :initarg :sea)
   (size :accessor anim-size
         :initform 1
         :initarg :size)))

(defgeneric load-creature (type &rest data))

(defmethod load-creature ((type (eql 'animal)) &rest data)
  (destructuring-bind (id name . rest) data
    (apply #'make-instance 'animal-data :id id :name name rest)))
