(in-package #:net-game)

(defclass food-data (named)
  ((full-name :accessor food-full-name
              :initform ""
              :initarg :full-name
              :type string)
   (plant-type :accessor food-plant-type
               :initform nil
               :initarg :plant-type
               :type symbol)
   (nutrition :accessor food-nutrition
              :initform 1.0
              :initarg :nutritional-value
              :type number)
   (poison-chance :accessor food-poison-chance
                  :initform 0.0
                  :initarg :poison-chance
                  :type number)))

(defclass plant (named located)
  ((type :accessor plant-type
         :initform nil
         :initarg :type
         :type symbol)
   (food :accessor plant-food
         :initform nil
         :initarg :food
         :type (or null food-data))
   (growth-moment :accessor plant-growth-moment
                  :initform 0
                  :type integer)
   (growth-time :accessor plant-growth-time
                :initform 5
                :initarg :growth-time
                :type integer)))

(defclass food (item)
  ((data :accessor food-data
         :initform nil
         :initarg :data
         :type (or null food-data))
   (restore :accessor food-health
            :initform 0
            :initarg :health
            :type number)
   (tags :accessor food-tags
         :initform nil
         :initarg :tags
         :type list)))

(defmethod food-full-name ((obj food))
  (food-full-name (food-data obj)))

(defmethod food-plant-type ((obj food))
  (food-plant-type (food-data obj)))

(defun make-plant (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'plant :name name keys))

(defun make-food-data (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'food-data :name name keys))

(defun make-food (data)
  (let ((food (make-instance 'food
                             :name (get-name data)
                             :data data
                             :weight (choose '(4 5 5 6 6 6 7 7 8))))
        (restore (lerp (/ (1- (food-nutrition data)) 2) 3.0 20.0))
        (poisoned (< (random 1.0) (food-poison-chance data))))
    (setf restore (+ (random 10.0) restore -5))
    (when (<= restore 1.0)
      (setf restore 1.0))
    (when (< (random 1.0) 0.05)
      (push 'fresh (food-tags food))
      (setf restore (+ 10 restore))) ; 5% chance of boosted restore power
    (setf (food-health food)
          (if poisoned
              (/ restore -2)
              restore))
    (when poisoned
      (push 'poison (food-tags food)))
    food))

(defmethod entity-turn ((obj plant))
  (let ((loc (get-loc obj))
        (data (plant-food obj)))
    (unless (find-if (lambda (x) (and (typep x 'food)
                                      (eql (food-data x) data))) (location-contents loc))
      (incf (plant-growth-moment obj))
      (when (>= (plant-growth-moment obj) (plant-growth-time obj))
        (setf (plant-growth-moment obj) 0)
        (move-object (make-food data) loc)))))

(defmethod do-action ((act (eql 'examine)) (obj plant) preps)
  (declare (ignore preps))
  (format t "A ~(~A~) that ~[has almost ripe fruit growing~;will soon bear fruit~:;will likely bear ~
             fruit at some point~].~%"
          (get-name obj)
          (floor (/ (- (plant-growth-moment obj) (plant-growth-time obj)) 2))))

(defmethod system-keys append ((obj plant))
  (let ((food (plant-food obj)))
    `((food-full-name "Full Name" ,(food-full-name food))
      (plant-growth-moment "Current Growth" ,(plant-growth-moment obj))
      (plant-growth-time "Total Growth Time" ,(plant-growth-time obj))
      (nil "Remaining Growth Time" ,(- (plant-growth-time obj) (plant-growth-moment obj)))
      (food-nutrition "Nutritional Value" ,(food-nutrition food))
      (food-poison-chance "Poison Chance" ,(food-poison-chance food)))))

(defmethod do-action ((act (eql 'examine)) (obj food) preps)
  (declare (ignore preps))
  (format t "A ~(~A~)~@[, or ~(~A~)~]. You could probably eat it. It weighs ~A units.~%"
          (get-name obj)
          (if (string-equal (get-name obj) (food-full-name obj))
              nil
              (food-full-name obj))
          (item-weight obj)))

(defmethod system-keys append ((obj food))
  `((food-health "Health Restored" ,(food-health obj))
    (food-tags "Tags" ,(food-tags obj))))

(defmethod is-trivial ((act (eql 'eat)) (obj food) preps)
  nil)

(defmethod do-action ((act (eql 'eat)) (obj food) preps)
  (declare (ignore preps))
  (format t "You eat the ~(~A~).~%"
          (get-name obj))
  (when (find 'poison (food-tags obj))
    (format t "You feel an unsettling feeling in your stomach...~%"))
  (format t "~:[Gained~;Lost~] ~D HP~%"
          (minusp (food-health obj))
          (abs (food-health obj)))
  (setf (hp *player*) (+ (hp *player*) (/ (food-health obj) 100)))
  (when (> (hp *player*) 1.00)
    (setf (hp *player*) 1.00))
  (move-object obj nil)
  (remove-item obj *player*))
