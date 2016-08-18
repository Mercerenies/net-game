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

; TODO Standardize the capitalization so we don't get things like "Golden samphire Plant"
(defun make-food (data)
  (let ((food (make-instance 'food :name (get-name data) :data data))
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

; TODO Make an inventory limit so the player can't hoard fruits from a plant
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

(defmethod do-action ((act (eql 'probe)) (obj plant) preps)
  (declare (ignore preps))
  (format t "Plant (~A) will produce Food (~A) in ~D turn~:P. ~
             The food has nutrition property ~F and poison chance ~F.~%"
          (get-name obj)
          (food-full-name (plant-food obj))
          (- (plant-growth-time obj) (plant-growth-moment obj))
          (food-nutrition (plant-food obj))
          (food-poison-chance (plant-food obj))))

(defmethod do-action ((act (eql 'examine)) (obj food) preps)
  (declare (ignore preps))
  (format t "A ~(~A~)~@[, or ~(~A~)~]. You could probably eat it.~%"
          (get-name obj)
          (if (string-equal (get-name obj) (food-full-name obj))
              nil
              (food-full-name obj))))

(defmethod do-action ((act (eql 'probe)) (obj food) preps)
  (declare (ignore preps))
  (format t "Food: ~S (restores ~D) (tags ~:S)~%"
          (get-name obj)
          (food-health obj)
          (food-tags obj)))

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
  (setf (inventory *player*) (remove obj (inventory *player*))))
