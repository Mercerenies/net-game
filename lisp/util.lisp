(in-package #:net-game)

(defun intern-upcase (x)
  "Interns the string in a case-insensitive fashion, similar to how the Common Lisp reader
   interns atoms by default."
  (check-type x string)
  (intern (string-upcase x)))

(defun choose (args)
  "Chooses a random element from the proper sequence provided. Returns nil if the sequence is empty."
  (check-type args list)
  (if (zerop (length args))
      nil
      (elt args (random (length args)))))

(defun lerp (value lower upper)
  "Perform a linear interpolation between lower and upper, given the interpolation value of value."
  (check-type value number)
  (check-type lower number)
  (check-type upper number)
  (+ (* lower (- 1 value)) (* upper value)))

(defmacro addf (place n)
  "Perform in-place addition. (addf place n) is equivalent to (setf place (+ place n))."
  `(setf ,place (+ ,place ,n)))

(defmacro subf (place n)
  "Perform in-place subtraction. (subf place n) is equivalent to (setf place (- place n))."
  `(setf ,place (- ,place ,n)))

(defmacro appendf (place &rest args)
  `(setf ,place (append ,place ,@args)))

(defmacro prependf (place &rest args)
  `(setf ,place (append ,@args ,place)))

(defmacro collecting (&body body)
  "Accumulate values into a list by pushing onto the front or the back, returning the resulting
   list at the end."
  (let ((head (gensym))
        (tail (gensym))
        (values (gensym))
        (value (gensym))
        (temp (gensym)))
    `(let* ((,head nil)
            (,tail nil))
       (flet ((push-front (&rest ,values)
                (setf ,head (append ,values ,head))
                (when (null ,tail)
                  (setf ,tail (last ,head))))
              (push-back (&rest ,values)
                (loop for ,value in ,values
                      do (let ((,temp (cons ,value nil)))
                           (if ,tail
                             (setf (cdr ,tail) ,temp)
                             (setf ,head ,temp))
                           (setf ,tail ,temp)))))
         ,@body
         ,head))))

(defun weighted-random (weights)
  "Takes an alist of values with associated weights and produces
   one of the values in the alist at random."
  (let ((total (reduce #'+ weights :key #'cdr)))
    (if (<= total 0.0)
        nil
        (loop with random = (float (random total))
              for value in weights
              for curr = (- random (cdr value)) then (- curr (cdr value))
              until (< curr 0.0)
              finally (return (car value))))))

(defun nshuffle (vec &key (multiplier 3))
  "Shuffles the proper sequence in-place."
  (loop with len = (length vec)
        for i from 1 to (* multiplier len)
        for n = (random len)
        for m = (random len)
        do (rotatef (elt vec n) (elt vec m))
        finally (return vec)))

(defun shuffle (vec &key (multiplier 3))
  "Returns a shuffled copy of the proper sequence."
  (nshuffle (copy-seq vec) :multiplier multiplier))
