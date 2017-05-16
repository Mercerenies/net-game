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
