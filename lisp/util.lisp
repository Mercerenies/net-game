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
  `(setf ,place (+ ,place ,n)))

(defmacro subf (place n)
  `(setf ,place (- ,place ,n)))
