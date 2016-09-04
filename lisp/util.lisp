(in-package #:net-game)

(defun intern-upcase (x)
  (intern (string-upcase x)))

(defun choose (args)
  "Chooses a random element from the proper sequence provided. Returns nil if the sequence is empty."
  (if (zerop (length args))
      nil
      (elt args (random (length args)))))

(defun lerp (value lower upper)
  (+ (* lower (- 1 value)) (* upper value)))
