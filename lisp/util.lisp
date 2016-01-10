
(defun choose (args)
  "Chooses a random element from the proper sequence provided. Returns nil if the sequence is empty."
  (if (zerop (length args))
      nil
      (elt args (random (length args)))))
