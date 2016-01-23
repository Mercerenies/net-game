(in-package #:net-game)

(defstruct (fraction (:conc-name frac-))
  (numer 0 :type number)
  (denom 0 :type number))

(defun frac-value (fr)
  (if (zerop (frac-denom fr))
      0
      (/ (frac-numer fr) (frac-denom fr))))

(defparameter *real-names*
  nil)

; Formatted as a hash table of alists containing (string . count)
(defparameter *name-probabilities*
  nil)

(defparameter *name-order*
  2)

(defparameter *name-affixes*
  nil)

; Uses *name-affixes*
(defun load-affixes (&optional (fname "./data/naming_inner.txt"))
  (setf *name-affixes*
        (with-open-file (stream fname :direction :input)
          (loop with *read-eval* = nil
                for line = (read stream nil nil)
                while line
                collect line))))

(defun apply-affix (str affix)
  (format nil "~A~A~A" (car affix) str (cdr affix)))

; Uses *real-names*
(defun real-names (&optional (fname "./data/naming.txt"))
  (or *real-names*
      (setf *real-names*
            (with-open-file (stream fname :direction :input)
              (loop for line = (read-line stream nil nil)
                    while line
                    collect line)))))

; Uses *real-names*; Uses *name-probabilities* unless :store nil is provided;
; Uses *name-order* unless :order is provided
(defun load-probabilities (&key (fname "./data/naming.txt") (store t)
                             (order *name-order*))
  (loop with names = (real-names fname)
        with hash = (make-hash-table :test #'equal)
        for n in names
        do (loop for i upfrom (- order) to (- (length n) order)
                 for s0 = (let* ((ss (string-downcase (subseq n (max i 0) (+ i order))))
                                 (pos (position #\SPACE ss :from-end t)))
                            (if pos (subseq ss (1+ pos)) ss))
                 for ch1 = (if (= i (- (length n) order))
                               #\SPACE
                               (char-downcase (elt n (+ i order))))
                 for curr = (assoc ch1 (gethash s0 hash nil))
                 do (if curr
                        (incf (cdr curr))
                        (push (cons ch1 1) (gethash s0 hash))))
        finally (progn
                  (when store
                    (setf *name-probabilities* hash))
                  (return hash))))

; Uses *name-probabilities*; Uses *name-order* unless :order is provided
(defun generate-name (&key (order *name-order*) (trim nil))
  (loop with len = (+ 5 (random 7))
        with vec = (make-array len :element-type 'character :initial-element #\SPACE)
        for i below len
        for prev = (let* ((ss (subseq vec (max (- i order) 0) i))
                          (pos (position #\SPACE ss :from-end t)))
                     (if pos (subseq ss (1+ pos)) ss))
        for prob = (gethash prev *name-probabilities*)
        for sum = (reduce #'+ prob :key #'cdr)
        for choose = (when (plusp sum)
                       (random sum))
        do (setf (elt vec i) (if choose
                                 (loop for (ch . n) in prob
                                       do (setf choose (- choose n))
                                       when (minusp choose)
                                           return ch
                                       finally (return nil))
                                 (prog1 (code-char (+ (char-code #\a)
                                                      (random (- (char-code #\z)
                                                                 (char-code #\a)))))
                                   (warn "No match for ~S in generate-name"
                                         prev))))
        finally (return (let ((str (string-capitalize vec)))
                          (if trim
                              (string-trim '(#\space) str)
                              str)))))

; Uses *name-probabilities*
(defun generate-name-x (&key (order 1))
  (loop with vec = (make-array 5
                               :adjustable t
                               :fill-pointer 0
                               :element-type 'character
                               :initial-element #\SPACE)
        with words = (1+ (random 3))
        for i = (length vec)
        for prev = (let* ((ss (subseq vec (max (- i order) 0) i))
                          (pos (position #\SPACE ss :from-end t)))
                     (if pos (subseq ss (1+ pos)) ss))
        for prob = (gethash prev *name-probabilities*)
        for sum = (reduce #'+ prob :key #'cdr)
        for choose = (when (plusp sum)
                       (random sum))
        while (plusp words)
        do (let ((ch (if choose
                         (loop for (ch . n) in prob
                               do (setf choose (- choose n))
                               when (minusp choose)
                                   return ch
                               finally (return nil))
                         (prog1 (code-char (+ (char-code #\a)
                                              (random (- (char-code #\z)
                                                         (char-code #\a)))))
                           (warn "No match for ~S in generate-name"
                                 prev)))))
             (when (eql ch #\SPACE)
               (decf words))
             (vector-push-extend ch vec))
        finally (return (string-capitalize vec))))
