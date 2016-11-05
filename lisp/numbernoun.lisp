(in-package #:net-game)

(defparameter *numerical* nil)

; Uses *numerical*
(defun assign-numbers (&rest lists)
  "Assigns numbers to every element in each of the lists. Sets the
   global *numerical* variable to be an alist of numbers associated
   with objects."
  (setf *numerical*
        (loop for elem in (apply #'concatenate 'list lists)
              for i upfrom 1
              collect (cons i elem))))

(defun get-number (obj &key ((:numerical *numerical*) *numerical*))
  "Looks up the number for the given object in *numerical*, or the
   keyword argument given."
  (car (rassoc obj *numerical*)))

(defun get-formatted-number (obj &key ((:numerical *numerical*) *numerical*))
  "Formats a string consisting of the corresponding number for the given object,
   padded to a length to match the longest number in *numerical*."
  (let ((len (loop for (n . o) in *numerical*
                   maximize (length (write-to-string n)))))
    (and (get-number obj)
         (format nil "~v,'0D" len (get-number obj)))))

(defun get-formatted-numbers (&key ((:numerical *numerical*) *numerical*))
  "Gets a list of formatted numbers for each object in *numerical*."
  (let ((len (loop for (n . o) in *numerical*
                   maximize (length (write-to-string n)))))
    (loop for (n . o) in *numerical*
          collect (format nil "~v,'0D" len n))))

(defun get-numerical-object (num &key ((:numerical *numerical*) *numerical*))
  "Given a number, get the object associated with the number in *numerical*."
  (when (stringp num)
    (setf num (parse-integer num :junk-allowed t)))
  (cdr (assoc num *numerical*)))

(defun get-numbered-name (obj &key ((:numerical *numerical*) *numerical*))
  "Format a line of text containing the number associated with the object and
   the object's name. The object given should be a named object."
  (format nil "~A~@[ (~A)~]"
          (if (typep obj 'location)
              (location-short-name obj)
              (get-name obj))
          (get-formatted-number obj)))
