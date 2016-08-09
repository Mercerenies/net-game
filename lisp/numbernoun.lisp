(in-package #:net-game)

; Uses *numerical*
(defun assign-numbers (&rest lists)
  (setf *numerical*
        (loop for elem in (apply #'concatenate 'list lists)
              for i upfrom 1
              collect (cons i elem))))

(defun get-number (obj &key (*numerical* *numerical*))
  (car (rassoc obj *numerical*)))

(defun get-formatted-number (obj &key (*numerical* *numerical*))
  (let ((len (loop for (n . o) in *numerical*
                   maximize (length (write-to-string n)))))
    (and (get-number obj)
         (format nil "~v,'0D" len (get-number obj)))))

(defun get-formatted-numbers (&key (*numerical* *numerical*))
  (let ((len (loop for (n . o) in *numerical*
                   maximize (length (write-to-string n)))))
    (loop for (n . o) in *numerical*
          collect (format nil "~v,'0D" len n))))

(defun get-numerical-object (num &key (*numerical* *numerical*))
  (when (stringp num)
    (setf num (parse-integer num :junk-allowed t)))
  (cdr (assoc num *numerical*)))

(defun get-numbered-name (obj &key (*numerical* *numerical*))
  (format nil "~A~@[ (~A)~]"
          (if (typep obj 'location)
              (location-short-name obj)
              (get-name obj))
          (get-formatted-number obj)))
