(in-package #:net-game)

(defparameter *incoming-requests*
  (make-queue nil))

(defparameter *request-tags*
  nil)

(defclass request ()
  ((expr :reader request-expr
         :initarg :expr
         :initform ""
         :type string)))

(defclass request-tag ()
  ((number :reader request-tag-number
           :initarg :number
           :initform 0
           :type integer)))

(defmethod load-object ((header (eql 'request)) data)
  (let ((expr ""))
    (load-formatted data 'request
                    (ignored nil)
                    (expr-1 (setf expr expr-1)))
    (make-instance 'request :expr expr)))

(defmethod print-object ((obj request-tag) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream ":number ~D"
            (request-tag-number obj))))

(defun flush-request-pool ()
  (loop for tag in *request-tags*
        until (quest-empty-p *incoming-requests*)
        unless (client-waiting-on tag)
            do (let ((req (queue-pop *incoming-requests*)))
                 (client-request-custom (request-expr req) tag))))

(defmethod client-unlock ((tag request-tag))
  (unless (quest-empty-p *incoming-requests*)
    (let ((req (queue-pop *incoming-requests*)))
      (client-request-custom (request-expr req) tag))))
