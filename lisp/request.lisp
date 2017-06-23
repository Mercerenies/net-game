(in-package #:net-game)

(defparameter *incoming-requests*
  (make-queue nil))

(defparameter *request-tags*
  nil)

(defclass request ()
  ((expr :reader request-expr
         :initarg :expr
         :initform ""
         :type (or string list))))

(defclass request-tag ()
  ((number :reader request-tag-number
           :initarg :number
           :initform 0
           :type integer)))

(defgeneric client-lock (tag)
  (:documentation "This method is called when a tag is being locked in order to make a request.
                   Most tags needn't respond to this, as the locking operation is done
                   automatically regardless of what this method does, but some will change their
                   internal state to represent the lock."))

(defgeneric client-unlock (tag)
  (:documentation "This method is called when a request is finished and the tag is being unlocked.
                   The method client-unlock will be called after a corresponding client-lock call.
                   This method will not be called if the program terminates while a request is
                   in progress."))

(defmethod client-lock ((tag t))
  nil)

(defmethod client-unlock ((tag t))
  nil)

(defun make-request-tag (number)
  (make-instance 'request-tag :number number))

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

(defun initialize-request-pool (&optional (size 3))
  (setf *request-tags*
        (loop for i from 1 to size
              collect (make-request-tag i))))

(defun flush-request-pool ()
  (unless (eql *game-mode* 'client)
    (unless (queue-empty-p *incoming-requests*)
      (warn 'net-game-warning
            :level 1
            :text (format nil "Received request object in ~S mode..." *game-mode*)))
    (return-from flush-request-pool nil))
  (loop for tag in *request-tags*
        until (queue-empty-p *incoming-requests*)
        unless (client-waiting-on tag)
            do (let ((req (queue-pop *incoming-requests*)))
                 (client-request-custom (request-expr req) tag))))

(defmethod client-unlock ((tag request-tag))
  (unless (queue-empty-p *incoming-requests*)
    (let ((req (queue-pop *incoming-requests*)))
      (client-request-custom (request-expr req) tag))))
