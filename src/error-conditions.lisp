(in-package :slot-extra-options)

(define-condition slot-extra-options-error (error)
  ((message :initarg :message :initform nil :reader message))
  (:report (lambda (condition stream)
             (format stream (message condition)))))
