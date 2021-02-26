;;;; package.lisp

(macrolet ((defp (name &body body)
             `(defpackage ,name
                (:use #:cl)

                ,@body

                (:import-from
                 #:alexandria
                 #:compose
                 #:make-keyword
                 #:symbolicate
                 #:with-gensyms
                 #:curry
                 #:rcurry)

                (:import-from
                 #:serapeum
                 #:fbind)
                
                (:import-from
                 #:closer-mop
                 #:class-slots
                 #:validate-superclass
                 #:standard-direct-slot-definition
                 #:standard-effective-slot-definition
                 #:compute-effective-slot-definition
                 #:direct-slot-definition-class
                 #:effective-slot-definition-class
                 #:ensure-finalized)

                (:import-from ; not everything is here
                 #:iterate
                 #:iter #:for #:initially #:finally
                 #:finish #:leave #:next-iteration #:next #:terminate
                 #:repeat #:for #:as #:generate #:generating #:in
                 #:collect #:collecting #:with #:while #:until #:appending))))
  
  (defp #:slot-extra-options
      (:export
       ;; error conditions
       #:slot-extra-options-error
       
       ;; coalesce function and the specializers (a rock band from the 70s)
       #:coalesce-options
       #:replace-or-inherit
       #:merge
       #:difference
       #:bound-only-once

       ;; classes
       #:slot-option
       #:slot-extra-options-class

       ;; macros
       #:def-extra-options-metaclass))

  (defp #:slot-extra-options-tests
      (:use #:slot-extra-options)
    (:import-from
     #:parachute
     #:define-test
     #:test
     #:true
     #:false
     #:fail
     #:is)))

(in-package :slot-extra-options)
