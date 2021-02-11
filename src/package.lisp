;;;; package.lisp

(macrolet ((defp (name &body body)
             `(defpackage ,name
                (:use #:cl #:ut)

                ,@body
                
                (:import-from
                 #:alexandria
                 #:when-let*
                 #:when-let
                 #:switch
                 #:compose
                 #:mappend
                 #:make-keyword
                 #:symbolicate
                 #:ensure-symbol
                 #:shuffle
                 #:with-gensyms
                 #:iota
                 #:curry
                 #:rcurry)

                (:import-from
                 #:serapeum
                 #:assort
                 #:fbind
                 #:fbindrec
                 #:take
                 #:batches
                 #:partitions
                 #:partition
                 #:concat
                 #:defalias)
                
                (:import-from
                 #:closer-mop
                 #:class-slots
                 #:slot-definition-name
                 #:slot-definition-type
                 #:slot-definition-initform
                 #:slot-definition-location)

                (:import-from
                 #:iterate
                 #:iterate #:display-iterate-clauses
                 #:defsynonym #:dsetq #:declare-variables
                 #:defmacro-clause #:defmacro-driver #:defclause-sequence
                 #:initially #:after-each #:finally #:finally-protected
                 #:else #:if-first-time #:first-iteration-p #:first-time-p
                 #:finish #:leave #:next-iteration #:next #:terminate
                 #:repeat #:for #:as #:generate #:generating #:in
                 #:summing #:multiplying
                 #:maximizing #:minimizing #:counting
                 #:always #:never #:thereis #:finding #:collect #:collecting
                 #:with #:while #:until #:adjoining #:nconcing #:appending
                 #:nunioning #:unioning #:reducing #:accumulating))))

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
     #:true
     #:false
     #:fail
     #:is
     #:isnt
     #:is-values
     #:isnt-values
     #:of-type
     ;; #:finish ;; conflicts with iterate:finish, aliased `escape' instead
     )))

(in-package :slot-extra-options)
