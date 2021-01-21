;;;; package.lisp

(defpackage #:slot-extra-options
  (:use #:cl #:ut)
  
  (:export
   ;; coalesce
   #:coalesce-options
   #:coalesce-replace-or-inherit
   #:coalesce-merge
   #:coalesce-subtract
   #:coalesce-bound-only-once

   ;; classes
   #:slot-option
   #:slot-extra-options-class

   ;; macros
   #:def-extra-options-metaclass)
  
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
   )

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
   #:nunioning #:unioning #:reducing #:accumulating))

(in-package :slot-extra-options)
