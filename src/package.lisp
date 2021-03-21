#| Copyright 2021 Dmitrii Korobeinikov

This file is part of slot-extra-options.

slot-extra-options is free software: you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your option)
any later version.

slot-extra-options is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
more details.

You should have received a copy of the GNU Lesser General Public License along
with slot-extra-options.  If not, see <https://www.gnu.org/licenses/>. |#

(defmacro def-extra-slot-options-package (name &body body)
  `(defpackage ,name
     (:use #:cl)

     ,@body

     (:import-from
      #:alexandria
      #:flatten
      #:compose
      #:make-keyword
      #:symbolicate
      #:with-gensyms
      #:mappend
      #:curry
      #:rcurry)

     (:import-from
      #:serapeum
      #:batches
      #:fbind
      #:eval-always)
     
     (:import-from
      #:closer-mop
      #:class-slots
      #:validate-superclass
      #:standard-direct-slot-definition
      #:standard-effective-slot-definition
      #:compute-effective-slot-definition
      #:direct-slot-definition-class
      #:effective-slot-definition-class
      #:ensure-finalized
      #:class-direct-slots
      #:class-precedence-list
      #:slot-definition-name
      #:slot-definition-readers
      #:slot-definition-writers)

     (:import-from ; not everything is here
      #:iterate
      #:iter #:for #:initially #:finally
      #:finish #:leave #:next-iteration #:next #:terminate
      #:repeat #:for #:as #:generate #:generating #:in
      #:collect #:collecting #:with #:while #:until #:appending)))

(def-extra-slot-options-package #:slot-extra-options
  (:export
   ;; error conditions
   #:slot-extra-options-error

   ;; coalesce function and the specializers (a rock band from the 70s)
   #:coalesce-options
   #:replace-or-inherit
   #:merge
   #:difference
   #:bound-only-once

   ;; metaclass
   #:slot-extra-options-class
   #:options

   ;; slot-option
   #:slot-option
   #:name
   #:initform
   #:option-type
   #:coalescence

   ;; macros
   #:def-extra-options-metaclass

   ;; convenience functions
   #:all-direct-slot-definitions
   #:all-slot-readers
   #:all-slot-writers
   #:pick-in-slot-def
   #:pick-in-slot-defs
   #:remove-from-slot-def
   #:ensure-option-in-slot-def))

;; (in-package :slot-extra-options)
