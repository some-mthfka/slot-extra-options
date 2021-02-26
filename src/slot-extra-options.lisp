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

(in-package :slot-extra-options)

;; * References

;; The Art of the Metaobject Protocol: p.89

;; Additional properties to slot definition:
;; [[https://stackoverflow.com/questions/21986977/additional-properties-to-slot-definition]]

;; * Code

;; ** Slot Option

(defclass slot-option ()
  ((name :type symbol
         :initarg :name
         :accessor name)
   (initform :initarg :initform
             :accessor initform)
   (option-type :initform t
                :initarg :type
                :accessor option-type)
   (coalescence :type symbol
                :initform 'replace-or-inherit
                :initarg :coalescence
                :accessor coalescence))
  (:documentation "Contains information that defines an option."))

(defun make-slot-option (&rest args)
  (apply #'make-instance 'slot-option args))

(defun make-slot-option-from-definition (option-definition)
  "Make `slot-option' from OPTION-DEFINITION."
  (apply #'make-slot-option (cons :name option-definition)))

;; ** Slot Extra Options Class

(defclass slot-extra-options-class (standard-class)
  ((options :initarg :options
            :reader options
            :allocation :class))
  (:documentation "A metaclass which lets you define new slot options/keywords
for classes.  See `def-extra-options-metaclass' for usage details."))

(defmethod validate-superclass ((class slot-extra-options-class)
                                (superclass standard-class))
  t)

;; *** Computing effective slot definition

;; see p.86 for details on direct-slots
(defmethod compute-effective-slot-definition
    ((class slot-extra-options-class) slot-name direct-slots)
  "Coalesce DIRECT-SLOTS and produce a slot with custom options defined in
CLASS."
  (let ((normal-slot (call-next-method)))
    (itr (for option in (options class))
         (multiple-value-bind (value action)
             (coalesce-options (name option)
                               class
                               direct-slots
                               (coalescence option))
           (unless (member action '(bind leave-unbound))
             (error 'slot-extra-options-error
                    :message (format nil "`coalesce-options' for option ~A in
slot ~A is expected to return (values <new value of the option> <'bind or
'leave-unbound>). Please, make sure that the function you passed in :coalescence
(or `coalesce-options' if you specified it yourself) does that."
                                     (name option) slot-name)))
           (if (eql action 'bind)
               (setf (slot-value normal-slot (name option)) value)
               ;; Initform binding has to be done here, so that option may stay
               ;; unbound, if unspecified by the slot, so that coalescing
               ;; direct-slots is done correctly. Note: can't do this in
               ;; `def-extra-options-metaclass'.
               (when (slot-boundp option 'initform)
                 (setf (slot-value normal-slot (name option))
                       (initform option))))))
    normal-slot))
