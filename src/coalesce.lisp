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

;; * Code

;; ** Option coalescence methods / inheritence

(defgeneric coalesce-options (option-name class dslots rule)
  (:documentation "Coalesce option OPTION-NAME of CLASS from direct slots
DSLOTS. This lets you define inheritence for custom options (see
`slot-extra-options-class').  `compute-effective-slot-definition' calls this
method for each option in turn and expects (values <new value of the option>
<'bind or 'leave-unbound>).  To define a custom inheritence rule, you can
specialize on RULE (see already existing specializations for examples)."))

;; *** Specific strategies

;; Note: `slot-exists-and-bound-p' is used and not `slot-boundp', because you
;; can inherit from a standard class which doesn't have the slot option.

(defmethod coalesce-options
    (option-name class dslots (rule (eql 'replace-or-inherit)))
  "Inherit option OPTION-NAME from the first of DSLOTS where it's bound."
  (declare (ignore class))
  (itr (for dslot in dslots)
       (when (slot-exists-and-bound-p dslot option-name)
         (leave (values (slot-value dslot option-name) 'bind)))
       (finally (return (values nil 'leave-unbound)))))

(defmethod coalesce-options (option-name class dslots (rule (eql 'merge)))
  "Merge option OPTION-NAME of all DSLOTS. Lists only (unbound treated as
NIL)."
  (declare (ignore class))
  (values (remove-duplicates
           (itr (for dslot in dslots)
                (when (slot-exists-and-bound-p dslot option-name)
                  (appending (slot-value dslot option-name)))))
          'bind))

(defmethod coalesce-options
    (option-name class dslots (rule (eql 'difference)))
  "Set difference of first dslot's OPTION from the rest of DSLOTS, removing
duplicates. Lists only (unbound treated as NIL)."
  (values (remove-duplicates
           (when (slot-exists-and-bound-p (first dslots) option-name)
             (set-difference
              (slot-value (first dslots) option-name)
              (coalesce-options option-name class (rest dslots) 'merge)
              :test 'equal))
           :test 'equal)
          'bind))

(defmethod coalesce-options
    (option-name class dslots (rule (eql 'bound-only-once)))
  "Ensure that a value is bound only once in the whole inheritence line.
Subclasses inherit that value (and cannot override it, getting an error)."
  (fbind ((option-boundp (rcurry 'slot-exists-and-bound-p option-name)))
    (let* ((inherited-value-slot (find-if #'option-boundp (rest dslots)))
           (inherited-value (when inherited-value-slot
                              (slot-value inherited-value-slot option-name))))
      (cond ((option-boundp (first dslots))
             (let ((option-value (slot-value (first dslots) option-name)))
               (restart-case
                   (when (and inherited-value-slot
                              (not (equal option-value inherited-value)))
                     (error
                      'slot-extra-options-error
                      :message
                      (format nil "The inheritence rule for ~A of class ~A
specifies that the value may only be bound once.  You are getting this error
because one of the classes you are inheriting from has already bound this value
or if you specified an :initform for this option in `def-extra-options-class'
when defining this class, and the new value ~A is different from the inherited
~A."
                              option-name class option-value inherited-value)))
                 (rebind-anyway () :report "Rebind anyway (at your own risk)"))
               (values option-value 'bind)))
            (inherited-value-slot (values inherited-value 'bind))
            (t (values nil 'leave-unbound))))))
