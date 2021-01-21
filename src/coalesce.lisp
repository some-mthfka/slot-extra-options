(in-package :slot-extra-options)

;; * Code

;; ** Option coalescence methods / inheritence

(defgeneric coalesce-options (option-name metaclass-name dslots)
  (:documentation "Coalesce option OPTION-NAME, as defined in metaclass named
METACLASS-NAME, from direct slots DSLOTS. This lets you define inheritence for
custom options (see `slot-extra-options-class').
`c2mop:compute-effective-slot-definition' calls this method for each option in
turn and expects (values <new value of the option> <'bind or 'leave-unbound>).
To specify an inheritence rule for an option, you can either (1) specialize on
this class (OPTION-NAME and METACLASS-NAME being symbols) or (2) pass a function
name to `def-extra-options-class'.")
  (:method (option-name metaclass-name dslots)
    "Coalesce option OPTION-NAME in direct slots DSLOTS for metaclass
METACLASS-NAME. Default behavior is `coalesce-replace-or-inherit'."
    (coalesce-replace-or-inherit option-name dslots)))

;; *** Specific strategies

(defun coalesce-replace-or-inherit (option-name dslots)
  "Inherit option OPTION-NAME from the first of DSLOTS where it's bound."
  (itr (for dslot in dslots)
       (when (slot-boundp dslot option-name)
         (leave (values (slot-value dslot option-name) 'bind)))
       (finally (return (values nil 'leave-unbound)))))

(defun coalesce-merge (option-name dslots)
  "Merge option OPTION-NAME of all DSLOTS. Lists only (unbound treated as
NIL)."
  (values (remove-duplicates
           (itr (for dslot in dslots)
                (when (slot-boundp dslot option-name)
                  (appending (slot-value dslot option-name)))))
          'bind))

(defun coalesce-subtract (option-name dslots)
  "Set difference of first dslot's OPTION from the rest of DSLOTS, removing
duplicates. Lists only (unbound treated as NIL)."
  (values (remove-duplicates
           (when (slot-boundp (first dslots) option-name)
             (set-difference (slot-value (first dslots) option-name)
                             (coalesce-merge option-name (rest dslots))
                             :test 'equal))
           :test 'equal)
          'bind))

(defun coalesce-bound-only-once (option-name dslots)
  "Ensure that only one bound value occurs in the whole inheritence line."
  (fbind ((option-boundp (rcurry 'slot-boundp option-name)))
    (if (option-boundp (first dslots))
        (progn
          (error-on (find-if #'option-boundp (rest dslots))
                    "The inheritence rule for ~A specifies that the value may
only be bound once.  You are getting this error because one of the classes you
are inheriting from has already bound this value or if you specified an
:initform for this option in `def-extra-options-class' when defining this
class."
                    option-name)
          (values (slot-value (first dslots) option-name) 'bind))
        (values nil 'leave-unbound))))
