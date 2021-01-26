(in-package :slot-extra-options)

;; * References

;; The Art of the Metaobject Protocol: p.89

;; Additional properties to slot definition:
;; [[https://stackoverflow.com/questions/21986977/additional-properties-to-slot-definition]]

;; * Code

;; ** Slot Option

(defclass/quick slot-option ()
  ((name)
   (initform)
   (coalesce-function))
  (:documentation "Contains information that defines an option."))

(defun make-slot-option-from-definition (option-definition)
  "Make `slot-option' from OPTION-DEFINITION."
  (apply #'make-slot-option (cons :name option-definition)))

;; ** Slot Extra Options Class

(defclass slot-extra-options-class (standard-class)
  ((options :initform nil
            :initarg :options
            :reader options))
  (:documentation "A metaclass which lets you define new slot options/keywords.
These options may be easily inspected and custom inheritence rules can be set
up (see `coalesce-options' for details).  For examples, see *Examples* section
at the bottom of the file where this class is defined."))

(defmethod initialize-instance :after
    ((class slot-extra-options-class) &key option-definitions)
  (setf (slot-value class 'options)
        (mapcar #'make-slot-option-from-definition option-definitions)))

(defmethod c2mop:validate-superclass ((class slot-extra-options-class)
                                      (superclass standard-class))
  t)

;; *** Computing effective slot definition

;; see p.86 for details on direct-slots
(defmethod c2mop:compute-effective-slot-definition
    ((class slot-extra-options-class) slot-name direct-slots)
  "Coalesce DIRECT-SLOTS and produce a slot with custom options defined in
CLASS."
  (let ((normal-slot (call-next-method)))
    (itr (for option in (options class))
         (mvbind (value action)
             (coalesce-options (name option) (type-of class) direct-slots)
           (unless (member action '(bind leave-unbound))
             (error 'slot-extra-options-error "`coalesce-options' for option ~A
in slot ~A is expected to return (values <new value of the option> <'bind or
'leave-unbound>). Please, make sure that the function you passed in
:coalesce-function (or `coalesce-options' if you specified it yourself) does
that."
                    (name option) slot-name))
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
