(in-package :slot-extra-options)

;; * Code

(defmacro def-extra-options-metaclass
    (name option-definitions &rest defclass-options)
  "Define metaclass NAME from OPTION-DEFINITIONS while inheriting from
`slot-extra-options-class'.  OPTION-DEFINITIONS are used to construct
`slot-option's.  The format for an option definition is (name [:initform
<initform>] [:coalescence <rule specializer>] [:type <type of option>]).  The
custom options you define may be easily inspected (after the class has been
finalized) just like regular options (aka `slot-definition-initform'): with
slot-definition-<option-name>, which yields the desired `slot-option'.  Slot
options may have custom inheritence rules - see `coalesce-options' for details."
  (flet ((make-slot-definition (option)
           (list
            (name option)
            :type (option-type option)
            :initarg (make-keyword (name option))
            :reader (symbolicate 'slot-definition- (name option)))))
    (let* ((dsd (symbolicate name '-direct-slot-definition))
           (esd (symbolicate name '-effective-slot-definition))
           (options (mapcar #'make-slot-option-from-definition
                            option-definitions))
           (slots (mapcar #'make-slot-definition options)))
      `(progn
         (defclass ,name (slot-extra-options-class)
           ((options :initform (mapcar #'make-slot-option-from-definition
                                       ',option-definitions)))
           ,@defclass-options)
         (defclass ,dsd (standard-direct-slot-definition)
           ,slots)
         (defclass ,esd (standard-effective-slot-definition)
           ,slots)
         (defmethod direct-slot-definition-class
             ((class ,name) &rest initargs)
           (declare (ignore class initargs))
           (find-class ',dsd))
         (defmethod effective-slot-definition-class
             ((class ,name) &rest initargs)
           (declare (ignore class initargs))
           (find-class ',esd))
         (find-class ',name)))))
