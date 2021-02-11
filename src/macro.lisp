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
         (defclass ,dsd (c2mop:standard-direct-slot-definition)
           ,slots)
         (defclass ,esd (c2mop:standard-effective-slot-definition)
           ,slots)
         (defmethod c2mop:direct-slot-definition-class
             ((class ,name) &rest initargs)
           (declare (ignore class initargs))
           (find-class ',dsd))
         (defmethod c2mop:effective-slot-definition-class
             ((class ,name) &rest initargs)
           (declare (ignore class initargs))
           (find-class ',esd))
         (find-class ',name)))))

;; ** Test 

(in-package :slot-extra-options-tests)

(def-extra-options-metaclass options-test-metaclass
    ((replaces :initform nil)
     (subtracts :initform nil :coalescence slot-extra-options:difference)
     (merges :coalescence merge)
     (validates :coalescence bound-only-once :type (member t nil))))

(defclass class-a ()
  ((v :replaces old-value
      :subtracts (1 2 3)
      :merges (1 2 3)
      :validates t))
  (:metaclass options-test-metaclass))

;; (funcall (compose #'c2mop:ensure-finalized #'find-class) 'class-a)

(defclass class-b ()
  ((v :subtracts (3 4 5)))
  (:metaclass options-test-metaclass))

(defclass class-c (class-a)
  ((v :replaces new-value
      :subtracts (0 1 2 3 4 5 6 7)
      :merges (0 1 2)))
  (:metaclass options-test-metaclass))

(defclass class-d (class-b class-c)
  ((v :subtracts (0 1 2 3 4 5 6 7 8)))
  (:metaclass options-test-metaclass))

(defclass class-e (class-b)
  ((v :validates t))
  (:metaclass options-test-metaclass))

(test slot-extra-options
  (fbind ((first-slot (compose 'first 'c2mop:class-slots 'find-class))
          (finalize (compose #'c2mop:ensure-finalized #'find-class)))
    (mapcar #'finalize '(class-a class-b class-c class-d class-e))
    ;; option type error
    (fail (progn (defclass option-type-failure-class (class-a)
                   ((v :validates 0)) ; 0 is not t or nil
                   (:metaclass options-test-metaclass))
                 (finalize 'class-validates-fail))
        'type-error)    
    (let ((slot-a (first-slot 'class-a))
          (slot-b (first-slot 'class-b))
          (slot-c (first-slot 'class-c))
          (slot-d (first-slot 'class-d))
          (slot-e (first-slot 'class-e)))
      ;; coalesce-replace-or-inherit
      (is eql (slot-definition-replaces slot-a) 'old-value)
      (is eql (slot-definition-replaces slot-c) 'new-value)
      (is eql (slot-definition-replaces slot-d) 'new-value)
      ;; difference
      (is equal (sort (copy-list (slot-definition-subtracts slot-c)) '<)
          '(0 4 5 6 7))
      (is equal (slot-definition-subtracts slot-d) '(8))
      ;; coalesce-merge
      (false (slot-definition-merges slot-b))
      (is equal (slot-definition-merges slot-c) '(0 1 2 3))
      (is equal (slot-definition-merges slot-d) '(0 1 2 3))
      ;; coalesce-bound-only-once
      (fail (progn (defclass bound-only-once-failure-class (class-a)
                     ((v :validates nil)) ; can't replace value
                     (:metaclass options-test-metaclass))
                   (finalize 'class-validates-fail))
          'slot-extra-options-error)
      (true (slot-definition-validates slot-a))
      (fail (slot-definition-validates slot-b) 'unbound-slot)
      (true (slot-definition-validates slot-c))
      (true (slot-definition-validates slot-d))
      (true (slot-definition-validates slot-e)))))
