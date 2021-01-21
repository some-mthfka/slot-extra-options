(in-package :slot-extra-options)

;; * Code

(defmacro def-extra-options-metaclass (name option-definitions)
  "Define metaclass NAME from OPTION-DEFINITIONS (see
`slot-extra-options-class').  OPTION-DEFINITIONS are used to construct
`slot-option's and are unevaluated.  The format for an option definition is
(name <:initform initform> <:coalesce-function function>). For details on
:coalesce-function, look up `coalesce-options'."
  (flet ((make-slot-definition (option)
           (list
            (name option)
            :initarg (make-keyword (name option))
            :reader (symbolicate 'slot-definition- (name option)))))
    (let* ((dsd (symbolicate name '-direct-slot-definition))
           (esd (symbolicate name '-effective-slot-definition))
           (options (mapcar #'make-slot-option-from-definition
                            option-definitions))
           (slots (mapcar #'make-slot-definition options)))
      `(progn
         (defclass ,name (slot-extra-options-class)
           ()
           (:default-initargs :option-definitions ',option-definitions))
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
         ,@(itr (for option in options)
                (when (slot-boundp option 'coalesce-function)
                  (collect
                      `(defmethod coalesce-options
                           ((option-name (eql ',(name option)))
                            (metaclass-name (eql ',name))
                            dslots)
                         (,(coalesce-function option) option-name dslots)))))
         (find-class ',name)))))

;; ** Test 

(test slot-extra-options

  (def-extra-options-metaclass options-test-metaclass
      ((replaces :initform nil)
       (subtracts :initform nil :coalesce-function coalesce-subtract)
       (merges :coalesce-function coalesce-merge)
       (validates :coalesce-function coalesce-bound-only-once)))

  (defclass class-a ()
    ((v :replaces old-value
        :subtracts (1 2 3)
        :merges (1 2 3)
        :validates t))
    (:metaclass options-test-metaclass))

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

  (fbind ((first-slot (compose 'first 'c2mop:class-slots 'find-class)))
    (mapcar (compose #'c2mop:ensure-finalized #'find-class)
            '(class-a class-b class-c class-d class-e))
    (let ((slot-a (first-slot 'class-a))
          (slot-b (first-slot 'class-b))
          (slot-c (first-slot 'class-c))
          (slot-d (first-slot 'class-d))
          (slot-e (first-slot 'class-e)))
      ;; coalesce-replace-or-inherit
      (is eql (slot-definition-replaces slot-a) 'old-value)
      (is eql (slot-definition-replaces slot-c) 'new-value)
      (is eql (slot-definition-replaces slot-d) 'new-value)
      ;; coalesce-subtract
      (is equal (sort (copy-list (slot-definition-subtracts slot-c)) '<)
          '(0 4 5 6 7))
      (is equal (slot-definition-subtracts slot-d) '(8))
      ;; coalesce-merge
      (false (slot-definition-merges slot-b))
      (is equal (slot-definition-merges slot-c) '(0 1 2 3))
      (is equal (slot-definition-merges slot-d) '(0 1 2 3))
      ;; coalesce-bound-only-once
      (true (slot-definition-validates slot-a))
      (fail (slot-definition-validates slot-b) 'unbound-slot)
      (fail (slot-definition-validates slot-c) 'unbound-slot)
      (fail (slot-definition-validates slot-d) 'unbound-slot)
      (true (slot-definition-validates slot-e)))))
