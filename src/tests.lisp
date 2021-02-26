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

;; * Commentary

;; Thoroughly test `def-extra-options-metaclass' and the coalescence methods.

;; * Code

(in-package :slot-extra-options-tests)

;; out of the test definition below to avoid warnings
(def-extra-options-metaclass options-test-metaclass
    ((replaces :initform nil)
     (subtracts :initform nil :coalescence difference)
     (merges :coalescence merge)
     (validates :coalescence bound-only-once :type (member t nil))))

(define-test slot-extra-options

  (defclass class-a ()
    ((v :replaces old-value
        :subtracts (1 2 3)
        :merges (1 2 3)
        :validates t))
    (:metaclass options-test-metaclass))

  ;; (funcall (compose #'ensure-finalized #'find-class) 'class-a)

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

  (eval ; needed to avoid warnings due to the 
   '(fbind ((first-slot (compose 'first 'class-slots 'find-class))
            (finalize (compose #'ensure-finalized #'find-class)))
     (mapcar #'finalize '(class-a class-b class-c class-d class-e))
     ;; option type error
     (fail (progn (defclass option-type-failure-class (class-a)
                    ((v :validates 0))  ; 0 is not t or nil
                    (:metaclass options-test-metaclass))
                  (finalize 'option-type-failure-class))
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
                    (finalize 'bound-only-once-failure-class))
           'slot-extra-options-error)
       (true (slot-definition-validates slot-a))
       (fail (slot-definition-validates slot-b) 'unbound-slot)
       (true (slot-definition-validates slot-c))
       (true (slot-definition-validates slot-d))
       (true (slot-definition-validates slot-e))))))

#+or
(test 'slot-extra-options)
