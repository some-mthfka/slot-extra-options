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

(defmacro test (name &body body)
  (with-gensyms (result extra)
    `(progn (eval-when (:compile-toplevel :load-toplevel)
              (define-test ,name ,@body))
            (eval-when (:execute)
              (multiple-value-bind (,result ,extra)
                  (define-test+run ,name ,@body)
                (if (eql :passed (parachute:status ,result))
                    ,result
                    (values (list :failed (length ,extra))
                            (first ,extra))))))))

(test metaclass-inheritence
  (def-extra-options-metaclass metaclass-a ()
    ((a :type integer :coalescence bound-only-once)
     (b :type char)))
  (def-extra-options-metaclass metaclass-b ()
    ((a :coalescence difference)
     (c)))
  (def-extra-options-metaclass metaclass-c (metaclass-b)
    ((a :type list)
     (b :initform 22)))
  (def-extra-options-metaclass metaclass-d (metaclass-a metaclass-c)
    ((b :initform 25)))
  (def-extra-options-metaclass metaclass-e (metaclass-c metaclass-a)
    ())
  (macrolet ((with-options (sym metaclass-name &body body)
               `(let ((,sym (slot-extra-options::options (c2mop:class-prototype
                                                          (find-class ',metaclass-name)))))
                  (is eql ,(length body) (length ,sym))
                  ,@body))
             (check-option (name options &key (option-type t)
                                           (coalescence 'replace-or-inherit)
                                           (initform nil initform-p))
               (with-gensyms (option-sym)
                 `(let* ((,option-sym (find ',name ,options :key #'name)))
                    (is eql ,option-type (option-type ,option-sym))
                    (is eql ',coalescence (coalescence ,option-sym))
                    ,(if initform-p
                         `(is eql ,initform (initform ,option-sym))
                         `(false (slot-boundp ,option-sym 'initform)))))))
    (with-options m metaclass-a
      (check-option a m :option-type 'integer :coalescence bound-only-once)
      (check-option b m :option-type 'char))
    (with-options m metaclass-b
      (check-option a m :coalescence difference)
      (check-option c m))
    (with-options m metaclass-c
      (check-option a m :option-type 'list :coalescence difference)
      (check-option b m :initform 22)
      (check-option c m))
    (with-options m metaclass-d
      (check-option a m :option-type 'integer :coalescence bound-only-once)
      (check-option b m :option-type 'char :initform 25)
      (check-option c m))
    (with-options m metaclass-e
      (check-option a m :option-type 'list :coalescence difference)
      (check-option b m :option-type t :initform 22)
      (check-option c m))))

;; out of the test definition below to avoid warnings
(def-extra-options-metaclass options-test-metaclass ()
  ((replaces :initform nil)
   (subtracts :initform nil :type list :coalescence difference)
   (merges :type list :coalescence merge)
   (validates :coalescence bound-only-once :type boolean)))

(test slot-extra-options
  ;; CCL and ECL (but not SBCL which works just fine) error out on the passes
  ;; past the first one here because of `bound-only-once-failure-class'. I don't
  ;; know how to circumvent this properly, trying to redefine it correctly
  ;; doesn't help, `makunbound' doesn't either.  Redefining a superclass just
  ;; keeps running the slot computation code based on the stale definition.
  ;; Therefore this setf klutch here:
  (setf (find-class 'class-a) nil)
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

  ;; metaclass'ed classes should inherit from non-optioned classes just fine, so
  ;; just throw this in there
  (defclass non-options-class ()
    ((v)))

  (defclass class-c (class-a non-options-class)
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

  (eval ; needed because the following needs to be in a seperate macro pass
   '(fbind ((first-slot (compose 'first 'class-slots 'find-class))
            (finalize (compose #'ensure-finalized #'find-class)))
     (mapcar #'finalize '(class-a class-b class-c class-d class-e))
     ;; option type error
     (fail (progn (defclass option-type-failure-class (class-a)
                    ((v :merges 0))     ; 0 is not a list
                    (:metaclass options-test-metaclass))
                  (finalize 'option-type-failure-class)) 'type-error)
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
       (true (slot-definition-validates slot-e))
       ;; slot-option-<>-changed-p tests
       (ensure-finalized-precedence (find-class 'class-d))
       ;; unbound stays unbound -> nothing changes
       (true (slot-option-direct-changed-p
              (find-class 'class-b) 'v 'replaces #'slot-definition-replaces))
       (true (slot-option-effective-changed-p
              (find-class 'class-b) 'v 'replaces #'slot-definition-replaces))
       (false (slot-option-direct-changed-p
               (find-class 'class-e) 'v 'replaces #'slot-definition-replaces))
       (false (slot-option-effective-changed-p
               (find-class 'class-e) 'v 'replaces #'slot-definition-replaces))
       (true (slot-option-direct-changed-p ; :validates is nil by default
              (find-class 'class-c) 'v 'validates #'slot-definition-validates))
       (false (slot-option-effective-changed-p
               (find-class 'class-c) 'v 'validates #'slot-definition-validates))
       ;; unbound -> value is considered a change
       (true (slot-option-direct-changed-p
              (find-class 'class-a) 'v 'subtracts #'slot-definition-subtracts))
       (true (slot-option-effective-changed-p
              (find-class 'class-a) 'v 'subtracts #'slot-definition-subtracts))
       (true (slot-option-direct-changed-p
              (find-class 'class-c) 'v 'subtracts #'slot-definition-subtracts))
       (true (slot-option-effective-changed-p
              (find-class 'class-c) 'v 'subtracts #'slot-definition-subtracts))
       (true (slot-option-direct-changed-p
              (find-class 'class-d) 'v 'subtracts #'slot-definition-subtracts))
       (true
        (slot-option-effective-changed-p
         (find-class 'class-d) 'v 'subtracts 'slot-definition-subtracts))))))
