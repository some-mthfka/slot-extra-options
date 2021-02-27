#| Copyright 2021 Dmitrii Korobeinikov

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the “Software”), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. |#

;; * Commentary

;; Example code for README.org. It's not loaded by the system, is meant for
;; demonstration of some concepts and contains deliberate errors.

;; ** Code

(in-package :slot-extra-options-tests)

;; ** Example

;; *** Metaclass

(def-extra-options-metaclass example-metaclass
    ((start :initform nil ; `:coalescence', by default, is `replace-or-inherit'
            :type integer)
     (validates :coalescence bound-only-once
                :type boolean)))

;; *** Class: `alpha'

;; Note: specifying a non-integer for `start' would yield a type error (if
;; safety is 3 on SBCL).
(defclass alpha ()
  ((zulu :initform 22
         :start 55
         :validates t))
  (:metaclass example-metaclass))

;; not strictly necessary, this is automatically done when an object is instantiated:
(c2mop:ensure-finalized (find-class 'alpha))

(let ((zulu-definition (find 'zulu (c2mop:class-slots (find-class 'alpha))
                             :key #'c2mop:slot-definition-name)))
  (values (slot-definition-start zulu-definition)
          (slot-definition-validates zulu-definition)))
;; => 55, T

;; *** Class: `beta', which inherits from `alpha'

(defclass beta (alpha)
  ((zulu :start 66))
  (:metaclass example-metaclass))

(c2mop:ensure-finalized (find-class 'beta))

(let ((zulu-definition (find 'zulu (c2mop:class-slots (find-class 'beta))
                             :key #'c2mop:slot-definition-name)))
  (values (slot-definition-start zulu-definition)
          (slot-definition-validates zulu-definition)))
;; => 66, T

;; *** Wrong types

(defclass wrong-types () ; ERROR! WON'T COMPILE!
  ((v :start nil
      :validates 0))
  (:metaclass example-metaclass))

;; *** Wrong `bound-only-once' redifinition

(defclass wrong-inheritence (alpha)
  ((zulu :validates nil))
  (:metaclass example-metaclass))

;; `:validates' can't be overriden once bound
(make-instance 'wrong-inheritence) ; ERROR! WON'T COMPILE!
