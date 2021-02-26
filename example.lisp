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
