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

(def-extra-slot-options-package #:slot-extra-options-tests
  (:use #:slot-extra-options)
  (:import-from
   #:parachute
   #:define-test
   #:test
   #:true
   #:false
   #:fail
   #:is)
  (:export :run))
