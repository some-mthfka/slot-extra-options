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

(asdf:defsystem #:slot-extra-options-tests
  :version "1.0.0"
  :description "Tests for slot-extra-options."
  :author "Dmitrii Korobeinikov <dim1212k@gmail.com>"
  :homepage "https://github.com/some-mthfka/slot-extra-options"
  :license "LGPL-3.0-or-later"
  :depends-on (:alexandria
               :slot-extra-options
               :parachute
               :closer-mop
               :serapeum
               :iterate)
  ;; https://common-lisp.net/project/asdf/asdf.html#test_002dop
  ;; "we suggest defining conditions to signal when a test-op fails" => be loud
  ;; by using `interactive'.
  :perform (test-op (o s) (print (uiop:symbol-call
                                  :parachute '#:test :slot-extra-options-tests
                                  :report (uiop:find-symbol*
                                           "INTERACTIVE" 'parachute))))
  :pathname "src/tests"
  :serial t
  :components
  ((:file "package")
   (:file "tests")))
