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

(asdf:defsystem #:slot-extra-options
  :version "1.0.0"
  :description "Extra options for slots using MOP."
  :author "Dmitrii Korobeinikov <dim1212k@gmail.com>"
  :homepage "https://github.com/some-mthfka/slot-extra-options"
  :license "LGPL-3.0-or-later"
  :depends-on (:alexandria
               :parachute
               :closer-mop
               :serapeum
               :iterate)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "error-conditions")
   (:file "coalesce")
   (:file "slot-extra-options")
   (:file "macro")
   (:file "tests")))
