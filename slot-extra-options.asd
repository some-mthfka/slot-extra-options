(asdf:defsystem #:slot-extra-options
  :description "Extra options for slots using MOP."
  :author "Dmitrii Korobeinikov <dim1212k@gmail.com>"
  :license "MIT"
  :depends-on (:alexandria
               :parachute
               :ut
               :closer-mop
               :serapeum
               :iterate)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "coalesce")
   (:file "slot-extra-options")
   (:file "macro")))
