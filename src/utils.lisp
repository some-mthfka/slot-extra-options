(in-package :slot-extra-options)

(defmacro itr (&rest rest)
  "Alias for `iterate:iterate'."
  `(iter ,@rest))

(defun slot-exists-and-bound-p (object slot-name)
  (and (slot-exists-p object slot-name)
       (slot-boundp object slot-name)))
