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

;; * Code

(in-package :slot-extra-options)

;; ** Utils

(defmacro itr (&rest rest)
  "Alias for `iterate:iterate'."
  `(iter ,@rest))

(defun slot-exists-and-bound-p (object slot-name)
  (and (slot-exists-p object slot-name)
       (slot-boundp object slot-name)))

;; ** Extras -- not used in the project, just exported

;; *** Working with direct slots

(defun all-direct-slot-definitions (class slot-name)
  "Get all slot definition of SLOT-NAME in the precedence list of CLASS, ordered
as they come in the precedence list.  CLASS must be finalized."
  (remove-if-not (curry #'eql slot-name)
                 (flatten (mapcar #'class-direct-slots
                                  (class-precedence-list class)))
                 :key #'slot-definition-name))

(defun all-slot-readers (class slot-name)
  "All slot readers gathered from the direct definitions of the precedence list
of CLASS.  CLASS must be finalized."
  (loop for slotd in (all-direct-slot-definitions class slot-name)
        appending (slot-definition-readers slotd)))

(defun all-slot-writers (class slot-name)
  "All slot writers gathered from the direct definitions of the precedence list
of CLASS.  CLASS must be finalized."
  (loop for slotd in (all-direct-slot-definitions class slot-name)
        appending (slot-definition-writers slotd)))

;; *** Working with slot definitions

(defun pick-in-slot-def (slot-def key)
  "Return a list of all options specified by KEY in the slot definition
SLOT-DEF, ordered as they come in there.  Example: (pick-in-slot-def
'(zulu :initform 0 :reader z0 :reader z1) :reader) => (z0 z1)."
  (mapcar #'second (remove-if-not (curry #'eql key)
                                  (batches (rest slot-def) 2)
                                  :key #'first)))

(defun pick-in-slot-defs (slot-defs key)
  "Return a list of all options specified by KEY in SLOT-DEFS.  See
`pick-in-slot-def'."
  (mappend (rcurry #'pick-in-slot-def key) slot-defs))

(defun remove-from-slot-def (slot-def &rest keys)
  "Remove all options specified by KEY from slot definition SLOT-DEF."
  (list* (first slot-def)
         (apply #'append (remove-if (rcurry #'member keys)
                                    (batches (rest slot-def) 2)
                                    :key #'first))))

;; (remove-from-slot-def
;;  '(zulu :initform 0 :reader z0 :reader z1 :writer zw :reader z2)
;;  :reader :initform)

(defun remove-from-slot-defs (slot-defs &rest keys)
  "Remove all options specified by KEY from slot deifinitions SLOT-DEFS.  See
`remove-from-slot-def'."
  (mapcar (lambda (x) (apply #'remove-from-slot-def x keys))
          slot-defs))

;; (remove-from-slot-defs
;;  '((zulu :initform 0 :reader z0 :reader z1 :writer zw :reader z2)
;;    (zulu :initform 0 :reader z0 :reader z1 :writer zw :writer zw2 :reader z2)
;;    (zulu :initform 0 :reader z0 :reader z1 :reader z2))
;;  :reader :initform :type)

(defun ensure-option-in-slot-def (slot-def key &optional (default nil defaultp))
  "Ensure that key is present in slot definition SLOT-DEF, and, if not, place it
there with DEFAULT.  Or error if default is not supplied."
  (cond ((pick-in-slot-def slot-def key) slot-def)
        (defaultp (append slot-def (list key default)))
        (t (error "Key ~a was not found in ~a and no default was specified."
                  key slot-def))))
