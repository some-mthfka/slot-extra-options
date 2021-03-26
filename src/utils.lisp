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

;; *** Finding effective/direct slots in classes

(flet ((f (get-slots slot-name class-or-class-name)
         (find slot-name (funcall get-slots
                                  (if (symbolp class-or-class-name)
                                      (find-class class-or-class-name)
                                      class-or-class-name))
               :key #'slot-definition-name)))
  (defun find-slot (slot-name class-or-class-name)
    "Find effective slot named SLOT-NAME in CLASS-OR-CLASS-NAME."
    (f #'class-slots slot-name class-or-class-name))
  (defun find-dslot (slot-name class-or-class-name)
    "Find direct slot named SLOT-NAME in CLASS-OR-CLASS-NAME."
    (f #'class-direct-slots slot-name class-or-class-name)))

(defmacro def-slot-option-changed-p (name find-slot-f spec)
  `(defun ,name (class slot-name option-name option-accessor
                 &key (eq-comp #'equal))
     ,(concat "Find the first class in the `rest' of `class-precedence-list'
where SLOT-NAME exists, and then, if option OPTION-NAME exists, compare it with
the option OPTION-NAME of SLOT-NAME of CLASS using equality predicate EQ-COMP
(if both are unbound, nil is returned). If option doesn't not exist, or there
are no SLOT-NAME slots, return t. In this version of the function, the " spec)
     (let* ((current (,find-slot-f slot-name class)))
       (unless (and current (slot-exists-p current option-name))
         (error "Slot ~a must exist and option ~a must exist in the slot."
                slot-name option-name))
       (itr (with boundp = (slot-boundp current option-name))
            (with value = (if boundp (funcall option-accessor current)))
            (for previous-class in (rest (class-precedence-list class)))
            (when-let ((s (,find-slot-f slot-name previous-class)))
              (leave (if (slot-exists-p s option-name)
                         (if (slot-boundp s option-name)
                             (not (funcall eq-comp value
                                           (funcall option-accessor s)))
                             boundp) ; option is now bound (maybe)
                         (finish)))) ; option now exists
            (finally (return t)))))) ; option now exists

(def-slot-option-changed-p slot-option-effective-changed-p find-slot
  "effective slots are examined. Please, ensure that all classes are finalized
before calling!")

(def-slot-option-changed-p slot-option-direct-changed-p find-dslot
  "direct slots are examined.")

;; *** Finalization

(defun ensure-finalized-precedence (class)
  "Finalize the class and all classes in its precedence list."
  (c2mop:ensure-finalized class) ; compute class-precedence-list
  (mapc #'c2mop:ensure-finalized (class-precedence-list class)))
