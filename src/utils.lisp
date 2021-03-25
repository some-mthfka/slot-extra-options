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
    "Find effective slot named SLOT-NAME in class or class named
CLASS-OR-CLASS-NAME."
    (f #'class-slots slot-name class-or-class-name))
  (defun find-dslot (slot-name class-or-class-name)
    "Find direct slot named SLOT-NAME in class or class named
CLASS-OR-CLASS-NAME."
    (f #'class-direct-slots slot-name class-or-class-name)))

(defmacro def-option-history (name find-slot-f kind)
  `(defun ,name (class slot-name option-name)
     ,(concat "Return all previous option OPTION-NAME values according to
the " kind " slots of classes `class-precedence-list' (includes CLASS itself) of
SLOT-NAME.  Classes that don't have the SLOT-NAME or slots without OPTION-NAME
or with unbound OPTION-NAME are ommited.")
     (mapcar (let ((*package* (symbol-package option-name)))
               (symbolicate 'slot-definition- option-name))
             (remove-if-not
              (lambda (x) (and x (slot-exists-and-bound-p x option-name)))
              (mapcar (curry #',find-slot-f slot-name)
                      (class-precedence-list class))))))

(def-option-history option-history-effective find-slot "effective")
(def-option-history option-history-direct find-dslot "direct")

(defmacro def-slot-option-changed-p (name option-history-f kind)
  `(defun ,name (class slot-name option-name &key (eq-comp #'equal))
     ,(concat "See if the previous option value OPTION-NAME of SLOT-NAME has
changed in the `class-precedence-list' of CLASS using equality comparison COMP,
looking through the " kind " Depending on which values are bound, new value and
previous value may be returned in the list as the second value.  If the slot
option is bound for the first time, it's considered to be a change as well.")
     (when-let ((history (,option-history-f class slot-name option-name)))
       (values (if (null (rest history))
                   t
                   (not (funcall eq-comp (first history) (second history))))
               history))))

(def-slot-option-changed-p slot-option-effective-changed-p
    option-history-effective
    "effective. Please, ensure that all classes are finalized before calling!")

(def-slot-option-changed-p slot-option-direct-changed-p
    option-history-direct "direct slots.")

;; *** Finalization

(defun ensure-finalized-precedence (class)
  "Finalize the class and all classes in its precedence list."
  (c2mop:ensure-finalized class) ; compute class-precedence-list
  (mapc #'c2mop:ensure-finalized (class-precedence-list class)))
