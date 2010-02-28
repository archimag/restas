;;;; preserve-context.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :restas)

(defun context-add-variable (context symbol &optional value)
  (setf (gethash symbol context)
        (or value
            (symbol-value symbol))))

(defun context-remove-variable (context symbol)
  (remhash symbol context))

(defun context-symbol-value (context symbol)
  (multiple-value-bind (value present-p) (gethash symbol context)
    (if present-p
        value
        (symbol-value symbol))))

(defun (setf context-symbol-value) (newval context symbol)
  (setf (gethash symbol context)
        newval))

(defmacro make-preserve-context (&body bindings)
  `(let ((context (make-hash-table)))
     (iter (for (symbol value) in ',bindings)
           (context-add-variable context symbol)
           (setf (context-symbol-value context symbol)
                 (eval value)))
     context))


(defmacro with-context (context &body body)
  `(let ((cntx ,context))
     (if cntx
         (let ((symbols)
               (values))
           (iter (for (s v) in-hashtable cntx)
                 (push s symbols)
                 (push v values))
           (progv symbols values
             (unwind-protect
                  (progn ,@body)
               (iter (for s in symbols)
                     (setf (gethash s cntx)
                           (symbol-value s))))))
         (progn ,@body))))


