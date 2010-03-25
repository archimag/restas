;;;; context.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :restas)

(defclass restas-context ()
  ((vars :initarg :vars :initform nil :accessor context-data)))


(defun context-add-variable (context symbol &optional value)
  (push (cons symbol
              (or value
                  (symbol-value symbol)))
        (context-data context)))
  ;; (setf (gethash symbol context)
  ;;       (or value
  ;;           (symbol-value symbol))))

(defun context-remove-variable (context symbol)
  (setf (context-data context)
        (remove symbol
                (context-data context)
                :key #'car)))
                
  ;;(remhash symbol context))

(defun context-symbol-value (context symbol)
  (let ((pair (assoc symbol (context-data context))))
    (if pair
        (cdr pair)
        (symbol-value symbol))))
  ;; (multiple-value-bind (value present-p) (gethash symbol context)
  ;;   (if present-p
  ;;       value
  ;;       (symbol-value symbol))))

(defun (setf context-symbol-value) (newval context symbol)
  (let ((pair (assoc symbol (context-data context))))
    (if pair
        (setf (cdr pair)
              newval)
        (context-add-variable context symbol newval))))
;;  (setf (gethash symbol context)
;;        newval))

(defmacro make-context (&body bindings)
  `(make-instance 'restas-context
                  :vars (iter (for (symbol value) in ',bindings)
                              (collect (cons symbol
                                             (eval value))))))

  ;; `(let ((context (make-hash-table)))
  ;;    (iter (for (symbol value) in ',bindings)
  ;;          (context-add-variable context symbol)
  ;;          (setf (context-symbol-value context symbol)
  ;;                (eval value)))
  ;;    context))


(defmacro with-context (context &body body)
  (let ((cntx (gensym)))
    `(let ((,cntx ,context))
       (if ,cntx
           (let ((symbols)
                 (values))
             (iter (for (s . v) in (context-data ,cntx))
                   (push s symbols)
                   (push v values))
             (progv symbols values
               (progn ,@body)))
           (progn ,@body)))))

