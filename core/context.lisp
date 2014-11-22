;;;; context.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :restas)

(defclass restas-context ()
  ((vars :initarg :vars :initform nil :accessor context-vars)
   (values :initarg :values :initform nil :accessor context-values)
   (prototype :initarg :prototype :initform nil :accessor context-prototype)))

(defun copy-restas-context (rc)
  (make-instance 'restas-context
                 :vars (copy-list (context-vars rc))
                 :values (copy-list (context-values rc))
                 :prototype (context-prototype rc)))

(defun context-add-variable (context symbol &optional value)
    (push symbol
          (context-vars context))
    (push (or value
              (and (boundp symbol)
                   (symbol-value symbol)))
          (context-values context)))

(defun context-remove-variable (context symbol)
  (let ((pos (position symbol (context-vars context))))
    (when pos
      (flet ((remove-n (list)
               (concatenate 'list
                            (subseq list 0 pos)
                            (subseq list (1+ pos)))))
        (setf (context-vars context)
              (remove-n (context-vars context)))
        (setf (context-values context)
              (remove-n (context-values context)))))))

(defun context-symbol-value (context symbol)
  (let ((pos (position symbol (context-vars context))))
    (if pos
        (values (nth pos (context-values context)) t)
        (let ((prototype (context-prototype context)))
          (if prototype
              (context-symbol-value prototype symbol)
              (values (symbol-value symbol) nil))))))

(defun (setf context-symbol-value) (newval context symbol)
  (let ((pos (position symbol (context-vars context))))
    (cond
      (pos
       (setf (nth pos (context-values context))
             newval))
      (t
       (context-add-variable context symbol newval)))))

(defun make-context (&optional bindings)
  (iter (for (var . value) in bindings)
        (collect var into vars)
        (collect value into values)
        (finally
         (return (make-instance 'restas-context
                                :vars vars
                                :values values)))))

(defun context-all-vars (context)
  (let ((prototype (context-prototype context)))
    (if prototype
        (append (context-vars prototype)
                (context-vars context))
        (context-vars context))))

(defun context-all-values (context)
  (let ((prototype (context-prototype context)))
    (if prototype
        (append (context-values prototype)
                (context-values context))
        (context-values context))))

(defmacro with-context (context &body body)
  (let ((cntx (gensym)))
    `(let ((,cntx ,context))
       (progv (context-all-vars ,cntx) (context-all-values ,cntx)
         ,@body))))
           

