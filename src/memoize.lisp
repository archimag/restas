;;;; memoize.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas)

(defmacro with-memoization (&body body)
  `(let ((*memotable* (make-hash-table :test 'equal)))
     ,@body))

(defmacro define-memoized-function (name args &body body)
  (let ((key (gensym))
        (value (gensym))
        (present-p (gensym)))
    `(defun ,name (,@args)
       (if *memotable*
           (let ((,key (list ',name ,@args)))
             (multiple-value-bind (,value ,present-p) (gethash ,key *memotable*)
               (if ,present-p
                   ,value
                   (setf (gethash ,key *memotable*)
                         (progn ,@body)))))
           (progn ,@body)))))