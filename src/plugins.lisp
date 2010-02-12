;;;; plugins.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :restas)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *plugins* (make-hash-table :test 'equal)))

;;; define-plugin

(defparameter *route-macros* '(define-route))

(defmacro define-plugin (name &rest options)
  (let ((use (cdr (assoc :use options)))
        (export (cdr (assoc :export options)))
        (impl-package-name (format nil "~:@(~A.IMPL.ROUTES~)" name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((package (defpackage ,name (:use ,@use) (:export #:*baseurl* ,@export))))
         (flet ((set-package-var (vname &optional value)
                  (eval `(defparameter ,(intern vname package) ,value))))
           (iter (for s in (list '*request-pool* '*bindings* 'genurl 'define-route))
                 (import s package))
           (set-package-var "*ROUTES*" (defpackage ,impl-package-name (:use)))
           (set-package-var "*BASEURL*")
           (set-package-var "*DEFAULT-CONTENT-TYPE*" ,(cadr (assoc :default-content-type options)))
           (setf (gethash ',name *plugins*)
                 (find-package package))
           package)))))


;;;; define-initialization

(defmacro define-initialization ((context) &body body)
  (let ((init-func-name (intern "%PLUGIN-INITIALIZE-FUNCTION%")))
    `(defun ,init-func-name (,context) ,@body)))

(defmacro define-finalization ((context) &body body)
  (let ((finish-func-name (intern "%PLUGIN-FINALIZE-FUNCTION%")))
    `(defun ,finish-func-name (,context) ,@body)))