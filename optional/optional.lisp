;;;; optional.lisp

(defpackage #:restas.optional
  (:use :cl :iter)
  (:import-from "RESTAS" "APPLY-FORMAT-AUX"))

(in-package #:restas.optional)

(defmacro defun/export (name args &body body)
  `(export (defun ,name ,args ,@body)))

(defun/export username ()
  "Return name of the user if he loggen on"
  (cdr (assoc :user-login-name restas:*bindings*)))


(defun/export in-pool (obj)
  (gp:object-register obj restas:*request-pool*))
