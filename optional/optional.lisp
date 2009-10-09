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


;;;; string<->octets

(defun/export string-to-octets (string &key (external-format :utf-8) (start 0) end)
  #+sbcl(sb-ext:string-to-octets string
                                 :external-format external-format
                                 :start start
                                 :end end)
  #-sbcl(babel:string-to-octets string
                                :encoding external-format
                                :start start
                                :end end))

(defun/export octets-to-string (vector &key (external-format :utf-8) (start 0) end)
  #+sbcl(sb-ext:octets-to-string vector
                                 :external-format external-format
                                 :start start
                                 :end end)
  #-sbcl(babel:octets-to-string vector
                                :encoding external-format
                                :start start
                                :end end))
