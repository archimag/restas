;;;; reply.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas.hunchentoot)

(defmethod restas:headers-out ((reply hunchentoot:reply))
  (hunchentoot:headers-out reply))

(defmethod restas:header-out (name (reply hunchentoot:reply))
  (hunchentoot:header-out name reply))

(defmethod (setf header-out) (new-value name (reply hunchentoot:reply))
  (setf (hunchentoot:header-out name reply)
        new-value))

(defmethod restas:cookies-out ((reply hunchentoot:reply))
  (hunchentoot:cookies-out reply))

(defmethod (setf restas:cookies-out) (newvalue (reply hunchentoot:reply))
  (setf (hunchentoot:cookies-out reply)
        newvalue))

(defmethod restas:return-code ((reply hunchentoot:reply))
  (hunchentoot:return-code reply))

(defmethod (setf restas:return-code) (newvalue (reply hunchentoot:reply))
  (setf (hunchentoot:return-code reply)
        newvalue))

(defmethod restas:abort-request-handler ((reply hunchentoot:reply) result)
  (throw 'restas::route-done result))

(defmethod restas:reply-external-format ((reply hunchentoot:reply))
  (hunchentoot:reply-external-format reply))

(defmethod (setf restas:reply-external-format) (newvalue (reply hunchentoot:reply))
  (setf (hunchentoot:reply-external-format reply)
        newvalue))

(defmethod hunchentoot::stringify-cookie ((cookie restas:cookie))
  (restas::stringify-cookie cookie))

