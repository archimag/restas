;;;; reply.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas.hunchentoot)

(defun hunchentoot-external-format-encoding (external-format)
  (cond
    ((eql external-format hunchentoot::+utf-8+ ) :utf-8)
    ((eql external-format hunchentoot::+latin-1+ ) :latin1)
    (t external-format)))

(defmethod restas:headers-out ((reply hunchentoot:reply))
  (hunchentoot:headers-out reply))

(defmethod (setf restas:headers-out) (newvalue (reply hunchentoot:reply))
  (setf (slot-value reply 'hunchentoot:headers-out)
        newvalue))

(defmethod restas:content-length ((reply hunchentoot:reply))
  (hunchentoot:content-length reply))

(defmethod restas:content-type ((reply hunchentoot:reply))
  (hunchentoot:content-type reply))

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

(defmethod restas:reply-external-format ((reply hunchentoot:reply))
  (hunchentoot-external-format-encoding (hunchentoot:reply-external-format reply)))

(defmethod (setf restas:reply-external-format) (newvalue (reply hunchentoot:reply))
  (hunchentoot-external-format-encoding 
   (setf (hunchentoot:reply-external-format reply)
         (encoding-hunchentoot-external-format newvalue))))

;; (defmethod hunchentoot::stringify-cookie ((cookie restas:cookie))
;;   (restas:stringify-cookie cookie))
