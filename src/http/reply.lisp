;;;; reply.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

(defvar *reply*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adopt from hunchentoot/reply.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric (setf content-type) (new-value reply)
  (:documentation "Set 'Content-Type' http header."))

(defgeneric (setf content-length) (new-value reply)
  (:documentation "Set 'Content-Length'
http header which defaults NIL.  If this is NIL, RESTAS will
compute the content length."))

(defgeneric (setf header-out) (new-value name reply)
    (:documentation "Changes the current value of the outgoing http
header named NAME \(a keyword or a string).  If a header with this
name doesn't exist, it is created."))

;;(defgeneric (setf reply-external-format) (new-value reply)
;;  (:documentation

   

;; (defun (setf cookies-out*) (new-value &optional (reply *reply*))
;;   "Sets the alist of the outgoing cookies associated with the REPLY
;; object REPLY."
;;   (setf (cookies-out reply) new-value))

(defun (setf content-type*) (new-value &optional (reply *reply*))
  "Sets the outgoing 'Content-Type' http header of REPLY."
  (setf (header-out :content-type reply) new-value))

(defun (setf content-length*) (new-value &optional (reply *reply*))
  "Sets the outgoing 'Content-Length' http header of REPLY."
  (setf (header-out :content-length reply) new-value))

;; (defun (setf return-code*) (new-value &optional (reply *reply*))
;;   "Sets the http return code of REPLY."
;;   (setf (return-code reply) new-value))

;; (defun (setf reply-external-format*) (new-value &optional (reply *reply*))
;;   "Sets the external format of REPLY."
;;   (setf (reply-external-format reply) new-value))


;;(defgeneric set-cookie (reply name &key (value "") expires path domain secure http-only)

