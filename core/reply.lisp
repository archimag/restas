;;;; reply.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reply protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric headers-out (reply)
  (:documentation "Returns an alist of the outgoing headers associated with the REPLY object reply."))

(defgeneric (setf headers-out) (newvalue reply)
  (:documentation "Sets an alist of the outgoing headers associated with the REPLY object reply."))

(defgeneric content-length (reply)
  (:documentation "The outgoing 'Content-Length' http header of reply."))

(defgeneric (setf content-length) (newvalue reply)
  (:documentation "Set the outgoing 'Content-Length' http header of reply."))

(defgeneric content-type (reply)
  (:documentation "The outgoing 'Content-Type' http header of reply."))

(defgeneric (setf content-type) (newvalue reply)
  (:documentation "Set the outgoing 'Content-Type' http header of reply."))

(defgeneric cookies-out (reply)
  (:documentation "Return an alist of the outgoing cookies associated with the REPLY object reply."))

(defgeneric (setf cookies-out) (newvalue reply)
  (:documentation "Set an alist of the outgoing cookies associated with the REPLY object reply."))

(defgeneric return-code (reply)
  (:documentation "Get the http return code of reply. The return code of each REPLY object is initially set to +HTTP-OK+."))

(defgeneric (setf return-code) (newvalue reply)
  (:documentation "Set the http return code of reply."))

(defgeneric reply-external-format (reply)
  (:documentation "Get the external format of reply which is used for character output."))

(defgeneric (setf reply-external-format) (newvalue reply)
  (:documentation "Set the external format of reply which is used for character output."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reply interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *reply*)

(defun headers-out* (&optional (reply *reply*))
  "Returns an alist of the outgoing headers associated with the
REPLY object REPLY."
  (headers-out reply))

(defun cookies-out* (&optional (reply *reply*))
  "Returns an alist of the outgoing cookies associated with the
REPLY object REPLY."
  (cookies-out reply))

(defun (setf cookies-out*) (new-value &optional (reply *reply*))
  "Sets the alist of the outgoing cookies associated with the REPLY
object REPLY."
  (setf (cookies-out reply) new-value))

(defun content-type* (&optional (reply *reply*))
  "The outgoing 'Content-Type' http header of REPLY."
  (content-type reply))

(defun (setf content-type*) (new-value &optional (reply *reply*))
  "Sets the outgoing 'Content-Type' http header of REPLY."
  (setf (header-out :content-type reply) new-value))

(defun content-length* (&optional (reply *reply*))
  "The outgoing 'Content-Length' http header of REPLY."
  (content-length reply))

(defun (setf content-length*) (new-value &optional (reply *reply*))
  "Sets the outgoing 'Content-Length' http header of REPLY."
  (setf (header-out :content-length reply) new-value))

(defun return-code* (&optional (reply *reply*))
  "The http return code of REPLY.  The return codes Hunchentoot can
handle are defined in specials.lisp."
  (return-code reply))

(defun (setf return-code*) (new-value &optional (reply *reply*))
  "Sets the http return code of REPLY."
  (setf (return-code reply) new-value))

(defun reply-external-format* (&optional (reply *reply*))
  "The external format of REPLY which is used for character output."
  (reply-external-format reply))

(defun (setf reply-external-format*) (new-value &optional (reply *reply*))
  "Sets the external format of REPLY."
  (setf (reply-external-format reply) new-value))

(defun header-out-set-p (name &optional (reply *reply*))
  "Returns a true value if the outgoing http header named NAME has
been specified already.  NAME should be a keyword or a string."
  (assoc* name (headers-out reply)))

(defun header-out (name &optional (reply *reply*))
  "Returns the current value of the outgoing http header named NAME.
NAME should be a keyword or a string."
  (cdr (assoc name (headers-out reply))))

(defun cookie-out (name &optional (reply *reply*))
  "Returns the current value of the outgoing cookie named
NAME. Search is case-sensitive."
  (cdr (assoc name (cookies-out reply) :test #'string=)))

(defgeneric (setf header-out) (new-value name &optional reply)
  (:documentation "Changes the current value of the outgoing http
header named NAME \(a keyword or a string).  If a header with this
name doesn't exist, it is created.")
  (:method (new-value (name symbol) &optional (reply *reply*))
   ;; the default method
   (let ((entry (assoc name (headers-out reply))))
     (if entry
       (setf (cdr entry) new-value)
       (setf (headers-out reply)
             (acons name new-value (headers-out reply))))
     new-value))
  (:method (new-value (name string) &optional (reply *reply*))
   "If NAME is a string, it is converted to a keyword first."
   (setf (header-out (as-keyword name :destructivep nil) reply) new-value)))

  ;; (:method :after (new-value (name (eql :content-length)) &optional (reply *reply*))
  ;;  "Special case for the `Content-Length' header."
  ;;  (check-type new-value integer)
  ;;  (setf (slot-value reply 'content-length) new-value))
  ;; (:method :after (new-value (name (eql :content-type)) &optional (reply *reply*))
  ;;  "Special case for the `Content-Type' header."
  ;;  (check-type new-value (or null string))
  ;;  (setf (slot-value reply 'content-type) new-value)))
