;;;; reply.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas.wookie)

(defclass reply ()
  ((origin :initarg :origin
           :reader origin-response)
   (external-format :initform :utf-8
                    :accessor restas:reply-external-format)   
   (return-code :initform restas:+http-ok+
                :accessor restas:reply-return-code)
   (headers-out :initform nil
                :accessor restas:reply-headers-out)
   (cookies-out :initform nil
                :accessor restas:reply-cookies-out)
   (alredy-send-p :initform nil
                  :accessor alredy-send-p)))

(defmethod restas:reply-header-out (name (reply reply))
  (cdr (assoc name (restas:headers-out reply))))

(defmethod (setf restas:reply-header-out) (new-value name (reply reply))
   (let ((entry (assoc name (restas:headers-out reply))))
     (if entry
       (setf (cdr entry) new-value)
       (setf (slot-value reply 'headers-out)
             (acons name new-value (restas:headers-out reply))))
     new-value))

(defmethod restas:abort-request-handler ((reply reply) result)
  (send-reply reply result)
  nil)

(defun send-reply (reply body)
  (unless (alredy-send-p reply)
    #|------------------------------------------------------------------------|#
    (setf (alredy-send-p reply) t)
    #|------------------------------------------------------------------------|#
    (let ((response (origin-response reply))
          (code (restas:return-code reply))
          (content body))
      (when (and (emptyp body) (not (eql code restas:+http-ok+)))
        (setf content 
              (restas:restas-status-message (restas:return-code reply)))
        (setf (restas:content-type reply)
              "text/html"))
      (when (stringp content)
        (setf content
              (babel:string-to-octets content
                                      :encoding (restas:reply-external-format reply))))
      (wookie:send-response response
                            :status code
                            :body content
                            :headers (append
                                      (alist-plist (restas:headers-out reply))
                                      (iter (for (nil . cookie) in (restas:cookies-out reply))
                                            (collect :set-cookie)
                                            (collect (restas:stringify-cookie cookie))))))))

