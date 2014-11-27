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
   (return-code :initform restas:+http-ok+
                :accessor restas:return-code)
   (headers-out :initform nil
                :accessor restas:headers-out)
   (cookies-out :initform nil
                :accessor restas:cookies-out)))

(defmethod restas:header-out (name (reply reply))
  (cdr (assoc name (restas:headers-out reply))))

(defmethod (setf restas:header-out) (new-value name (reply reply))
   (let ((entry (assoc name (restas:headers-out reply))))
     (if entry
       (setf (cdr entry) new-value)
       (setf (slot-value reply 'headers-out)
             (acons name new-value (restas:headers-out reply))))
     new-value))

;; (defmethod restas:reply-external-format ((reply wookie:response))
;;   (hunchentoot-external-format-encoding (wookie:response-external-format reply)))

;; (defmethod (setf restas:reply-external-format) (newvalue (reply wookie:response))
;;   (hunchentoot-external-format-encoding 
;;    (setf (wookie:response-external-format reply)
;;          (encoding-hunchentoot-external-format newvalue))))


(defun send-reply (reply body)
  (let ((response (origin-response reply)))
    (wookie:send-response response
                          :status (restas:return-code reply)
                          :body body
                          :headers (append
                                    (alist-plist (restas:headers-out reply))
                                    (iter (for (nil . cookie) in (restas:cookies-out reply))
                                          (collect :set-cookie)
                                          (collect (restas:stringify-cookie cookie)))))))

