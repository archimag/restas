;;;; reply.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas.wookie)

(defmethod restas:headers-out ((reply wookie:response))
  (alexandria:plist-alist (wookie:response-headers reply)))

(defmethod (setf restas:headers-out) (newvalue (reply wookie:response))
  ;; (setf (slot-value reply 'hunchentoot:headers-out)
  ;;       newvalue)
  nil
  )

(defmethod restas:content-length ((reply wookie:response))
  ;;(hunchentoot:content-length reply)
  nil
  )

(defmethod restas:content-type ((reply wookie:response))
  nil
  ;;(hunchentoot:content-type reply)
  )

(defmethod restas:cookies-out ((reply wookie:response))
  ;;(hunchentoot:cookies-out reply)
  nil
  )

;; (defmethod (setf restas:cookies-out) (newvalue (reply wookie:response))
;;   (setf (hunchentoot:cookies-out reply)
;;         newvalue))

(defmethod restas:return-code ((reply wookie:response))
  ;;(hunchentoot:return-code reply)
  restas:+http-ok+
  )

(defmethod (setf restas:return-code) (newvalue (reply wookie:response))
  ;; (setf (hunchentoot:return-code reply)
  ;;       newvalue)
  newvalue
  )

;; (defmethod restas:reply-external-format ((reply wookie:response))
;;   (hunchentoot-external-format-encoding (wookie:response-external-format reply)))

;; (defmethod (setf restas:reply-external-format) (newvalue (reply wookie:response))
;;   (hunchentoot-external-format-encoding 
;;    (setf (wookie:response-external-format reply)
;;          (encoding-hunchentoot-external-format newvalue))))
