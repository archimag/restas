;;;; request.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.hunchentoot)

(defmethod restas:get-parameters ((request hunchentoot:request))
  (hunchentoot:get-parameters request))

(defmethod restas:post-parameters ((request hunchentoot:request))
  (hunchentoot:post-parameters request))

(defmethod restas:cookies-in ((request hunchentoot:request))
  (hunchentoot:cookies-in request))

(defmethod restas:query-string ((request hunchentoot:request))
  (hunchentoot:query-string request))

(defmethod restas:request-method ((request hunchentoot:request))
  (hunchentoot:request-method request))

(defmethod restas:request-uri ((request hunchentoot:request))
  (hunchentoot:request-uri request))

(defmethod restas:server-protocol ((request hunchentoot:request))
  (hunchentoot:server-protocol request))

(defmethod restas:headers-in ((request hunchentoot:request))
  (hunchentoot:headers-in request))

(defmethod restas:remote-address ((request hunchentoot:request))
  (hunchentoot:remote-addr request))

(defmethod restas:remote-port ((request hunchentoot:request))
  (hunchentoot:remote-port request))

(defmethod restas:script-name ((request hunchentoot:request))
  (hunchentoot:script-name request))

(defmethod restas:raw-post-data ((request hunchentoot:request))
  (or (hunchentoot:raw-post-data :request request :force-binary t)
      (make-array 0 :element-type '(unsigned-byte 8))))

(defmethod restas:request-listener ((request hunchentoot:request))
  (hunchentoot:request-acceptor request))
