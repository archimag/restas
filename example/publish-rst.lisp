;;;; example-1.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(ql:quickload "cl-who")

;; http://www.cliki.net/CL-DOCUTILS
(ql:quickload "docutils")

;; https://github.com/archimag/restas-directory-publisher
(ql:quickload "restas-directory-publisher")

(restas:define-module #:restas.publish-rst
  (:use #:cl))

(in-package #:restas.publish-rst)

(defparameter *rstdir*
  (merge-pathnames "../example/rst/" (asdf:component-pathname (asdf:find-system '#:restas))))

(restas:define-route entry ("")
  (who:with-html-output-to-string (out)
    (:html
     (:body
      (:ul
       (:li (:a :href (restas:genurl '-raw-.route :path "") "Raw reStructuredText"))
       (:li (:a :href (restas:genurl '-fine-.route :path "") "reStructuredText as HTML"))
       (:li (:a :href (restas:genurl '-safe-.route :path "") "reStructuredText as HTML in safe mode")))))))

;; publish raw files

(restas:mount-module -raw- (#:restas.directory-publisher)
  (:url "raw")          
  (restas.directory-publisher:*directory* *rstdir*)
  (restas.directory-publisher:*autoindex* t))

;; publish reStructuredText files as HTML

(defclass renderer () ())

(defmethod restas:render-object ((renderer renderer) (file pathname))
  (restas:render-object renderer (docutils:read-rst file)))

(defmethod restas:render-object ((renderer renderer) (rst docutils.nodes:document))
  (with-output-to-string (out)
    (docutils:write-html out rst)))

(restas:mount-module -fine- (#:restas.directory-publisher)
  (:url "fine")
  (:render-method (make-instance 'renderer)) ;; Set custom renderer!!!
  (restas.directory-publisher:*directory* *rstdir*)
  (restas.directory-publisher:*directory-index-files* '("index.txt"))
  (restas.directory-publisher:*autoindex* t))

;;; Publish reStructuredText files as HTML with HTTP authorization

(defclass http-auth-route (routes:proxy-route) () )

(defmethod routes:route-check-conditions ((route http-auth-route) bindings)
  (if (call-next-method)
      (multiple-value-bind (user password) (hunchentoot:authorization)
        (or (and (string= user "admin")
                 (string= password "default"))
            (hunchentoot:require-authorization)))))

(defun http-auth-decorator (route)
  (make-instance 'http-auth-route :target route))

(restas:mount-module -safe- (#:restas.directory-publisher)
  (:url "safe")
  (:render-method (make-instance 'renderer))
  (:decorators #'http-auth-decorator)
  (restas.directory-publisher:*directory* *rstdir*)
  (restas.directory-publisher:*directory-index-files* '("index.txt"))
  (restas.directory-publisher:*autoindex* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:start '#:restas.publish-rst :port 8080)
