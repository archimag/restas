;;;; example-1.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(asdf:operate 'asdf:load-op '#:cl-who)

;; http://www.cliki.net/CL-DOCUTILS
(asdf:operate 'asdf:load-op '#:docutils)

;;https://github.com/archimag/restas-directory-publisher
(asdf:operate 'asdf:load-op '#:restas-directory-publisher)

(restas:define-module #:restas.publish-rst
  (:use #:cl))

(in-package #:restas.publish-rst)

(defparameter *rstdir*
  (merge-pathnames "example/rst/"
                   (make-pathname :directory (pathname-directory
                                              (asdf:component-pathname (asdf:find-system '#:restas))))))
(restas:define-route entry ("")
  (flet ((durl (submodule-symbol)
           (restas:genurl-submodule submodule-symbol
                                    'restas.directory-publisher:route
                                    :path '(""))))
    (who:with-html-output-to-string (out)
      (:html
       (:body
        (:ul
         (:li
          (:a :href (durl '-raw-) "Raw reStructuredText"))
         (:li
          (:a :href (durl '-fine-) "reStructuredText as HTML"))
         (:li
          (:a :href (durl '-safe-) "reStructuredText as HTML in safe mode"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; publish raw files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:mount-submodule -raw- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("raw"))
  (restas.directory-publisher:*directory* *rstdir*)
  (restas.directory-publisher:*autoindex* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; publish reStructuredText files as HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drawer () ())

(defmethod restas:render-object ((drawer drawer) (file pathname))
  (restas:render-object drawer (docutils:read-rst file)))

(defmethod restas:render-object ((drawer drawer) (rst docutils.nodes:document))
  (with-output-to-string (out)
    (docutils:write-html out rst)))

(restas:mount-submodule -fine- (#:restas.directory-publisher)
  (restas.directory-publisher:*default-render-method* (make-instance 'drawer)) ;;; Set custom drawer!!!
  (restas.directory-publisher:*baseurl* '("fine"))
  (restas.directory-publisher:*directory* *rstdir*)
  (restas.directory-publisher:*directory-index-files* '("index.txt"))
  (restas.directory-publisher:*autoindex* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Publish reStructuredText files as HTML with HTTP authorization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; http auth decorator

(defclass http-auth-route (routes:proxy-route) () )

(defmethod routes:route-check-conditions ((route http-auth-route) bindings)
  (if (call-next-method)
      (multiple-value-bind (user password) (hunchentoot:authorization)
        (or (and (string= user "hello")
                 (string= password "world"))
            (hunchentoot:require-authorization)))))

(defun http-auth-decorator (route)
  (make-instance 'http-auth-route :target route))

;;; render reStructuredText decorator

(defclass render-rst-route (routes:proxy-route) ())

(defmethod restas:process-route :around ((route render-rst-route) bindings)
  (let ((restas.directory-publisher:*default-render-method* (make-instance 'drawer)))
    (call-next-method)))

(defun render-rst-decorator (route)
  (make-instance 'render-rst-route :target route))

;;; mount module

(restas:mount-submodule -safe- (#:restas.directory-publisher http-auth-decorator render-rst-decorator)
  (restas.directory-publisher:*baseurl* '("safe"))
  (restas.directory-publisher:*directory* *rstdir*)
  (restas.directory-publisher:*directory-index-files* '("index.txt"))
  (restas.directory-publisher:*autoindex* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:start '#:restas.publish-rst :port 8080)