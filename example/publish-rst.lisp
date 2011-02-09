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
(defun directory-url (submodule-symbol)
  (restas:genurl-submodule submodule-symbol
                           'restas.directory-publisher:route
                           :path '("")))

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
          (:a :href (durl '-fine-) "reStructuredText as HTML"))))))))


;; publish raw files

(restas:mount-submodule -raw- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("raw"))
  (restas.directory-publisher:*directory* *rstdir*)
  (restas.directory-publisher:*autoindex* t))

;; publish reStructuredText files as HTML

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


;; start

(restas:start '#:restas.publish-rst :port 8080)