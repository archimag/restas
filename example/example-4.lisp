;;;; example-4.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(asdf:operate 'asdf:load-op '#:closure-template)
(asdf:operate 'asdf:load-op '#:restas)

(restas:define-module #:restas.example-4
  (:use #:cl))

(in-package #:restas.example-4)

;;;; view with templates

(closure-template:compile-cl-templates
 (asdf:system-relative-pathname '#:restas "example/example-4.tmpl"))

(defclass template-drawer ()
  ((template-package :initarg :template-package)))

(defmethod restas:render-object ((drawer template-drawer) obj)
  (restas:render-object (symbol-function (find-symbol (symbol-name (restas:route-symbol restas:*route*))
                                                      (slot-value drawer 'template-package)))
                        obj))

;;;; view with context processor

(defclass context-template-drawer (template-drawer) ())

(defmethod restas:render-object ((drawer context-template-drawer) obj)
  (call-next-method drawer
                    (list* :info (list :foo (list :href (restas:genurl 'foo))
                                       :bar (list :href (restas:genurl 'bar)))
                           obj)))


;;;; routes

(restas:define-route foo ("foo")
  (list :foo "Foo"))

(restas:define-route bar ("bar")
  (list :bar "Bar"))

;;;; init

(defmethod restas:initialize-module-instance ((module (eql #.*package*)) context)
  (unless (restas:context-symbol-value context '*default-render-method*)
    (restas:context-add-variable context
                                 '*default-render-method*
                                 (make-instance 'context-template-drawer :template-package '#:restas.example-4.view))))
