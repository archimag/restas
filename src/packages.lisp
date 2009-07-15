;;; packages.lisp

(defpackage :restas
  (:use :cl :iter :split-sequence)
  (:export #:*request-pool*
           #:*bindings*
           #:define-filesystem-route
           #:define-fs-xsl-route
           #:define-simple-route
           #:define-plugin
           #:plugin-update
           #:start-web-server
           #:reconnect-all-plugins
           #:expand-text
           #:genurl
           ))


(defpackage :restas.plugin
  (:use :cl))

(in-package :restas)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *request-pool*))