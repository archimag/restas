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

           #:*chrome-mapper*
           #:restas-request-bindings
           #:process-route

           #:*sites*
           #:site
           #:site-add-plugin
           #:plugin-instance
           #:adopt-route-result
           #:calculate-user-login
           ))


(defpackage :restas.plugin
  (:use :cl))

(in-package :restas)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *request-pool*))