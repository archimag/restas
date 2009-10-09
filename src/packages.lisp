;;; packages.lisp

(defpackage :restas
  (:use :cl :iter :split-sequence)
  (:export #:*request-pool*
           #:*bindings*
           #:define-filesystem-route
           #:define-fs-xsl-route
           #:define-route
           #:define-plugin
           #:plugin-update

           #:parse-host

           #:start-site
           
           #:reconnect-all-plugins
           #:expand-text
           #:expand-file

           #:genurl
           #:genurl-with-host
           #:apply-format-aux
           #:redirect

           #:*chrome-mapper*
           #:restas-request-bindings
           #:process-route

           #:*sites*
           #:site
           #:defsite
           #:site-add-plugin
           #:plugin-instance
           #:adopt-route-result
           #:calculate-user-login
           #:site-url
           ))


(defpackage :restas.plugin
  (:use :cl))

(in-package :restas)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *request-pool*))