;;;; packages.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage :restas
  (:use :cl :iter :split-sequence)
  (:export #:*request-pool*
           #:*bindings*
           #:define-route
           #:define-module
           #:plugin-update

           #:make-context
           #:context-add-variable
           #:context-remove-variable
           #:context-symbol-value
           #:with-context

           #:define-initialization
           #:define-finalization
           
           #:*route*

           #:submodule
           #:connect-submodule
           #:with-submodule-context

           #:parse-host
           #:start
           
           #:reconnect-all-plugins
           #:expand-text
           #:expand-file

           #:genurl
           #:genurl-with-host
           #:apply-format-aux
           #:redirect

           #:restas-request-bindings
           #:process-route

           #:defhost
           #:define-submodule
           #:*default-host-redirect*

           #:site-url))


(in-package :restas)

(defvar *request-pool*)
(defvar *bindings*)