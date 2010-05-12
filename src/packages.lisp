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
           
           #:render-object

           #:with-memoization
           #:define-memoized-function

           #:make-context
           #:context-add-variable
           #:context-remove-variable
           #:context-symbol-value
           #:with-context
           ;; modules
           #:define-module           
           #:define-initialization
           #:define-finalization
           #:define-default-render-method
           ;; submodules
           #:define-submodule
           #:*submodule*
           #:submodule
           #:submodule-symbol
           #:submodule-module
           #:submodule-parent
           #:connect-submodule
           #:with-submodule-context
           ;; routes
           #:define-route
           #:*route*
           #:genurl
           #:genurl-submodule
           #:genurl-with-host
           
           #:redirect


           #:request-full-uri
           #:start           
           #:reconnect-all-routes

           #:process-route

           #:*default-host-redirect*))
