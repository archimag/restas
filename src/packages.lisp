;;;; packages.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage :restas
  (:use :cl :iter :split-sequence)
  (:export #:*default-host-redirect*
           #:*request-pool*
           #:*bindings*
           #:*route*
           #:*submodule*
           ;; general
           #:render-object
           #:redirect
           ;; cache
           #:with-memoization
           #:define-memoized-function
           ;; context
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
           #:submodule
           #:submodule-symbol
           #:submodule-module
           #:submodule-parent
           #:connect-submodule
           #:with-submodule-context
           ;; routes
           #:define-route
           #:route-symbol
           #:genurl
           #:genurl-submodule
           #:genurl-with-host
           ;; service
           #:start 
           ;; aux
           #:reconnect-all-routes
           #:debug-mode-on
           #:debug-mode-off
           ;; misc
           #:request-full-uri
           ;;#:process-route
           ))
