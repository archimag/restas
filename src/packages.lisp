;;;; packages.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage :restas
  (:use :cl :iter :split-sequence)
  (:export #:*default-host-redirect*
           #:*route*
           #:*module*

           ;; acceptors
           #:restas-acceptor
           #:restas-ssl-acceptor
           
           ;; routes
           #:route
           #:define-route
           #:route-symbol
           #:make-route-url
           #:genurl
           #:genurl*
           #:redirect
           #:parse-route-url
           #:abort-route-handler

           #:process-route
           
           ;; modules
           #:define-module
           #:initialize-module-instance
           #:finalize-module-instance
           #:mount-module
           #:find-mounted-module
           #:with-module
           #:module-context
           
           ;; render
           #:render-object

           ;; policy
           #:define-policy
           
           ;; context
           #:make-context
           #:context-add-variable
           #:context-remove-variable
           #:context-symbol-value
           #:with-context
           #:copy-restas-context
           
           ;; service
           #:start
           #:stop-all
           #:reconnect-all-routes
           
           ;; debug
           #:*max-debugging-threads*
           #:debug-mode-on
           #:debug-mode-off
           
           ;; misc
           #:request-full-uri

           ;; decorators
           #:@no-cache

           #:@nginx-accel-redirect
           #:*nginx-internal-location*
           #:*nginx-internal-alias*
           #:*nginx-internal-root*
           
           #:@apache-xsendfile

           #:delete-route))
