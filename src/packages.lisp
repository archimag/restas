;;;; packages.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:restas
  (:use #:cl #:iter #:split-sequence)
  (:export #:*default-host-redirect*
           #:*request-pool*
           #:*bindings*
           #:*route*
           #:*submodule*
           #:*request*
           #:*reply*

           ;; http
           #:headers-in
           #:header-out
           #:headers-out
           #:request-method
           #:request-uri
           #:server-protocol
           #:remote-address
           #:remote-port
           #:cookies-in
           #:get-parameters
           #:post-parameters
           #:script-name
           #:query-string
           #:raw-post-data
           #:header-in
           #:script-name*
           #:query-string*
           #:get-parameters*
           #:post-parameters*
           #:headers-in*
           #:cookies-in*
           #:header-in*
           #:remote-addr*
           #:remote-port*
           #:real-remote-addr
           #:host
           #:request-uri*
           #:request-method*
           #:server-protocol*
           #:user-agent
           #:cookie-in
           #:referer
           #:get-parameter
           #:post-parameter
           #:parameter
           #:handle-if-modified-since

           ;; routes
           #:define-route
           #:route-symbol
           #:genurl
           #:genurl-submodule
           #:genurl-with-host
           #:redirect
           #:parse-route-url
           #:abort-route-handler

           ;; modules
           #:define-module           
           #:define-initialization
           #:define-finalization
           #:define-default-render-method

           ;; submodules
           #:mount-submodule
           #:submodule
           #:submodule-symbol
           #:submodule-module
           #:submodule-parent
           #:connect-submodule
           #:with-submodule-context
           #:with-submodule
           #:find-submodule
           #:submodule-context
           #:find-upper-submodule

           ;; render
           #:render-object

           ;; cache
           #:define-memoized-function
           #:with-memoization

           ;; context
           #:make-context
           #:context-add-variable
           #:context-remove-variable
           #:context-symbol-value
           #:with-context
           #:copy-restas-context

           ;; service
           #:start
           #:reconnect-all-routes

           ;; debug
           #:*max-debugging-threads*
           #:debug-mode-on
           #:debug-mode-off

           ;; misc
           #:request-full-uri

           ))
