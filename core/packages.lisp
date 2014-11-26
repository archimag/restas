;;;; packages.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:restas
  (:use #:cl #:iter #:split-sequence)
  (:shadow #:defconstant)
  (:export #:*route*
           #:*module*

           #:*tmp-directory*
           #:*tmp-files*

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
           #:delete-route

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
           #:restas-status-message

           ;; policy
           #:define-policy
           
           ;; context
           #:make-context
           #:context-add-variable
           #:context-remove-variable
           #:context-symbol-value
           #:with-context
           #:copy-restas-context
           
           ;; decorators
           #:@no-cache

           #:@nginx-accel-redirect
           #:*nginx-internal-location*
           #:*nginx-internal-alias*
           #:*nginx-internal-root*
           
           #:@apache-xsendfile

           ;; listener
           #:listener-port
           #:listener-address

           ;; request
           #:get-parameters
           #:post-parameters
           #:cookies-in
           #:query-string
           #:request-method
           #:request-uri
           #:server-protocol
           #:headers-in
           #:remote-address
           #:remote-port
           #:script-name
           #:raw-post-data
           #:request-listener

           #:*request*

           #:script-name*
           #:query-string*
           #:get-parameters*
           #:post-parameters*
           #:headers-in*
           #:cookies-in*
           #:header-in
           #:header-in*
           #:authorization
           #:remote-address*
           #:remote-port*
           #:real-remote-address
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

           ;; reply
           #:headers-out
           #:cookies-out
           #:return-code
           #:reply-external-format

           #:*reply*
           
           #:headers-out*
           #:cookies-out*
           #:content-type*
           #:content-length*
           #:return-code*
           #:reply-external-format*
           #:header-out-set-p
           #:header-out
           #:cookie-out

           ;; http constant
           #:+http-continue+
           #:+http-switching-protocols+
           #:+http-ok+
           #:+http-created+
           #:+http-accepted+
           #:+http-non-authoritative-information+
           #:+http-no-content+
           #:+http-reset-content+
           #:+http-partial-content+
           #:+http-multi-status+
           #:+http-multiple-choices+
           #:+http-moved-permanently+
           #:+http-moved-temporarily+
           #:+http-see-other+
           #:+http-not-modified+
           #:+http-use-proxy+
           #:+http-temporary-redirect+
           #:+http-bad-request+
           #:+http-authorization-required+
           #:+http-payment-required+
           #:+http-forbidden+
           #:+http-not-found+
           #:+http-method-not-allowed+
           #:+http-not-acceptable+
           #:+http-proxy-authentication-required+
           #:+http-request-time-out+
           #:+http-conflict+
           #:+http-gone+
           #:+http-length-required+
           #:+http-precondition-failed+
           #:+http-request-entity-too-large+
           #:+http-request-uri-too-large+
           #:+http-unsupported-media-type+
           #:+http-requested-range-not-satisfiable+
           #:+http-expectation-failed+
           #:+http-failed-dependency+
           #:+http-internal-server-error+
           #:+http-not-implemented+
           #:+http-bad-gateway+
           #:+http-service-unavailable+
           #:+http-gateway-time-out+
           #:+http-version-not-supported+

           ;; misc
           #:mime-type
           ))

;; (defpackage #:restas.rfc2388
;;   (:use :common-lisp)
;;   (:export
;;    #:parse-header
;;    #:header
;;    #:header-name
;;    #:header-value
;;    #:header-parameters

;;    #:content-type
;;    #:find-header
;;    #:find-parameter
;;    #:find-content-disposition-header
;;    #:get-file-name

;;    #:parse-mime
;;    #:mime-part
;;    #:mime-part-contents
;;    #:mime-part-headers
;;    #:make-mime-part))
