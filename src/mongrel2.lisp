;;;; mongre2.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:restas.mongrel2
  (:use #:cl #:iter)
  (:export #:start))

(in-package #:restas.mongrel2)

(defmethod restas:headers-in ((request mongrel2:request))
  (mongrel2:headers-in request))

;; (defgeneric headers-in (request)
;;   (:documentation "An alist of the incoming headers."))

(defmethod restas:request-method ((request mongrel2:request))
  (intern (restas:header-in :method request) :keyword))

;; (defgeneric request-method (request)
;;   (:documentation "The request method as a keyword."))

(defmethod restas:request-uri ((request mongrel2:request))
  (restas:header-in :uri request))

;; (defgeneric request-uri (request)
;;   (:documentation "The request URI as a string."))

(defmethod restas:server-protocol ((request mongrel2:request))
  :http)

;; (defgeneric server-protocol (request)
;;   (:documentation "The HTTP protocol as a keyword."))

;; (defgeneric remote-address (request)
;;   (:documentation "The IP address of the client that initiated this request."))

(defmethod restas:remote-port ((request mongrel2:request))
  6767)

;; (defgeneric remote-port (request)
;;   (:documentation "The TCP port number of the client socket from which this request originated."))


;; (defgeneric cookies-in (request)
;;   (:documentation "An alist of the cookies sent by the client."))

;; (defgeneric get-parameters (request)
;;   (:documentation "An alist of the GET parameters sent by the client."))

(defmethod restas:post-parameters ((request mongrel2:request))
  (hunchentoot::form-url-encoded-list-to-alist (ppcre:split "&" (restas:raw-post-data request))))

;; (defgeneric post-parameters (request)
;;   (:documentation "An alist of the POST parameters sent by the client."))

(defmethod restas:script-name ((request mongrel2:request))
  (restas:header-in :path request))

;; (defgeneric script-name (request)
;;   (:documentation "The URI requested by the client without the query string."))

(defmethod restas:query-string ((request mongrel2:request))
  (restas:header-in :query request))

;; (defgeneric query-string (request)
;;   (:documentation "The query string of this request."))

(defmethod restas:raw-post-data ((request mongrel2:request) &key (encoding :utf-8))
  (mongrel2:raw-post-data request :encoding encoding))

(defmethod restas:headers-out ((reply mongrel2:reply))
  (mongrel2:headers-out reply))

(defmethod (setf restas:headers-out) (new-value (reply mongrel2:reply))
  nil)
  ;;(setf (mongrel2:headers-out reply) new-value))

(defmethod (setf restas:header-out) (new-value name (reply mongrel2:reply))
   (let ((entry (assoc name (restas:headers-out reply))))
     (if entry
       (setf (cdr entry) new-value)
       (setf (restas:headers-out reply)
             (acons name new-value (restas:headers-out reply))))
     new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *connection* nil)

(defun request-hostname-port (request &aux (host (restas:header-in :host request)))
  (let* ((tmp (ppcre:split ":" host))
         (port (second tmp)))
    (cons (first tmp)
          (if port
              (parse-integer port)
              80))))

(defun restas-dispatcher (req)
  (let ((vhost (restas::find-vhost (request-hostname-port req)))
        (restas:*request* req)
        (restas:*reply* (make-instance 'mongrel2:reply)))
    (if vhost
        (multiple-value-bind (route bindings) (routes:match (slot-value vhost 'restas::mapper)
                                                (restas:request-uri req))
          (cond
            (route
             (mongrel2:reply *connection*
                             req
                             restas:*reply*
                             (handler-bind ((error #'restas::maybe-invoke-debugger))
                               (restas::process-route route bindings))))
            (t (mongrel2:reply *connection*
                               req
                               restas:*reply*
                               "Not Found")))))))



(defun start (module &key
              hostname
              (port 8080)
              (sub-addr "tcp://127.0.0.1:9997")
              (pub-addr "tcp://127.0.0.1:9996")
              (context (restas:make-context)))
  (restas::add-toplevel-submodule (restas::make-submodule module :context context)
                          hostname
                          port)
  (restas:reconnect-all-routes :reinitialize nil)
  (let* ((sender-uuid (write-to-string (uuid:make-v4-uuid))))
    (mongrel2::with-trivial-server (sender-uuid sub-addr pub-addr :port port)
      (loop
         (mongrel2:with-connection (*connection* :sender-uuid sender-uuid
                                                 :sub-addr sub-addr
                                                 :pub-addr pub-addr)
           (restas-dispatcher (mongrel2:recv *connection*)))))))
