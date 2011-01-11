;;;; mongre2.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:restas.mongrel2
  (:use #:cl #:iter))

(in-package #:restas.mongrel2)

(defmethod restas:headers-in ((request mongrel2::request))
  (mongrel2::request-headers request))

;; (defgeneric headers-in (request)
;;   (:documentation "An alist of the incoming headers."))

(defmethod restas:request-method ((request mongrel2::request))
  (intern (restas:header-in :method request) :keyword))

;; (defgeneric request-method (request)
;;   (:documentation "The request method as a keyword."))

(defmethod restas:request-uri ((request mongrel2::request))
  (restas:header-in :uri request))

;; (defgeneric request-uri (request)
;;   (:documentation "The request URI as a string."))

(defmethod restas:server-protocol ((request mongrel2::request))
  :http)

;; (defgeneric server-protocol (request)
;;   (:documentation "The HTTP protocol as a keyword."))

;; (defgeneric remote-address (request)
;;   (:documentation "The IP address of the client that initiated this request."))

(defmethod restas:remote-port ((request mongrel2::request))
  6767)

;; (defgeneric remote-port (request)
;;   (:documentation "The TCP port number of the client socket from which this request originated."))


;; (defgeneric cookies-in (request)
;;   (:documentation "An alist of the cookies sent by the client."))

;; (defgeneric get-parameters (request)
;;   (:documentation "An alist of the GET parameters sent by the client."))

;; (defgeneric post-parameters (request)
;;   (:documentation "An alist of the POST parameters sent by the client."))

(defmethod restas:script-name ((request mongrel2::request))
  (restas:header-in :path request))

;; (defgeneric script-name (request)
;;   (:documentation "The URI requested by the client without the query string."))

(defmethod restas:query-string ((request mongrel2::request))
  (restas:header-in :query request))

;; (defgeneric query-string (request)
;;   (:documentation "The query string of this request."))

(defmethod restas:raw-post-data ((request mongrel2::request))
  (mongrel2::request-data request))

;; (defgeneric raw-post-data (request)
;;   (:documentation "The raw string sent as the body of a
;; POST request, populated only if not a multipart/form-data request."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass vhost ()
  ((host :initarg :host :reader vhost-host)
   (mapper :initform (make-instance 'routes:mapper))
   (modules :initarg :modules :initform nil)))

(defparameter *vhosts* nil)

(defvar *connection* nil)

(defun restas-dispatcher (req &aux (host (restas:header-in :host req)))
  (let ((vhost (or (find (if (find #\: host)
                             host
                             (format nil "~A:~A"
                                     host 
                                     (restas:remote-port req)))
                         *vhosts*
                         :key #'vhost-host
                         :test #'string=)
                   (find-if #'null
                            *vhosts*
                            :key #'vhost-host)))
        (restas:*request* req))
    ;;(break "~A" req)
    (when vhost
        (multiple-value-bind (route bindings) (routes:match (slot-value vhost 'mapper)
                                                (restas:request-uri req))
          (if route
              (mongrel2::reply *connection*
                               req
                               (mongrel2::format-http-response 
                                (handler-bind ((error #'restas::maybe-invoke-debugger))
                                  (restas::process-route route bindings))
                                200
                                "Ok"
                                (list :content-type "text/html")))
              (mongrel2::reply *connection*
                               req
                               (mongrel2::format-http-response "Not Found" 404 "Not Found" nil)))))))



(defun start (sender-uuid sub-addr pub-addr module &key (context (restas:make-context)) )
  (let* ((package (or (find-package module)
                      (error "Package ~A not found" module)))
         (vhost (make-instance 'vhost
                               :host nil
                               :modules (list (make-instance 'restas:submodule
                                                             :module package
                                                             :context context))))
         (mapper (slot-value vhost 'mapper))
         (*vhosts* (list vhost)))
    (routes:reset-mapper mapper)
    (iter (for module in (slot-value vhost 'modules))
          (restas:connect-submodule module mapper))
    
    (loop
         (mongrel2::with-connection (*connection* :sender-uuid sender-uuid
                                                  :sub-addr sub-addr
                                                  :pub-addr pub-addr)
           (restas-dispatcher (mongrel2::recv *connection*))))))
    