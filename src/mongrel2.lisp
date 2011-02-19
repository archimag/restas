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

(defvar *connection* nil)

(defun request-hostname-port (request &aux (host (wsal:header-in :host request)))
  (let* ((tmp (ppcre:split ":" host))
         (port (second tmp)))
    (cons (first tmp)
          (if port
              (parse-integer port)
              80))))

(defun restas-dispatcher (req)
  (let ((vhost (restas::find-vhost (request-hostname-port req)))
          (wsal:*request* req)
          (wsal:*reply* (make-instance 'mongrel2:reply)))
      (if vhost
          (multiple-value-bind (route bindings) (routes:match (slot-value vhost 'restas::mapper)
                                                  (wsal:request-uri req))
            (cond
              (route
               (mongrel2:reply *connection*
                               req
                               wsal:*reply*
                               (handler-bind ((error #'restas::maybe-invoke-debugger))
                                 (restas::process-route route bindings))))
              (t (mongrel2:reply *connection*
                                 req
                                 wsal::*reply*
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
  (labels ((impl ()
             (let* ((sender-uuid (write-to-string (uuid:make-v4-uuid))))
               (mongrel2::with-trivial-server (sender-uuid sub-addr pub-addr :port port)
                 (loop
                    (mongrel2:with-connection (*connection* :sender-uuid sender-uuid
                                                            :sub-addr sub-addr
                                                            :pub-addr pub-addr)
                      (restas-dispatcher (mongrel2:recv *connection*))))))))
    (bordeaux-threads:make-thread  #'impl :name "Mongrel2 handler")))
