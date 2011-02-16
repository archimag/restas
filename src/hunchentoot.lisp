;;;; hunchentoot.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

(setf hunchentoot:*hunchentoot-default-external-format* hunchentoot::+utf-8+)

(defun request-full-uri (&optional (request hunchentoot:*request*))
  (let ((uri (puri:parse-uri (hunchentoot:request-uri request))))
    (setf (puri:uri-scheme uri)
          (if (hunchentoot:acceptor-ssl-p (hunchentoot:request-acceptor request))
              :https
              :http))
    (ppcre:register-groups-bind  (host port) ("([^:]+)(?=:(.+))?" (hunchentoot:host request))
      (setf (puri:uri-host uri)
            host)
      (unless (or (null port)
                  (= (parse-integer port) 80))
        (setf (puri:uri-port uri)
              port)))
    uri))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restas-request
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass restas-request (hunchentoot:request)
  ((substitutions :initarg substitutions :initform routes:+no-bindings+ :accessor restas-request-bindings)))

(defmethod hunchentoot:process-request :around ((request restas-request))
  (let ((*handle-http-errors-p* *handle-http-errors-p*))
    (call-next-method)))

(defmethod hunchentoot:header-in ((name (eql :host)) (request restas-request))
  (or (hunchentoot:header-in :x-forwarded-host request)
      (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restas-acceptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass restas-acceptor-mixin ()
  ((vhosts :initform nil :accessor restas-acceptor-vhosts)))
    
(defclass restas-acceptor (hunchentoot:acceptor restas-acceptor-mixin) 
  ())

(defclass restas-ssl-acceptor (hunchentoot:ssl-acceptor restas-acceptor-mixin) 
  ())

(defmethod shared-initialize :after ((acceptor restas-acceptor-mixin) slot-names &rest initargs &key)
  (declare (ignore slot-names initargs))
  (setf (hunchentoot:acceptor-request-class acceptor) 'restas-request))

(defmethod hunchentoot::acceptor-status-message :around ((acceptor restas-acceptor-mixin) http-status-code &key &allow-other-keys)
  (if *handle-http-errors-p*
      (call-next-method)))

(defmethod hunchentoot:acceptor-dispatch-request :before ((acceptor restas-acceptor-mixin) request)
  (dolist (function *before-dispatch-request-hook*)
      (funcall function)))

(defmethod hunchentoot:acceptor-dispatch-request :after ((acceptor restas-acceptor-mixin) request)
  (dolist (function *after-dispatch-request-hook*)
      (funcall function)))

(defun format-host (acceptor request)
  (let ((host (cdr (assoc :host (hunchentoot:headers-in request)))))
    (if (find #\: host)
        host
        (format nil "~A:~A" host (hunchentoot:acceptor-port acceptor)))))

(defun restas-dispatch-request (acceptor request)
  (let ((vhost (find-vhost (format-host acceptor request)))
        (hunchentoot:*request* request))
    (when (and (not vhost) *default-host-redirect*)
      (hunchentoot:redirect (hunchentoot:request-uri*)
                            :host *default-host-redirect*))
    (flet ((not-found-if-null (thing)
             (unless thing
               (setf (hunchentoot:return-code*)
                     hunchentoot:+HTTP-NOT-FOUND+)
               (hunchentoot:abort-request-handler))))
      (not-found-if-null vhost)
      (with-memoization 
        (multiple-value-bind (route bindings) (routes:match (slot-value vhost 'mapper)
                                                (hunchentoot:request-uri*))
          (not-found-if-null route)
          (handler-bind ((error #'maybe-invoke-debugger))
            (process-route route bindings)))))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor restas-acceptor) request)
  (restas-dispatch-request acceptor request))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor restas-ssl-acceptor) request)
  (restas-dispatch-request acceptor request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start (module &key 
              ssl-certificate-file ssl-privatekey-file ssl-privatekey-password
              hostname (port (if ssl-certificate-file 443 80)) (context (make-context))
              &aux (hostname/port (if hostname (format nil "~A:~A" hostname port))))
  (unless (find port *acceptors* :key #'hunchentoot:acceptor-port)
    (push (hunchentoot:start
           (if ssl-certificate-file
               (make-instance 'restas-ssl-acceptor
                              :ssl-certificate-file ssl-certificate-file
                              :ssl-privatekey-file ssl-privatekey-file
                              :ssl-privatekey-password ssl-privatekey-password
                              :port port)
               (make-instance 'restas-acceptor
                              :port port)))
          *acceptors*))
  (add-toplevel-submodule hostname/port
                        (make-submodule module :context context))
  (reconnect-all-routes :reinitialize nil))
    
    
