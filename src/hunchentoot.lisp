;;;; hunchentoot.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)


(defmethod request-method ((request hunchentoot:request))
  (hunchentoot:request-method request))

(defmethod post-parameters ((request hunchentoot:request))
  (hunchentoot:post-parameters request))

(defmethod (setf header-out) (new-value name (reply hunchentoot:reply))
  (setf (hunchentoot:header-out name reply) new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let ((*standard-special-page-p* t))
    (call-next-method)))

(defmethod hunchentoot:header-in ((name (eql :host)) (request restas-request))
  (or (hunchentoot:header-in :x-forwarded-host request)
      (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restas-acceptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass restas-acceptor-mixin () ())
   
(defclass restas-acceptor (hunchentoot:acceptor restas-acceptor-mixin) 
  ()
  (:default-initargs
   :request-class 'restas-request
   :error-template-directory nil))

(defclass restas-ssl-acceptor (hunchentoot:ssl-acceptor restas-acceptor-mixin) 
  ()
  (:default-initargs
   :request-class 'restas-request
   :error-template-directory nil))

(defmethod hunchentoot:acceptor-status-message :around ((acceptor restas-acceptor-mixin) http-status-code &key &allow-other-keys)
  (if *standard-special-page-p*
      (call-next-method)))

(defmethod hunchentoot:acceptor-dispatch-request :before ((acceptor restas-acceptor-mixin) request)
  (dolist (function *before-dispatch-request-hook*)
      (funcall function)))

(defmethod hunchentoot:acceptor-dispatch-request :after ((acceptor restas-acceptor-mixin) request)
  (dolist (function *after-dispatch-request-hook*)
      (funcall function)))

(defun request-hostname-port (acceptor request)
  (let ((host (cdr (assoc :host (hunchentoot:headers-in request)))))
    (if (find #\: host)
        (destructuring-bind (hostname port) (ppcre:split ":" host)
          (cons hostname
                (parse-integer port)))
        (cons host (hunchentoot:acceptor-port acceptor)))))


(defun restas-dispatch-request (acceptor request)
  (let ((vhost (find-vhost (request-hostname-port acceptor request)))
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
              hostname (port (if ssl-certificate-file 443 80)) (context (make-context)))
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
  (add-toplevel-submodule (make-submodule module :context context)
                          hostname
                          port)
  (reconnect-all-routes :reinitialize nil))
    
    
