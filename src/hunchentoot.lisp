;;;; hunchentoot.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

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
;;;; WSAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun encoding-hunchentoot-external-format (encoding)
  (case encoding
    (:utf-8 hunchentoot::+utf-8+)
    (:latin1 hunchentoot::+latin-1+)
    (otherwise encoding)))

(defun hunchentoot-external-format-encoding (external-format)
  (cond
    ((eql external-format hunchentoot::+utf-8+ ) :utf-8)
    ((eql external-format hunchentoot::+latin-1+ ) :latin1)
    (t external-format)))

;;; request

(defmethod wsal:get-parameters ((request hunchentoot:request))
  (hunchentoot:get-parameters request))

(defmethod wsal:post-parameters ((request hunchentoot:request))
  (hunchentoot:post-parameters request))

(defmethod wsal:cookies-in ((request hunchentoot:request))
  (hunchentoot:cookies-in request))

(defmethod wsal:query-string ((request hunchentoot:request))
  (hunchentoot:query-string request))

(defmethod wsal:request-method ((request hunchentoot:request))
  (hunchentoot:request-method request))

(defmethod wsal:request-uri ((request hunchentoot:request))
  (hunchentoot:request-uri request))

(defmethod wsal:server-protocol ((request hunchentoot:request))
  (hunchentoot:server-protocol request))

(defmethod wsal:headers-in ((request hunchentoot:request))
  (hunchentoot:headers-in request))

(defmethod wsal:remote-address ((request hunchentoot:request))
  (hunchentoot:remote-addr request))

(defmethod wsal:remote-port ((request hunchentoot:request))
  (hunchentoot:remote-port request))

(defmethod wsal:script-name ((request hunchentoot:request))
  (hunchentoot:script-name request))

(defmethod wsal:raw-post-data (request &key encoding force-text force-binary &allow-other-keys)
  (hunchentoot:raw-post-data :request request
                             :external-format (encoding-hunchentoot-external-format encoding)
                             :force-text force-text
                             :force-binary force-binary))

;;; reply

(defmethod wsal:headers-out ((reply hunchentoot:reply))
  (hunchentoot:headers-out reply))

(defmethod (setf wsal:headers-out) (newvalue (reply hunchentoot:reply))
  (setf (slot-value reply 'hunchentoot:headers-out)
        newvalue))

(defmethod wsal:content-length ((reply hunchentoot:reply))
  (hunchentoot:content-length reply))

(defmethod wsal:content-type ((reply hunchentoot:reply))
  (hunchentoot:content-type reply))

(defmethod wsal:cookies-out ((reply hunchentoot:reply))
  (hunchentoot:cookies-out reply))

(defmethod (setf wsal:cookies-out) (newvalue (reply hunchentoot:reply))
  (setf (hunchentoot:cookies-out reply)
        newvalue))

(defmethod wsal:return-code ((reply hunchentoot:reply))
  (hunchentoot:return-code reply))

(defmethod (setf wsal:return-code) (newvalue (reply hunchentoot:reply))
  (setf (hunchentoot:return-code reply)
        newvalue))

(defmethod wsal:reply-external-format ((reply hunchentoot:reply))
  (hunchentoot-external-format-encoding (hunchentoot:reply-external-format reply)))

(defmethod (setf wsal:reply-external-format) (newvalue (reply hunchentoot:reply))
  (hunchentoot-external-format-encoding 
   (setf (hunchentoot:reply-external-format reply)
         (encoding-hunchentoot-external-format newvalue))))

(defmethod hunchentoot::stringify-cookie ((cookie wsal:cookie))
  (wsal:stringify-cookie cookie))

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
        (wsal:*request* request)
        (wsal:*reply* hunchentoot:*reply*))
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
            (let ((result (process-route route bindings)))
              (cond
                ((pathnamep result)
                 (hunchentoot:handle-static-file result
                                                 (or (wsal:mime-type result)
                                                     (wsal:content-type wsal:*reply*))))
                (t result)))))))))

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
    
    
