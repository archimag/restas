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
    (ppcre:register-groups-bind  (host port) ("([^:]+)(?=:(.+))?"
                                              (hunchentoot:host request))
      (setf (puri:uri-host uri)
            host)
      (unless (or (null port)
                  (= (parse-integer port) 80))
        (setf (puri:uri-port uri)
              port)))
    uri))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restas-request
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass restas-request (hunchentoot:request)
  ((substitutions :initarg substitutions :initform routes:+no-bindings+
                  :accessor restas-request-bindings)))

(defmethod hunchentoot:process-request :around ((request restas-request))
  (let ((*standard-special-page-p* t))
    (call-next-method)))

(defmethod hunchentoot:header-in ((name (eql :host)) (request restas-request))
  (or (hunchentoot:header-in :x-forwarded-host request)
      (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restas-acceptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass restas-acceptor-mixin () ())

(defclass restas-acceptor (hunchentoot:acceptor restas-acceptor-mixin)
  ()
  (:default-initargs
   :request-class 'restas-request
   :error-template-directory nil
   :access-log-destination nil
   :message-log-destination nil))

#-hunchentoot-no-ssl
(defclass restas-ssl-acceptor (hunchentoot:ssl-acceptor restas-acceptor-mixin)
  ()
  (:default-initargs
   :request-class 'restas-request
   :error-template-directory nil
   :access-log-destination nil
   :message-log-destination nil))

(defmethod hunchentoot:acceptor-status-message
    :around ((acceptor restas-acceptor-mixin) http-status-code
             &key &allow-other-keys)
  (if *standard-special-page-p*
      (call-next-method)))

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
      (multiple-value-bind (route bindings)
          (routes:match (slot-value vhost 'mapper)
            (hunchentoot:request-uri*))
        (not-found-if-null route)
        (handler-bind ((error #'maybe-invoke-debugger))
          (let ((result (process-route route bindings)))
            (cond
              ((pathnamep result)
               (hunchentoot:handle-static-file
                result
                (or (hunchentoot:mime-type result)
                    (hunchentoot:content-type hunchentoot:*reply*))))
              (t result))))))))

(defmethod hunchentoot:acceptor-dispatch-request
    ((acceptor restas-acceptor) request)
  (restas-dispatch-request acceptor request))

#-hunchentoot-no-ssl
(defmethod hunchentoot:acceptor-dispatch-request
    ((acceptor restas-ssl-acceptor) request)
  (restas-dispatch-request acceptor request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start (module
              &key
                ssl-certificate-file ssl-privatekey-file ssl-privatekey-password
                hostname (port (if ssl-certificate-file 443 80))
                address
                acceptor-class
                (context (make-context))
                url
                render-method
                decorators)
  (unless (find-package module)
    (error "Package ~A not found" module) )
  (add-toplevel-module module hostname port
                       :context context
                       :url url
                       :render-method render-method
                       :decorators decorators)
  (unless (find port *acceptors* :key #'hunchentoot:acceptor-port)
    (push (hunchentoot:start
           (if ssl-certificate-file
               (make-instance (or acceptor-class 'restas-ssl-acceptor)
                              :ssl-certificate-file ssl-certificate-file
                              :ssl-privatekey-file ssl-privatekey-file
                              :ssl-privatekey-password ssl-privatekey-password
                              :address address
                              :port port)
               (make-instance (or acceptor-class 'restas-acceptor)
                              :address address
                              :port port)))
          *acceptors*))
  (values))

(defun stop-all (&key soft)
  (setf *vhosts* nil)
  (iter (for acceptor in *acceptors*)
        (hunchentoot:stop acceptor :soft soft))
  (setf *acceptors* nil))
