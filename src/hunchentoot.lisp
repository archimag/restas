;;; hunchentoot.lisp

(in-package :restas)

(setf hunchentoot:*hunchentoot-default-external-format* hunchentoot::+utf-8+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debuggable-acceptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *catch-errors-p* t)

(defclass debuggable-acceptor (hunchentoot:acceptor)
    ())

(defmethod hunchentoot:acceptor-request-dispatcher ((acceptor debuggable-acceptor))
  (if *catch-errors-p*
      (call-next-method)
      (let ((dispatcher (handler-bind ((error #'invoke-debugger))
                          (call-next-method))))
        (lambda (request)
          (handler-bind ((error #'invoke-debugger))
            (funcall dispatcher request))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restas-acceptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass restas-request (hunchentoot:request)
  ((substitutions :initarg substitutions :initform routes:+no-bindings+ :accessor restas-request-bindings)))

(defclass restas-acceptor (debuggable-acceptor)
  ((mappers :initform (make-hash-table :test 'equal))))

(defmethod initialize-instance :after ((acceptor restas-acceptor) &key)
  (setf (hunchentoot:acceptor-request-dispatcher acceptor)
        'restas-dispatcher))

(defmethod hunchentoot:acceptor-request-class ((acceptor restas-acceptor))
  'restas-request)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dispatcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *acceptors* nil)

(defparameter *hosts-mappers* (make-hash-table :test 'equal))

(defparameter *default-host-redirect* nil)

(defun parse-host (host)
  (if host
      (let ((hostname/port (split-sequence:split-sequence #\: host)))
        (list (if (string= (first hostname/port) "")
                  nil
                  (first hostname/port))
              (parse-integer (or (second hostname/port) "80"))))
      (list nil 80)))

(defun find-mapper (host)
  (let ((hostname/port (parse-host host)))
    (or (gethash hostname/port *hosts-mappers*)
        (gethash (list nil (second hostname/port))
                 *hosts-mappers*))))

(defun restas-dispatcher (req)
  (let ((mapper (find-mapper (hunchentoot:host req))))
    (when (and (not mapper)
               *default-host-redirect*)
      (hunchentoot:redirect (hunchentoot:request-uri req)
                            :host *default-host-redirect*))
    (let ((match-result (if mapper
                            (routes:match mapper
                                          (hunchentoot:request-uri req)
                                          (acons :method (hunchentoot:request-method hunchentoot:*request*) nil)))))
      (if match-result
          (gp:with-garbage-pool (*request-pool*)
            (let ((*bindings* (cdr match-result)))
              (process-route (car match-result)
                             (cdr match-result))))
          (setf (hunchentoot:return-code*)
                hunchentoot:+HTTP-NOT-FOUND+)))))


;;;; redirect

(defun redirect (route-symbol &rest args)
  (flet ((username ()
           (cdr (assoc :user-login-name restas:*bindings*))))
    (let* ((url (apply-format-aux route-symbol
                                  (mapcar #'(lambda (s)
                                              (if (stringp s)
                                                  (hunchentoot:url-encode s)
                                                  s))
                                          args)))
           (route (car (routes:match (find-mapper (hunchentoot:host))
                                     url
                                     (acons :method :get nil))))
           (required-login-status (restas::route-required-login-status route)))
      (hunchentoot:redirect (if (or (null required-login-status)
                                    (and (eql required-login-status :logged-on)
                                         (username))
                                    (and (eql required-login-status :not-logged-on)
                                         (null (username))))
                                (hunchentoot:url-decode url)
                                "/")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start-site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun start-site (site &key hostname (port 80))
  (unless (find site *sites*)
    (setf (gethash (list hostname port)
                   *hosts-mappers*)
          (symbol-value (find-symbol "*MAPPER*" site)))
    (push (find-package site) *sites*)
    (reconnect-all-sites))
  (unless (find port *acceptors* :key #'hunchentoot:acceptor-port )
    (push (hunchentoot:start (make-instance 'restas-acceptor
                                            :port port))
          *acceptors*)))
    
    
