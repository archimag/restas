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
  ())

(defmethod initialize-instance :after ((acceptor restas-acceptor) &key)
  (setf (hunchentoot:acceptor-request-dispatcher acceptor)
        'restas-dispatcher))

(defmethod hunchentoot:acceptor-request-class ((acceptor restas-acceptor))
  'restas-request)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dispatcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun restas-dispatcher (req)
  (let ((match-result (routes:match *mapper*
                                    (hunchentoot:request-uri req)
                                    (acons :method (hunchentoot:request-method hunchentoot:*request*) nil))))
    (if match-result
        (gp:with-garbage-pool (*request-pool*)
          (let ((*bindings* (cdr match-result)))
            (process-route (car match-result)
                           (cdr match-result))))
        (setf (hunchentoot:return-code*)
              hunchentoot:+HTTP-NOT-FOUND+))))


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
           (route (car (routes:match restas::*mapper*
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
;; start-hunchentoot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *acceptor* nil)

(defun start-web-server (&optional (port 8080))
  (if *acceptor*
      (error "web server has already been started")
      (setf *acceptor*
            (hunchentoot:start (make-instance 'restas-acceptor :port port)))))

(defun stop-web-server ()
  (if *acceptor*
      (progn
        (hunchentoot:stop *acceptor*)
        (setf *acceptor* nil))
      (warn "web server has not yet been started")))
      
