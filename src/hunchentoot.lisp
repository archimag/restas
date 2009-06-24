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

(defun chrome-resolver (url id ctxt)
  (declare (ignore id))
  (if (eql (puri:uri-scheme url) :chrome)
      (let* ((match-result (routes:match *chrome-mapper*
                                         (concatenate 'string
                                                      (puri:uri-host url)
                                                      (puri:uri-path url))
                                         (acons :method :get (if (boundp '*bindings*)
                                                                 *bindings*
                                                                 (restas-request-bindings hunchentoot:*request*))))))
        (if match-result
            (gp:with-garbage-pool (*request-pool*)
              (let ((result (process-route (car match-result)
                                           (cdr match-result))))
                (typecase result
                  (string (xtree:resolve-string result ctxt))
                  (pathname (xtree:resolve-file/url (namestring result) ctxt ))
                  (xtree::libxml2-cffi-object-wrapper (xtree:resolve-string (xtree:serialize result
                                                                                             :to-string)
                                                                            ctxt)))))))))

(defun restas-dispatcher (req)
  (let ((match-result (routes:match *mapper*
                                    (hunchentoot:request-uri req)
                                    (acons :method (hunchentoot:request-method hunchentoot:*request*) nil))))
    (if match-result
        (xtree:with-custom-resolvers ('chrome-resolver)
          (gp:with-garbage-pool (*request-pool*)
            (let ((*bindings* (cdr match-result)))
              (process-route (car match-result)
                             (cdr match-result)))))
        (setf (hunchentoot:return-code*)
                hunchentoot:+HTTP-NOT-FOUND+))))


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
      
