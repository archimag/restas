;;;; hunchentoot.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

(setf hunchentoot:*hunchentoot-default-external-format* hunchentoot::+utf-8+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redirect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-format-aux (format args)
  (if (symbolp format)
      (apply #'restas:genurl format args)
      (if args
          (apply #'format nil (cons format args))
          format)))

(defun redirect (route-symbol &rest args)
  (hunchentoot:redirect 
   (hunchentoot:url-decode
    (apply-format-aux route-symbol
                      (mapcar #'(lambda (s)
                                  (if (stringp s)
                                      (hunchentoot:url-encode s)
                                      s))
                              args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restas-acceptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass restas-request (hunchentoot:request)
  ((substitutions :initarg substitutions :initform routes:+no-bindings+ :accessor restas-request-bindings)))

(defmethod hunchentoot:header-in ((name (eql :host)) (request restas-request))
  (or (hunchentoot:header-in :x-forwarded-host request)
      (call-next-method)))

(defclass vhost ()
  ((host :initarg :host :reader vhost-host)
   (mapper :initform (make-instance 'routes:mapper))
   (modules :initform nil)))

(defclass restas-acceptor (hunchentoot:acceptor) 
  ((vhosts :initform nil :accessor restas-acceptor-vhosts)))

(defmethod shared-initialize :after ((acceptor restas-acceptor) slot-names &rest initargs &key)
  (declare (ignore slot-names initargs))
  (setf (hunchentoot:acceptor-request-dispatcher acceptor) 'restas-dispatcher
        (hunchentoot:acceptor-request-class acceptor) 'restas-request))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dispatcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *default-host-redirect* nil)

(defvar *request-pool*)

(defun header-host (request)
  (cdr (assoc :host (hunchentoot:headers-in request))))

(defun restas-dispatcher (req &aux (host (header-host req)))
  (let ((vhost (or (find host
                         (restas-acceptor-vhosts hunchentoot:*acceptor*)
                         :key #'vhost-host
                         :test #'string=)
                   (find-if #'null
                            (restas-acceptor-vhosts hunchentoot:*acceptor*)
                            :key #'vhost-host)))
        (hunchentoot:*request* req))
    (when (and (not vhost)
               *default-host-redirect*)
      (hunchentoot:redirect (hunchentoot:request-uri*)
                            :host *default-host-redirect*))
    (when vhost
      (multiple-value-bind (route bindings) (routes:match (slot-value vhost 'mapper)
                                                         (hunchentoot:request-uri*))
        (if route
            (gp:with-garbage-pool (*request-pool*)
              (let ((*bindings* bindings))
                (process-route route
                               bindings)))
            (setf (hunchentoot:return-code*)
                  hunchentoot:+HTTP-NOT-FOUND+))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reconnect-all-routes ()
  (iter (for acceptor in *acceptors*)
        (iter (for vhost in (restas-acceptor-vhosts acceptor))
              (let ((mapper (slot-value vhost 'mapper)))
                (routes:reset-mapper mapper)
                (iter (for module in (slot-value vhost 'modules))
                      (connect-submodule module mapper))))))


(defun start (module &key hostname (port 80) (context (make-context))
              &aux (hostname/port (if hostname (format nil "~A:~A" hostname port))))
  (let* ((package (or (find-package module)
                      (error "Package ~A not found" module)))
         (acceptor (or (find port
                            *acceptors*
                            :key #'hunchentoot:acceptor-port)
                      (car (push (hunchentoot:start (make-instance 'restas-acceptor
                                                                   :port port))
                                 *acceptors*))))
         (vhost (or (if hostname/port
                        (find hostname/port
                              (restas-acceptor-vhosts acceptor)
                              :key #'vhost-host
                              :test #'string=)
                        (find-if #'null
                                 (restas-acceptor-vhosts acceptor)
                                 :key #'vhost-host))
                    (car (push (make-instance 'vhost
                                              :host hostname/port)
                               (restas-acceptor-vhosts acceptor))))))
    (push (make-instance 'submodule
                         :module package
                         :context context)
          (slot-value vhost 'modules))
    (reconnect-all-routes)))
    
    
