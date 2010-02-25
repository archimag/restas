;;;; route.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :restas)

(defgeneric process-route (route bindings))


(defun plugin-update ()
  (reconnect-all-sites))


(defun route-changed (route)
  (declare (ignore route))
  (plugin-update))


(defvar *route* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; routes

(defun parse-template/package (tmpl package)
  (concatenate 'list
               (symbol-value (find-symbol "*BASEURL*" package))
               (routes:parse-template tmpl)))
  
(defun routes/package ()
  (symbol-value (find-symbol "*ROUTES*" *package*)))

(defclass base-route (routes:route)
  ((plugin-instance :initarg :plugin-instance :initform nil)
   (protocol :initarg :protocol :initform :http :reader route-protocol)
   (content-type :initarg :content-type :initform nil :reader route-content-type)
   (required-login-status :initarg :required-login-status :initform nil :reader route-required-login-status)
   (required-method :initarg :required-method :initform nil :reader route-required-method)
   (arbitrary-requirement :initarg :arbitrary-requirement :initform nil :reader route-arbitrary-requirement)))

(defgeneric process-route/impl (route bindings))

(defmethod routes:route-check-conditions ((route base-route) bindings)
  (with-context (slot-value (slot-value route 'plugin-instance)
                            'context)
    (with-slots (required-method required-login-status arbitrary-requirement) route
      (and (if required-method
               (eql (cdr (assoc :method bindings)) required-method)
               t)
           (case required-login-status
             (:logged-on (assoc :user-login-name bindings))
             (:not-logged-on (not (assoc :user-login-name bindings)))
             ((nil) t)
             (otherwise (error "unknow required login status: ~A" required-login-status)))
           (if arbitrary-requirement
               (let ((*bindings* bindings))
                 (funcall arbitrary-requirement))
               t)))))

(defmethod process-route ((route base-route) bindings)
  (with-context (slot-value (slot-value route 'plugin-instance)
                            'context)
    (let ((res (process-route/impl route bindings)))
      (cond
        ((pathnamep res) (hunchentoot:handle-static-file res))
        ((integerp res) (setf (hunchentoot:return-code*)
                              res))
        (t (setf (hunchentoot:content-type*)
                 (or (route-content-type route)
                     "text/html"))
           res)))))
                                          


(defclass simple-route (base-route)
  ((symbol :initarg :symbol)))

(defmethod process-route ((route simple-route) bindings)
  (let ((*route* (slot-value route 'symbol)))
    (call-next-method)))

(defmethod process-route/impl ((route simple-route) bindings)
  (funcall (get (slot-value route 'symbol)
                :handler)))


(defmacro define-route (name (template &key (protocol :http) content-type login-status (method :get) requirement) &body body)
  (let* ((package (symbol-package name))
         (parsed-template (parse-template/package (if (stringp template)
                                                      template
                                                      (eval template))
                                                  package))
         (variables (iter (for var in (routes.unify:template-variables parsed-template))
                          (collect (list (intern (symbol-name var))
                                         (list 'cdr (list 'assoc var '*bindings*))))))
         (handler-body (if variables
                           `((let (,@variables) ,@body))
                           `(,@body))))
    `(progn
       (setf (get ',name :handler)
             #'(lambda ()
                 ,@handler-body))
       (setf (get ',name :template)
             '(parse-template/package ,template ,package))
       (setf (get ',name :initialize)
             #'(lambda () (make-instance 'simple-route
                                         :template (parse-template/package ,template ,package)
                                         :symbol ',name
                                         :protocol ,protocol
                                         :content-type (or ,content-type (symbol-value (find-symbol "*DEFAULT-CONTENT-TYPE*" ,*package*)))
                                         :required-login-status ,login-status
                                         :required-method ,method
                                         :arbitrary-requirement ,requirement)))
       (setf (get ',name :protocol)
             ,protocol)
       (intern (symbol-name ',name) (routes/package))
       (export ',name)
       (eval-when (:execute)
         (route-changed ',name)))))

;;; generate-route-url

(defun genurl (route-symbol &rest args)
  (format nil
          "/~{~A~^/~}"  
          (routes::apply-bindings (eval (get route-symbol :template))
                                  (iter (for pair in (alexandria:plist-alist args))
                                        (collect (cons (car pair)
                                                       (if (or (stringp (cdr pair))
                                                               (consp (cdr pair)))
                                                           (cdr pair)
                                                           (write-to-string (cdr pair)))))))))

(defun genurl-with-host (route &rest args)
  (format nil
          "http://~A~A"
          (hunchentoot:host)
          (apply #'restas:genurl route args)))


(defun apply-format-aux (format args)
  (if (symbolp format)
      (apply #'restas:genurl format args)
      (if args
          (apply #'format nil (cons format args))
          format)))