;;;; route.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :restas)

(defgeneric process-route (route bindings))

(defvar *route* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; routes

(defun parse-template/package (tmpl package &optional parse-vars)
  (concatenate 'list
               (symbol-value (find-symbol +baseurl-symbol+ package))
               (routes:parse-template tmpl parse-vars)))
  
(defun routes/package ()
  (symbol-value (find-symbol +routes-symbol+ *package*)))

(defclass base-route (routes:route)
  ((submodule :initarg :submodule :initform nil)
   (content-type :initarg :content-type :initform nil :reader route-content-type)
   (required-method :initarg :required-method :initform nil :reader route-required-method)
   (arbitrary-requirement :initarg :arbitrary-requirement :initform nil :reader route-arbitrary-requirement)))

(defgeneric process-route/impl (route bindings))

(defmethod routes:route-check-conditions ((route base-route) bindings)
  (with-context (slot-value (slot-value route 'submodule)
                            'context)
    (with-slots (required-method arbitrary-requirement) route
      (and (if required-method
               (eql (hunchentoot:request-method*) required-method)
               t)
           (if arbitrary-requirement
               (let ((*bindings* bindings))
                 (funcall arbitrary-requirement))
               t)))))

(defmethod process-route ((route base-route) bindings)
  (with-context (slot-value (slot-value route 'submodule)
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


(defmacro define-route (name (template &key content-type (method :get) requirement parse-vars) &body body)
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
                                         :template (parse-template/package ,template ,package ,parse-vars)
                                         :symbol ',name
                                         :content-type (or ,content-type "text/html")
                                         :required-method ,method
                                         :arbitrary-requirement ,requirement)))
       (intern (symbol-name ',name) (routes/package))
       (export ',name)
       (eval-when (:execute)
         (reconnect-all-sites)))))

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