;;;; route.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :restas)

(defvar *route* nil)

(defvar *bindings*)


(defgeneric process-route (route bindings))
(defgeneric process-route/impl (route bindings))

(defclass base-route (routes:route)
  ((submodule :initarg :submodule :initform nil)
   (content-type :initarg :content-type :initform nil :reader route-content-type)
   (required-method :initarg :required-method :initform nil :reader route-required-method)
   (arbitrary-requirement :initarg :arbitrary-requirement :initform nil :reader route-arbitrary-requirement)))

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
  (let ((*route* route))
    (call-next-method)))

(defmethod process-route/impl ((route simple-route) bindings)
  (funcall (get (slot-value route 'symbol)
                :handler)))


(defmacro define-route (name (template &key content-type (method :get) requirement parse-vars) &body body)
  (let* ((variables (iter (for var in (routes.unify:template-variables (routes:parse-template template)))
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
             (routes:parse-template ,template))
       (setf (get ',name :initialize)
             #'(lambda (submodule)
                 (make-instance 'simple-route
                                :template (concatenate 'list
                                                       (submodule-full-baseurl submodule)
                                                       (routes:parse-template ,template ,parse-vars))
                                :symbol ',name
                                :content-type (or ,content-type "text/html")
                                :required-method ,method
                                :arbitrary-requirement ,requirement
                                :submodule submodule)))
       (intern (symbol-name ',name)
               (symbol-value (find-symbol +routes-symbol+)))
       (export ',name)
       (eval-when (:execute)
         (reconnect-all-routes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate url by route
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun genurl/impl (tmpl args)
  (format nil
          "/~{~A~^/~}"
          (routes::apply-bindings tmpl 
                                  (iter (for pair in (alexandria:plist-alist args))
                                        (collect (cons (car pair)
                                                       (if (or (stringp (cdr pair))
                                                               (consp (cdr pair)))
                                                           (cdr pair)
                                                           (write-to-string (cdr pair)))))))))


(defun genurl (route-symbol &rest args)
  (genurl/impl (concatenate 'list
                            (submodule-full-baseurl (slot-value *route* 'submodule))
                            (get route-symbol :template))
               args))

(defun genurl-toplevel (submodule route-symbol &rest args)
  (genurl/impl (concatenate 'list
                            (submodule-full-baseurl (submodule-toplevel (slot-value *route* 'submodule)))
                            (if submodule
                                (submodule-baseurl submodule))
                            (get route-symbol :template))
               args))
  


(defun genurl-with-host (route &rest args)
  (format nil
          "http://~A~A"
          (hunchentoot:host)
          (apply #'restas:genurl route args)))


