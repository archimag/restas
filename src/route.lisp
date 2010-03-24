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
   (arbitrary-requirement :initarg :arbitrary-requirement :initform nil :reader route-arbitrary-requirement)
   (render-method :initarg :render-method :initform #'identity)))

(defun route-render-method (route)
  (or (slot-value route 'render-method)
      (string-symbol-value +render-method-symbol+
                           (slot-value (slot-value route
                                                   'submodule)
                                       'module))
      #'identity))

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
           (funcall (route-render-method route)
                    res))))))

(defclass simple-route (base-route)
  ((symbol :initarg :symbol)))

(defmethod process-route ((route simple-route) bindings)
  (let ((*route* route))
    (call-next-method)))

(defmethod process-route/impl ((route simple-route) bindings)
  (let ((*bindings* bindings))
    (funcall (slot-value route 'symbol))))

(defmacro define-route (name (template &key
                                       (content-type "text/html") 
                                       (method :get)
                                       render-method
                                       requirement
                                       parse-vars)
                        &body body)
  (let* ((variables (iter (for var in (routes.unify:template-variables (routes:parse-template template)))
                          (collect (list (intern (symbol-name var))
                                         (list 'cdr (list 'assoc var '*bindings*)))))))
    `(progn
       (defun ,name (,@(if variables (cons '&key variables)))
         ,@body)
       (setf (symbol-plist ',name)
             (list :template ,template
                   :method ,method
                   :content-type ,content-type
                   :parse-vars ,parse-vars
                   :requirement ,requirement
                   :render-method ,render-method))
       (intern (symbol-name ',name)
               (symbol-value (find-symbol +routes-symbol+)))
       (export ',name)
       (eval-when (:execute)
         (reconnect-all-routes)))))

(defun create-route-from-symbol (symbol submodule)
  (make-instance 'simple-route
                 :template (concatenate 'list
                                        (submodule-full-baseurl submodule)
                                        (routes:parse-template (get symbol :template)
                                                               (get symbol :parse-vars)))
                 :symbol symbol
                 :content-type (or (get symbol :content-type)
                                   (string-symbol-value +content-type-symbol+))
                 :required-method (get symbol :method)
                 :arbitrary-requirement (get symbol :requirement)
                 :render-method (get symbol :render-method)
                 :submodule submodule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate url by route
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun route-symbol-template (route-symbol)
  (routes:parse-template (get route-symbol :template)))

(defun genurl/impl (tmpl args)
  (let ((uri (make-instance 'puri:uri)))
    (setf (puri:uri-parsed-path uri)
          (cons :absolute
                (routes::apply-bindings tmpl 
                                        (iter (for pair in (alexandria:plist-alist args))
                                              (collect (cons (car pair)
                                                             (if (or (stringp (cdr pair))
                                                                     (consp (cdr pair)))
                                                                 (cdr pair)
                                                                 (write-to-string (cdr pair)))))))))
    uri))


(defun genurl (route-symbol &rest args)
  (puri:render-uri (genurl/impl (concatenate 'list
                                             (submodule-full-baseurl (slot-value *route* 'submodule))
                                             (route-symbol-template route-symbol))
                                args)
                   nil))

(defun genurl-toplevel (submodule route-symbol &rest args)
  (puri:render-uri (genurl/impl (concatenate 'list
                                             (submodule-full-baseurl (submodule-toplevel (slot-value *route* 'submodule)))
                                             (if submodule
                                                 (submodule-baseurl submodule))
                                             (route-symbol-template route-symbol))
                                args)
                   nil))
  


(defun genurl-with-host (route &rest args)
  (let ((uri (genurl/impl (concatenate 'list
                                       (submodule-full-baseurl (slot-value *route* 'submodule))
                                       (route-symbol-template route))
                          args)))
    (setf (puri:uri-scheme uri)
          :http)
    (setf (puri:uri-host uri)
          (hunchentoot:host))
    (puri:render-uri uri nil)))


