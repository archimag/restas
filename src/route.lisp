;;;; route.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :restas)

(defgeneric process-route (route bindings))
(defgeneric route-submodule (route))

(defclass route (routes:route)
  ((symbol :initarg :symbol :reader route-symbol)
   (submodule :initarg :submodule :initform nil :reader route-submodule)
   (required-method :initarg :required-method :initform nil :reader route-required-method)
   (arbitrary-requirement :initarg :arbitrary-requirement :initform nil :reader route-arbitrary-requirement)
   (render-method :initarg :render-method :initform #'identity)
   (headers :initarg :headers :initform nil :reader route-headers)))

(defun string-symbol-value (string &optional (package *package*))
  (symbol-value (find-symbol string package)))

(defun route-render-method (route)
  (or (slot-value route 'render-method)
      (string-symbol-value +render-method-symbol+
                           (slot-value (slot-value route
                                                   'submodule)
                                       'module))
      #'identity))

(defmethod routes:route-check-conditions :around ((route routes:base-route) bindings)
  (let ((*route* route))
    (with-submodule (route-submodule route)
      (call-next-method))))

(defmethod routes:route-check-conditions ((route route) bindings)
  (with-slots (required-method arbitrary-requirement) route
    (and (if required-method
             (eql (hunchentoot:request-method*) required-method)
             t)
         (if arbitrary-requirement
             (let ((*bindings* bindings))
               (funcall arbitrary-requirement))
             t))))

(defmethod routes:route-name ((route route))
  (string-downcase (write-to-string (slot-value route 'symbol))))

(defmethod route-submodule ((route routes:proxy-route))
  (route-submodule (routes:proxy-route-target route)))

(defmethod process-route :around ((route routes:base-route) bindings)
  (with-submodule (route-submodule route)
    (call-next-method)))

(defmethod process-route ((route route) bindings)
  (alexandria:doplist (name value (route-headers route))
    (setf (hunchentoot:header-out name)
          (if (functionp value)
              (funcall value)
              value)))
  (let ((*route* route)
        (*bindings* bindings))
    (render-object (route-render-method route)
                   (catch 'route-done
                     (funcall (slot-value route 'symbol))))))

(defun abort-route-handler (obj &key return-code content-type)
  (when return-code
    (setf (hunchentoot:return-code*) return-code
          *standard-special-page-p* nil))
  (when content-type
    (setf (hunchentoot:content-type*) content-type))
  (throw 'route-done obj))

(defmacro define-route (name (template
                              &key
                              (method :get)
                              content-type
                              render-method
                              requirement
                              validators
                              parse-vars
                              headers
                              decorators)
                        &body body)
  (let* ((template-value (if (symbolp template)
                             (symbol-value template)
                             template))
         (variables (iter (for var in (routes:template-variables (routes:parse-template template-value)))
                          (collect (list (intern (symbol-name var))
                                         (list 'cdr (list 'assoc var '*bindings*)))))))
    `(progn
       (defun ,name (,@(if variables (cons '&key variables)))
         ,@body)
       (setf (symbol-plist ',name)
             (list :template ,template
                   :method ,method
                   :content-type ,content-type
                   :validators ,validators
                   :parse-vars ,parse-vars
                   :requirement ,requirement
                   :render-method ,render-method
                   :headers ,headers
                   :decorators ,decorators))
       (intern (symbol-name ',name)
               (symbol-value (find-symbol +routes-symbol+)))
       (export ',name)
       (eval-when (:execute)
         (reconnect-all-routes)))))

(defun route-template-from-symbol (symbol submodule)
  (concatenate 'list
               (submodule-full-baseurl submodule)
               (routes:parse-template (get symbol :template)
                                      (append (iter (for lst on (get symbol :validators) by #'cddr)
                                                    (collect (first lst))
                                                    (collect (data-sift:compile-rule (second lst))))
                                              (get symbol :parse-vars)))))

(defun apply-decorators (route decorators)
  (if decorators
      (apply-decorators (funcall (car decorators)
                                 route)
                        (cdr decorators))
      route))

(defun create-route-from-symbol (symbol submodule)
  (let* ((headers (append (string-symbol-value +headers-symbol+ 
					       (symbol-package symbol))
			  (get symbol :headers)))
	 (content-type (get symbol :content-type)))
    (cond
      (content-type 
       (setf (getf headers :content-type) content-type))
      ((not (getf headers :content-type))
       (setf (getf headers :content-type)
	     (or (string-symbol-value +content-type-symbol+
				      (symbol-package symbol))
		 "text/html"))))
    (apply-decorators (make-instance 'route
                                     :template (route-template-from-symbol symbol submodule)
                                     :symbol symbol
                                     :required-method (get symbol :method)
                                     :arbitrary-requirement (get symbol :requirement)
                                     :render-method (get symbol :render-method)
                                     :submodule submodule
                                     :headers headers)
                      (get symbol :decorators))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; proxy route
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-route ((route routes:proxy-route) bindings)
  (process-route (routes:proxy-route-target route)
                 bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate url by route
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun route-symbol-template (route-symbol)
  (routes:parse-template (get route-symbol :template)))

(defun genurl/impl (tmpl args)
  (let* ((uri (make-instance 'puri:uri))
         (bindings (iter (for rest on args by #'cddr)
                         (for key = (first rest))
                         (for value = (second rest))
                         (collect
                             (cons key
                                   (if (or (stringp value) (consp value))
                                   value
                                   (write-to-string value))))))
         (query-part (set-difference bindings
                                     (routes:template-variables tmpl)
                                     :test (alexandria:named-lambda known-variable-p (pair var)
                                             (eql (car pair) var)))))
    (setf (puri:uri-parsed-path uri)
          (cons :absolute
                (routes::apply-bindings tmpl bindings)))
    (when query-part
      (setf (puri:uri-query uri)
            (format nil
                    "~{~(~A~)=~A~^&~}"
                    (alexandria:flatten query-part))))
    uri))


(defun genurl (route-symbol &rest args)
  (puri:render-uri (genurl/impl (concatenate 'list
                                             (submodule-full-baseurl *submodule*)
                                             (route-symbol-template route-symbol))
                                args)
                   nil))

(defun genurl-submodule (submodule-symbol route-symbol &rest args)
  (puri:render-uri (genurl/impl (concatenate 'list
                                             (submodule-full-baseurl (if submodule-symbol
                                                                         (find-submodule  submodule-symbol)
                                                                         *submodule*))
                                             (route-symbol-template route-symbol))
                                args)
                   nil))

(defun gen-full-url (route &rest args)
  (let ((uri (genurl/impl (concatenate 'list
                                       (submodule-full-baseurl *submodule*)
                                       (route-symbol-template route))
                          args)))
    (setf (puri:uri-scheme uri)
          :http)
    (setf (puri:uri-host uri)
          (if (boundp 'hunchentoot:*request*)
                      (hunchentoot:host)
                      "localhost"))
    (puri:render-uri uri nil)))

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
;;; parse url for route
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-route-url (url route-symbol &optional submodule-symbol)
  (let ((mapper (make-instance 'routes:mapper))
        (submodule (if submodule-symbol
                       (find-submodule  submodule-symbol)
                       *submodule*)))
    (routes:connect mapper
                    (make-instance 'route
                                   :template (route-template-from-symbol route-symbol
                                                                         submodule)
                                   :submodule submodule))
    (multiple-value-bind (route bindings) (routes:match mapper url)
      (if route
          (alexandria:alist-plist bindings)))))
  
