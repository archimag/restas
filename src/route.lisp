;;;; route.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :restas)

(defgeneric process-route (route bindings))
(defgeneric route-module (route))
(defgeneric make-route-url (obj args))

(defclass route (routes:route)
  ((symbol :initarg :symbol :reader route-symbol)
   (module :initarg :module :initform nil :reader route-module)
   (required-method :initarg :required-method :initform nil
                    :reader route-required-method)
   (arbitrary-requirement :initarg :arbitrary-requirement :initform nil
                          :reader route-arbitrary-requirement)
   (render-method :initarg :render-method :initform #'identity)
   (headers :initarg :headers :initform nil :reader route-headers)
   (variables :initarg :variables :initarg nil)
   (additional-variables :initarg :additional-variables :initform nil)))

(defun route-render-method (route)
  (or (slot-value route 'render-method)
      (module-render-method (route-module route))
      #'identity))

(defmethod routes:route-check-conditions :around ((route routes:base-route)
                                                  bindings)
  (let ((*route* route))
    (with-module (route-module route)
      (call-next-method))))

(defmethod routes:route-check-conditions ((route route) bindings)
  (with-slots (required-method arbitrary-requirement) route
    (and (if required-method
             (eql (hunchentoot:request-method*) required-method)
             t)
         (if arbitrary-requirement
             (if (listp arbitrary-requirement)
                 (every #'funcall arbitrary-requirement)
                 (funcall arbitrary-requirement))
             t))))

(defmethod routes:route-name ((route route))
  (string-downcase (write-to-string (slot-value route 'symbol))))

(defmethod process-route :around ((route routes:base-route) bindings)
  (with-module (route-module route)
    (call-next-method)))

(defmethod process-route ((route route) bindings)
  (alexandria:doplist (name value (route-headers route))
    (setf (hunchentoot:header-out name)
          (if (functionp value)
              (funcall value)
              value)))
  (let ((*route* route)
        (rsymbol (route-symbol route)))
    (render-object
     (route-render-method route)
     (catch 'route-done
       (apply rsymbol
              (append (iter (for item in (slot-value route 'variables))
                            (collect (cdr (assoc item bindings
                                                 :test #'string=))))
                      (iter (for (key fun) in
                                 (slot-value route 'additional-variables))
                            (collect key)
                            (collect (funcall fun)))))))))

(defun abort-route-handler (obj &key return-code content-type)
  (when return-code
    (setf (hunchentoot:return-code*) return-code
          *standard-special-page-p* nil))
  (when content-type
    (setf (hunchentoot:content-type*) content-type))
  (throw 'route-done obj))

;;;; define-route

(defmacro define-route (name (template &key method content-type) &body body)
  (multiple-value-bind (declarations-map real-body)
      (split-code-declarations body)
    (let* ((route-traits (make-hash-table))
           (variables (routes:template-variables
                       (routes:parse-template template)))
           (arglist (mapcar (alexandria:compose #'intern #'symbol-name)
                            variables)))

      (setf (gethash :template route-traits) template
            (gethash :method route-traits) (or method :get)
            (gethash :content-type route-traits) content-type
            (gethash :variables route-traits) variables)

      (parse-all-declarations declarations-map
                              '(:sift-variables :additional-variables
                                :render-method :apply-render-method
                                :content-type :http-method
                                :requirement
                                :decorators)
                              route-traits)

      (setf (gethash :variables route-traits) `',arglist)

      `(progn
         (defun ,name (,@arglist
                       ,@(gethash :additional-variables-arglist route-traits))
           ,@real-body)

         (register-route-traits
          ',name
          (alexandria:plist-hash-table
           (list ,@(iter (for type in
                              '(:template :method :content-type :render-method
                                :parse-vars :requirement :decorators
                                :additional-variables :variables))
                         (for value = (gethash type route-traits))
                         (when value
                           (collect type)
                           (collect value))))))
         (reconnect-all-routes)))))

(defun get-ref-syms (route-symbol &optional syms)
  (iter (for pkg in (pkgmodule-traits-references (symbol-package route-symbol)))
    (iter (for (key nil) in-hashtable (pkgmodule-traits-modules pkg))
      (push (alexandria:format-symbol pkg "~A.~A" key route-symbol) syms)
      (setf syms (get-ref-syms (first syms) syms))))
  syms)

(defun delete-route (route-symbol &aux package)
  (setf package (symbol-package route-symbol))
  (when package
    (remhash route-symbol (pkgmodule-traits-routes package))
    (mapcar (lambda (sym)
              (unintern sym (symbol-package sym)))
            (get-ref-syms route-symbol))
    (unintern route-symbol (symbol-package route-symbol))))

(defun route-template-from-symbol (symbol module)
  (flet ((call-in-context-wrap (fun)
           (alexandria:named-lambda parse-in-context (value)
             (with-module module
               (funcall fun value)))))
    (let ((traits (gethash symbol
                           (pkgmodule-traits-routes (symbol-package symbol)))))
      (concatenate 'list
                   (module-real-url module)
                   (routes:parse-template
                    (gethash :template traits)
                    (iter (for tail on (gethash :parse-vars traits) by #'cddr)
                          (collect (first tail))
                          (collect (call-in-context-wrap (second tail)))))))))

(defun create-route-from-symbol (symbol module)
  (destructuring-bind (&key content-type headers method requirement render-method
                            decorators variables additional-variables
                            &allow-other-keys)
      (alexandria:hash-table-plist (gethash symbol (pkgmodule-traits-routes
                                                    (symbol-package symbol))))
    (setf (getf headers :content-type)
          (or content-type
              (getf headers :content-type)
              (pkgmodule-traits-content-type (symbol-package symbol))
              "text/html"))
    (apply-decorators (make-instance 'route
                                     :template (route-template-from-symbol
                                                symbol module)
                                     :symbol symbol
                                     :required-method method
                                     :arbitrary-requirement requirement
                                     :render-method render-method
                                     :module module
                                     :headers headers
                                     :variables variables
                                     :additional-variables additional-variables)
                      (append (module-decorators module)
                              decorators))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate url by route
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun route-symbol-template (route-symbol)
  (routes:parse-template (gethash :template
                                  (gethash route-symbol
                                           (pkgmodule-traits-routes
                                            (symbol-package route-symbol))))))

(defmethod make-route-url ((tmpl list) args)
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
                                     :test (alexandria:named-lambda
                                               known-variable-p (pair var)
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

(defmethod make-route-url ((route symbol) args)
  (if *module*
      (make-route-url (or (find-route route)
                          (error "Unknown route: ~A" route)) args)
      (make-route-url (route-symbol-template route) args)))

(defmethod make-route-url ((route route) args)
  (make-route-url (routes:route-template route) args))

(defun genurl (route-symbol &rest args &key &allow-other-keys)
  (puri:render-uri (make-route-url route-symbol args) nil))

(defun genurl* (route-symbol &rest args &key &allow-other-keys)
  (let ((url (make-route-url route-symbol args)))
    (setf (puri:uri-scheme url) :http
          (puri:uri-host url) (if (boundp 'hunchentoot:*request*)
                                  (hunchentoot:host)
                                  "localhost"))
    (puri:render-uri url nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redirect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse url for route
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-route-url (url route-symbol)
  (let ((mapper (make-instance 'routes:mapper))
        (module *module*))
    (routes:connect mapper
                    (make-instance 'route
                                   :template
                                   (routes:route-template (module-find-route
                                                           module route-symbol))
                                   ;;(route-template-from-symbol route-symbol module)
                                   :module module))
    (multiple-value-bind (route bindings) (routes:match mapper url)
      (if route
          (alexandria:alist-plist bindings)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; proxy route
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-route ((route routes:proxy-route) bindings)
  (process-route (routes:proxy-route-target route)
                 bindings))

(defmethod route-module ((route routes:proxy-route))
  (route-module (routes:proxy-route-target route)))

(defmethod route-symbol ((route routes:proxy-route))
  (route-symbol (routes:proxy-route-target route)))

(defmethod make-route-url ((route routes:proxy-route) bindings)
  (make-route-url (routes:proxy-route-target route) bindings))
