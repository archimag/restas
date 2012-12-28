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
               (if (listp arbitrary-requirement)
                   (every #'funcall arbitrary-requirement)
                   (funcall arbitrary-requirement)))
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
        (*bindings* bindings)
        (rsymbol (route-symbol route)))
    (render-object (route-render-method route)
                   (catch 'route-done
                     (apply rsymbol
                            (append (iter (for item in (get rsymbol :variables))
                                          (collect (cdr (assoc item bindings :test #'string=))))
                                    (iter (for (key fun) in (get rsymbol :additional-variables))
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

(defgeneric parse-route-declarations (type declarations traits))

(defmethod parse-route-declarations (type declarations traits)
  (error "Unknown ROUTE declaration type: ~A" type))

(defmethod parse-route-declarations ((type (eql :sift-variables)) declarations traits)
  (let ((variables (gethash :variables traits)))
    (setf (gethash :parse-vars traits)
          (cons 'list
                (iter (for (var rule) in declarations)
                      (collect (find var variables :test #'string=))
                      (collect `(data-sift:compile-rule ,rule)))))))

(defmethod parse-route-declarations ((type (eql :additional-variables)) declarations traits)
  (iter (for (var handler default) in declarations)
        (collect (if default
                     (list var default)
                     var)
          :into arglist)
        (collect (if default
                     `(list ,(alexandria:make-keyword var)
                        (alexandria:named-lambda additional-variable-handler ()
                          (or ,handler ,default)))
                     `(list ,(alexandria:make-keyword var)
                        (alexandria:named-lambda additional-variable-handler ()
                          ,handler)))
          :into handlers)
        (finally
         (setf (gethash :additional-variables-arglist traits) (cons '&key arglist)
               (gethash :additional-variables traits) (cons 'list handlers)))))

(defmethod parse-route-declarations ((type (eql :render-method)) declarations traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A ROUTE declaration" type))
  (setf (gethash type traits)
        (first declarations)))

(defmethod parse-route-declarations ((type (eql :apply-render-method)) declarations traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A ROUTE declaration" type))
  (setf (gethash :render-method traits)
        `(alexandria:named-lambda apply-render-method (data)
           (apply ,(first declarations) data))))

(defmethod parse-route-declarations ((type (eql :content-type)) declarations traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A ROUTE declaration" type))
  (setf (gethash type traits)
        (first declarations)))

(defmethod parse-route-declarations ((type (eql :decorators)) declarations traits)
  (setf (gethash type traits)
        (cons 'list declarations)))

(defmethod parse-route-declarations ((type (eql :http-method)) declarations traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A ROUTE declaration" type))
  (setf (gethash :method traits)
        (first declarations)))

(defmethod parse-route-declarations ((type (eql :requirement)) declarations traits)
  (setf (gethash type traits) (cons 'list declarations)))

(defmacro define-route (name (template &key method content-type) &body body)
  (let* ((route-traits (make-hash-table))
         (declarations-map (make-hash-table))
         (variables (routes:template-variables (routes:parse-template template)))
         (arglist (mapcar (alexandria:compose #'intern #'symbol-name) variables))
         (real-body body))
    
    (setf (gethash :template route-traits) template
          (gethash :method route-traits) (or method :get)
          (gethash :content-type route-traits) content-type
          (gethash :variables route-traits) variables)

    (iter (for rbody on body)
          (for form = (car rbody))
          (while (and (consp  form) (keywordp (car form))))
          (setf real-body (cdr rbody))
          (setf (gethash (car form) declarations-map)
                (append (cdr form)
                        (gethash (car form) declarations-map))))

    (iter (for (type declarations) in-hashtable declarations-map)
          (parse-route-declarations type declarations route-traits))

    `(progn
       (defun ,name (,@arglist ,@(gethash :additional-variables-arglist route-traits))
         ,@real-body)

       (setf (symbol-plist ',name)
             (list ,@(iter (for type in '(:template :method :content-type :render-method
                                          :parse-vars :requirement :decorators
                                          :additional-variables))
                           (for value = (gethash type route-traits))
                           (when value
                             (collect type)
                             (collect value)))))
       (setf (get ',name :variables) ',arglist)
             
       (intern (symbol-name ',name)
               (symbol-value (find-symbol +routes-symbol+)))
       (export ',name)
       
       (reconnect-all-routes))))

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
                                             (if *submodule*
                                                 (submodule-full-baseurl *submodule*)
                                                 "")
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
                                       (if *submodule* (submodule-full-baseurl *submodule*) "")
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
  
