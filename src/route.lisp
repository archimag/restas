;;; route.lisp

(in-package :restas)

(defgeneric process-route (route bindings))

(defun parse-template (tmpl)
  (iter (for path in (routes::split-template (ppcre:regex-replace-all "//+"
                                                                      (string-left-trim "/" tmpl)
                                                                      "/")))
        (collect (let ((spec (routes::parse-path path)))
                   (if (cdr spec)
                       (unify:make-unify-template 'unify::concat spec)
                       (car spec))))))

(defun plugin-update ()
  (reconnect-all-plugins))

(defun route-changed (route)
  (declare (ignore route))
  (plugin-update))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; routes

(defun parse-template/package (tmpl)
  (concatenate 'list
               (symbol-value (find-symbol "*BASEURL*" *package*))
               (parse-template tmpl)))
  
(defun routes/package ()
  (symbol-value (find-symbol "*ROUTES*" *package*)))

(defclass base-route (routes:route)
  ((master :initarg :overlay-master :initform nil :reader route-master)
   (content-type :initarg :content-type :initform nil :reader route-content-type)
   (user-login :initarg :user-login :initform nil :reader route-user-login)
   (required-login-status :initarg :required-login-status :initform nil :reader route-required-login-status)
   (required-method :initarg :required-method :initform nil :reader route-required-method)))

(defgeneric process-route/impl (route bindings))

(defmethod routes:route-extend-bindings ((route base-route) bindings)
  (with-slots (user-login) route
    (let ((login (if user-login (funcall user-login))))
      (if login
          (acons :user-login-name login bindings)
          bindings))))

(defmethod routes:route-check-conditions ((route base-route) bindings)
  (with-slots (required-method required-login-status) route
    (and (if required-method
             (eql (cdr (assoc :method bindings)) required-method)
             t)
         (case required-login-status
           (:logged-on (assoc :user-login-name bindings))
           (:not-logged-on (not (assoc :user-login-name bindings)))
           ((nil) t)
           (otherwise (error "unknow required login status: ~A" required-login-status))))))

(defmethod restas::process-route ((route base-route) bindings)
  (let* ((master (route-master route))
         (res (if master
                  (restas::apply-overlay master (process-route/impl route bindings) bindings)
                  (process-route/impl route bindings))))
    (unless (pathnamep res)
      (setf (hunchentoot:content-type*)
            (or (route-content-type route)
                "application/xhtml+xml")))
    (typecase res
      (xtree::libxml2-cffi-object-wrapper (let ((tmp (xtree:serialize res :to-string :pretty-print t :encoding :utf-8))) tmp))
      (pathname (hunchentoot:handle-static-file res))
      (otherwise res))))
                                          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass filesystem-route (base-route)
  ((path :initarg :path)))

(defmethod process-route/impl ((route filesystem-route) bindings)
  (pathname (restas::expand-text (slot-value route 'path)
                                 bindings)))

(defmacro define-filesystem-route (name template path &key overlay-master content-type login-status (method :get))
  `(progn
     (setf (get ',name :initialize)
           #'(lambda () (make-instance 'filesystem-route
                                       :template (parse-template/package ,template)
                                       :path (namestring (merge-pathnames ,path ,(symbol-value (find-symbol "*BASEPATH*" *package*))))
                                       :overlay-master ,overlay-master
                                       :content-type ,content-type
                                       :user-login (find-symbol "COMPUTE-USER-LOGIN-NAME" ,*package*)
                                       :required-login-status ',login-status
                                       :required-method ,method)))
     (intern (symbol-name ',name) (routes/package))
     (eval-when (:execute)
       (route-changed ',name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass fs-xsl-route (filesystem-route)
  ((xsl :initarg :xsl)
   (xpath-functions :initarg :xpath-functions :initform nil)
   (xslt-elements :initarg :xslt-elements :initform nil)))

(defmethod process-route/impl ((route fs-xsl-route) bindings)
  (let ((xpath:*lisp-xpath-functions* (symbol-value (slot-value route 'xpath-functions)))
        (xslt:*lisp-xslt-elements* (symbol-value (slot-value route 'xslt-elements))))
    (gp:object-register (xslt:transform (slot-value route 'xsl)
                                        (call-next-method))
                        *request-pool*)))
  
(defmacro define-fs-xsl-route (name template path xsl  &key overlay-master content-type login-status (method :get))
  `(progn
     (setf (get ',name :initialize)
           #'(lambda () (make-instance 'fs-xsl-route
                                       :template (parse-template/package ,template)
                                       :path (namestring (merge-pathnames ,path ,(symbol-value (find-symbol "*BASEPATH*" *package*))))
                                       :xsl ,xsl
                                       :xpath-functions (find-symbol "*XPATH-FUNCTIONS*" ,*package*)
                                       :xslt-elements (find-symbol "*XSLT-ELEMENTS*" ,*package*)
                                       :overlay-master ,overlay-master
                                       :content-type ,content-type
                                       :user-login (find-symbol "COMPUTE-USER-LOGIN-NAME" ,*package*)
                                       :required-login-status ,login-status
                                       :required-method ,method)))
     (intern (symbol-name ',name) (routes/package))
     (eval-when (:execute)
       (route-changed ',name))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass simple-route (base-route)
  ((symbol :initarg :symbol)))

(defmethod process-route/impl ((route simple-route) bindings)
  (funcall (get (slot-value route 'symbol)
                :handler)))

(defmacro define-simple-route (name (template &key (protocol :http) overlay-master content-type login-status (method :get)) &body body)
  (let* ((parsed-template (parse-template/package (if (stringp template)
                                              template
                                              (eval template))))
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
       (setf (get ',name :initialize)
             #'(lambda () (make-instance 'simple-route
                                         :template (parse-template/package ,template)
                                         :symbol ',name
                                         :overlay-master ,overlay-master
                                         :content-type ,content-type
                                         :user-login (find-symbol "COMPUTE-USER-LOGIN-NAME" ,*package*)
                                         :required-login-status ,login-status
                                         :required-method ,method)))
       (setf (get ',name :protocol)
             ,protocol)
       (intern (symbol-name ',name) (routes/package))
       (eval-when (:execute)
         (route-changed ',name)))))
