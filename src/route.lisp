;;; route.lisp

(in-package :restas)

(defgeneric process-route (route bindings))


(defun plugin-update ()
  ;;(reconnect-all-plugins))
  (reconnect-all-sites))


(defun route-changed (route)
  (declare (ignore route))
  (plugin-update))


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

(defmethod routes:route-extend-bindings ((route base-route) bindings)
  (let ((login (calculate-user-login (slot-value route 'plugin-instance)
                                     hunchentoot:*request*)))
    (if login
        (acons :user-login-name login bindings)
        bindings)))

(defmethod routes:route-check-conditions ((route base-route) bindings)
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
             t))))

(defmethod restas::process-route ((route base-route) bindings)
  (with-context (slot-value (slot-value route 'plugin-instance)
                            'context)
    (let ((res (process-route/impl route bindings)))
      (unless (pathnamep res)
        (setf (hunchentoot:content-type*)
              (or (route-content-type route)
                  "text/html")))
      (if (eql (route-protocol route) :http)
          (adopt-route-result (slot-value route 'plugin-instance)
                              res)
          res))))
                                          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defclass filesystem-route (base-route)
;;   ((path :initarg :path)))

;; (defmethod process-route/impl ((route filesystem-route) bindings)
;;   (pathname (restas::expand-text (slot-value route 'path)
;;                                  bindings)))

;; (defmacro define-filesystem-route (name template path &key overlay-master content-type login-status (method :get))
;;   `(progn
;;      (setf (get ',name :template)
;;            '(parse-template/package ,template))
;;      (setf (get ',name :initialize)
;;            #'(lambda () (make-instance 'filesystem-route
;;                                        :template (parse-template/package ,template)
;;                                        :path (namestring (merge-pathnames ,path ,(symbol-value (find-symbol "*BASEPATH*" *package*))))
;;                                        :overlay-master ,overlay-master
;;                                        :content-type (or ,content-type (symbol-value (find-symbol "*DEFAULT-CONTENT-TYPE*" ,*package*)))
;;                                        :user-login (find-symbol "COMPUTE-USER-LOGIN-NAME" ,*package*)
;;                                        :required-login-status ',login-status
;;                                        :required-method ,method)))
;;      (intern (symbol-name ',name) (routes/package))
;;      (eval-when (:execute)
;;        (route-changed ',name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defclass fs-xsl-route (filesystem-route)
;;   ((xsl :initarg :xsl)
;;    (xpath-functions :initarg :xpath-functions :initform nil)
;;    (xslt-elements :initarg :xslt-elements :initform nil)))

;; (defmethod process-route/impl ((route fs-xsl-route) bindings)
;;   (let ((xpath:*lisp-xpath-functions* (symbol-value (slot-value route 'xpath-functions)))
;;         (xslt:*lisp-xslt-elements* (symbol-value (slot-value route 'xslt-elements))))
;;     (gp:object-register (xslt:transform (slot-value route 'xsl)
;;                                         (call-next-method))
;;                         *request-pool*)))
  
;; (defmacro define-fs-xsl-route (name template path xsl  &key overlay-master content-type login-status (method :get))
;;   `(progn
;;      (setf (get ',name :template)
;;            '(parse-template/package ,template))
;;      (setf (get ',name :initialize)
;;            #'(lambda () (make-instance 'fs-xsl-route
;;                                        :template (parse-template/package ,template)
;;                                        :path (namestring (merge-pathnames ,path ,(symbol-value (find-symbol "*BASEPATH*" *package*))))
;;                                        :xsl ,xsl
;;                                        :xpath-functions (find-symbol "*XPATH-FUNCTIONS*" ,*package*)
;;                                        :xslt-elements (find-symbol "*XSLT-ELEMENTS*" ,*package*)
;;                                        :overlay-master ,overlay-master
;;                                        :content-type (or ,content-type (symbol-value (find-symbol "*DEFAULT-CONTENT-TYPE*" ,*package*)))
;;                                        :user-login (find-symbol "COMPUTE-USER-LOGIN-NAME" ,*package*)
;;                                        :required-login-status ,login-status
;;                                        :required-method ,method)))
;;      (intern (symbol-name ',name) (routes/package))
;;      (eval-when (:execute)
;;        (route-changed ',name))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass simple-route (base-route)
  ((symbol :initarg :symbol)))

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