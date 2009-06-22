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

;; (defclass proxy-route (routes:route)
;;   ((origin :initarg :origin :reader proxy-route-origin)
;;    (baseurl :initarg :baseurl :initform nil)))

;; (defmethod initialize-instance ((proxy proxy-route) &key)
;;   (call-next-method)
;;   (setf (slot-value proxy 'routes::template)
;;         (concatenate 'list
;;                      (slot-value proxy 'baseurl)
;;                      (slot-value (proxy-route-origin proxy)
;;                                  'routes::template))))

;; (defmethod process-route ((proxy proxy-route) bindings)
;;   (process-route (proxy-route-origin proxy) bindings))

(defun plugin-update ()
  (reconnect-all-plugins))

(defun route-changed (route)
  (declare (ignore route))
  (plugin-update))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; routes

;;(defvar *base-path*)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defun parse-template/package (tmpl)
;;     (concatenate 'list
;;                  *base-path*
;;                  (parse-template tmpl))))

(defun parse-template/package (tmpl)
  (concatenate 'list
               (symbol-value (find-symbol "*BASEURL*" *package*))
               (parse-template tmpl)))
  
(defun routes/package ()
  (symbol-value (find-symbol "*ROUTES*" *package*)))

(defclass base-route (routes:route)
  ((master :initarg :overlay-master :initform nil :reader route-master)
   (content-type :initarg :content-type :initform nil :reader route-content-type)))

(defmethod initialize-instance ((route base-route) &key (method :get) &allow-other-keys)
  (call-next-method)
  (if method
      (setf (slot-value route 'routes::extra-bindings)
            (acons :method
                   :get
                   (slot-value route 'routes::extra-bindings)))))

(defgeneric process-route/impl (route bindings))

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
                                  
(defmacro define-filesystem-route (name template path &key overlay-master content-type)
  `(progn
     (setf (get ',name :initialize)
           #'(lambda () (make-instance 'filesystem-route
                                       :template (parse-template/package ,template)
                                       :path (namestring (merge-pathnames ,path ,(symbol-value (find-symbol "*BASEPATH*" *package*))))
                                       :overlay-master ,overlay-master
                                       :content-type ,content-type)))
     (intern (symbol-name ',name) (routes/package))
     (eval-when (:execute)
       (reconnect-all-plugins))))
;;       (route-changed ',name))))

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
  
(defmacro define-fs-xsl-route (name template path xsl  &key overlay-master content-type)
  `(progn
     (setf (get ',name :initialize)
           #'(lambda () (make-instance 'fs-xsl-route
                                       :template (parse-template/package ,template)
                                       :path (namestring (merge-pathnames ,path ,(symbol-value (find-symbol "*BASEPATH*" *package*))))
                                       :xsl ,xsl
                                       :xpath-functions (find-symbol "*XPATH-FUNCTIONS*" ,*package*)
                                       :xslt-elements (find-symbol "*XSLT-ELEMENTS*" ,*package*)
                                       :overlay-master ,overlay-master
                                       :content-type ,content-type)))
     (intern (symbol-name ',name) (routes/package))
     (eval-when (:execute)
       (reconnect-all-plugins))))
       ;;(route-changed ',name))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass simple-route (routes:route)
  ((symbol :initarg :symbol)))

(defmethod restas::process-route ((route simple-route) bindings)
  (funcall (get (slot-value route 'symbol)
                :handler)
           bindings))

(defmacro define-simple-route (name (template &key (protocol :http)) &body body)
  (let* ((parsed-template (parse-template/package (if (stringp template)
                                              template
                                              (eval template))))
         (variables (iter (for var in (routes.unify:template-variables parsed-template))
                          (collect (list (intern (symbol-name var) (routes/package))
                                         (list 'cdr (list 'assoc var (intern "BINDINGS" (routes/package))))))))
         (handler-body (if variables
                           `((let (,@variables) ,@body))
                           `(,@body))))
    `(progn
       (setf (get ',name :handler)
             #'(lambda (,(intern "BINDINGS" *package*)) ,@handler-body))
       (setf (get ',name :initialize)
             #'(lambda () (make-instance 'simple-route
                                         :template (parse-template/package ,template)
                                         :symbol ',name)))
       (setf (get ',name :protocol)
             ,protocol)
       (intern (symbol-name ',name) (routes/package))
       (eval-when (:execute)
         (route-changed ',name)))))
