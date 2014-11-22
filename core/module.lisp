;;;; module.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas)

(defgeneric module-symbol (module)
  (:documentation "MODULE identifier"))

(defgeneric module-context (module)
  (:documentation "Context of the MODULE"))

(defgeneric module-parent (module)
  (:documentation "Parent of the MODULE"))

(defgeneric module-mount-url (module)
  (:documentation "Return URL where the MODULE is mounted"))

(defgeneric module-render-method (module)
  (:documentation "Return MODULE render method"))

(defgeneric initialize-module-instance (module context)
  (:documentation "Call for module initialization")
  (:method (module context)))

(defgeneric finalize-module-instance (module context)
  (:documentation "Call for module finalization")
  (:method (module context)))

(defgeneric connect-module (module mapper)
  (:documentation "Adds routes of the module to the routing table"))

(defgeneric module-find-route (module route-symbol)
  (:documentation "Find the route in the MODULE"))

(defgeneric module-find-child-module (module module-symbol)
  (:documentation "Find the mounted module in the MODULE"))

(defmacro with-module-context (module &body body)
  `(with-context (module-context ,module)
     ,@body))

(defmacro with-module (module &body body)
  (alexandria:once-only (module)
    `(if (eq *module* ,module)
         (progn ,@body)
         (let ((*module* ,module))
           (with-module-context *module*
             ,@body)))))

(defun module-real-url (module)
  (let ((prefix (module-mount-url module))
        (parent (module-parent module)))
    (if parent
        (append (module-real-url parent) prefix)
        prefix)))

(defun find-route (route-symbol &optional (module *module*))
  (module-find-route module route-symbol))

(defun find-mounted-module (module-symbol &optional (module *module*))
  (module-find-child-module module module-symbol))

(defun apply-decorators (route decorators)
  (if decorators
      (funcall (car decorators) (apply-decorators route (cdr decorators)))
      ;; (apply-decorators (funcall (car decorators) route)
      ;;                   (cdr decorators))
      route))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *pkgmodules-traits*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *pkgmodules-traits* (make-hash-table))

(defun find-pkgmodule-traits (package)
  (gethash (find-package package) *pkgmodules-traits*))

(defun register-pkgmodule-traits (package &rest traits &key &allow-other-keys)
  (let ((dict (alexandria:plist-hash-table traits))
        (old (find-pkgmodule-traits package)))
    (cond
      (old
       (iter (for key in '(:modules :routes :references))
             (setf (gethash key dict)
                   (gethash key old))))
      (t
       (setf (gethash :modules dict) (make-hash-table)
             (gethash :routes dict) (make-hash-table)
             (gethash :references dict) nil)))
    (setf (gethash (find-package package) *pkgmodules-traits*)
          dict)))

(defun distribute-route (route-symbol)
  (let ((package (symbol-package route-symbol)))
    (iter (for pkg in (pkgmodule-traits-references package))
          (iter (for (key thing) in-hashtable (pkgmodule-traits-modules pkg))
                (distribute-route (alexandria:format-symbol pkg "~A.~A" key
                                                            route-symbol))))))

(defun distribute-all-routes (package)
  (iter (for (key value) in-hashtable (pkgmodule-traits-routes package))
        (distribute-route key))
  (iter (for (key value) in-hashtable (pkgmodule-traits-modules package))
        (distribute-all-routes (find-package (car value)))))

(defun pkgmodule-traits-routes (package)
  (gethash :routes (find-pkgmodule-traits package)))

(defun pkgmodule-traits-modules (package)
  (gethash :modules (find-pkgmodule-traits package)))

(defun pkgmodule-traits-references (package)
  (gethash :references (find-pkgmodule-traits package)))

(defun pkgmodule-traits-content-type (package)
  (gethash :content-type (find-pkgmodule-traits package)))

(defun pkgmodule-traits-append-reference (package reference)
  (alexandria:ensure-gethash :references (find-pkgmodule-traits package))
  (pushnew reference (gethash :references (find-pkgmodule-traits package))))

(defun register-route-traits (route-symbol traits)
  (let* ((package (symbol-package route-symbol))
         (export (gethash :export-route-symbols (find-pkgmodule-traits package))))
    (setf (gethash route-symbol (pkgmodule-traits-routes package))
          traits)
    (when export
      (export route-symbol package))
    (distribute-route route-symbol)))

(defmethod module-context ((module symbol))
  "Get the context for a mounted module.

MODULE should be the name used for mount-module."
  (second (gethash module (pkgmodule-traits-modules *package*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pkgmodules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pkgmodule ()
  ((symbol :initarg :symbol :initform nil :reader module-symbol)
   (context :initarg :context :initform (make-context) :reader module-context)
   (parent :initarg :parent :initform nil :reader module-parent)
   (mount-url :initarg :url :initform nil :reader module-mount-url)
   (render-method :initarg :render-method :initform nil :reader
                  module-render-method)
   (decorators :initarg :decorators :initform nil)
   (package :initarg :package)
   (children :initform (make-hash-table))
   (routes :initform (make-hash-table))))

(defun module-decorators (pkgmodule)
  (with-slots (parent decorators) pkgmodule
    (if parent
        (append (module-decorators parent) decorators)
        decorators)))

(defmethod shared-initialize :after ((module pkgmodule) slot-names &key)
  (when (alexandria:emptyp (alexandria:lastcar (module-mount-url module)))
    (setf (slot-value module 'mount-url)
          (butlast (module-mount-url module))))
  (with-slots (package children routes render-method) module
    (clrhash children)
    (clrhash routes)

    (let ((traits (find-pkgmodule-traits package)))

      (unless render-method
        (let ((fun (gethash :render-method traits)))
          (setf render-method
                (if fun (funcall fun)))))

      (iter (for (key thing) in-hashtable (gethash :modules traits))
            (destructuring-bind (pkg ctxt child-traits) thing
              (setf (gethash key children)
                    (make-instance 'pkgmodule
                                   :symbol key
                                   :context
                                   (let ((context (copy-restas-context ctxt)))
                                     (when (gethash :inherit-parent-context
                                                    child-traits)
                                       (setf (context-prototype context)
                                             (module-context module)))
                                     context)
                                   :parent module
                                   :package pkg
                                   :url (routes:parse-template
                                         (gethash :url child-traits ""))
                                   :render-method (gethash :render-method
                                                           child-traits)
                                   :decorators (gethash :decorators
                                                        child-traits)))))

      (iter (for rsymbol in (alexandria:hash-table-keys (gethash :routes traits)))
            (setf (gethash (find-symbol (string rsymbol) package)  routes)
                  (create-route-from-symbol (find-symbol (symbol-name rsymbol)
                                                         package)
                                            module)))

      (iter (for (child-key child) in-hashtable (slot-value module 'children))
            (for child-routes = (slot-value child 'routes))
            (iter (for (route-key route) in-hashtable (slot-value child 'routes))
                  (for key = (alexandria:format-symbol package "~A.~A"
                                                       child-key route-key))
                  (setf (gethash key routes)
                        route))))))

(defmethod connect-module ((module pkgmodule) mapper)
  (iter (for route in (alexandria:hash-table-values (slot-value module 'routes)))
        (routes:connect mapper route)))

(defmethod initialize-module-instance ((module pkgmodule) context)
  (with-module module
    (initialize-module-instance (find-package (slot-value module 'package))
                                context)
    (iter (for child in (alexandria:hash-table-values (slot-value module
                                                                  'children)))
          (initialize-module-instance child (module-context child)))))

(defmethod finalize-module-instance ((module pkgmodule) context)
  (iter (for child in (alexandria:hash-table-values (slot-value module
                                                                'children)))
        (finalize-module-instance child  (module-context child)))
  (finalize-module-instance (find-package (slot-value module 'package)) context))

(defmethod module-find-route ((module pkgmodule) route-symbol)
  (with-slots (routes parent) module
    (or (gethash route-symbol routes)
        (and parent (module-find-route parent route-symbol)))))

(defmethod module-find-child-module ((module pkgmodule) module-symbol)
  (gethash module-symbol (slot-value module 'children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-module (name &body options)
  (let (defpackage-options traits)
    (iter (for option in options)
          (case (car option)
            (:render-method
             (collect :render-method into pkgtraits)
             (collect `(alexandria:named-lambda make-render-method ()
                         ,(second option)) into pkgtraits))
            ((:export-route-symbols :content-type)
             (collect (first option) into pkgtraits)
             (collect (second option) into pkgtraits))
            (otherwise
             (collect option into pkgoptions)))
          (finally
           (setf defpackage-options pkgoptions
                 traits pkgtraits)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defpackage ,name ,@defpackage-options)
       (register-pkgmodule-traits ',name ,@traits)
       (reconnect-all-routes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mount-module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mount-module (name (module) &body body)
  (multiple-value-bind (declarations context) (split-code-declarations body)
    (let ((bindings (iter (for (symbol value) in context)
                          (collect `(cons ',symbol ,value))))
          (traits (parse-all-declarations declarations
                                          '(:decorators :url :render-method
                                            :inherit-parent-context))))
    `(progn
       (setf (gethash ',name (pkgmodule-traits-modules *package*))
             (list ',module
                   (make-context (list ,@bindings))
                   (alexandria:plist-hash-table
                    (list ,@(alexandria:hash-table-plist traits)))))
       (pkgmodule-traits-append-reference (find-package ',module) *package*)
       (distribute-all-routes (find-package ',module))
       (reconnect-all-routes)))))
