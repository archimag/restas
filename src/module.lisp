;;;; module.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas)

(defgeneric initialize-module-instance (module context)
  (:documentation "Call for module initialization")
  (:method (module context)
    (declare (ignore module context)))
  (:method ((module symbol) context)
    (unless (keywordp module)
      (initialize-module-instance (find-package module)
                                  context))))

(defgeneric finalize-module-instance (module context)
  (:documentation "Call for module finalization")
  (:method (module context)
    (declare (ignore module context)))
  (:method ((module symbol) context)
    (unless (keywordp module)
      (finalize-module-instance (find-package module)
                                context))))

(defparameter +routes-symbol+ "*ROUTES*")
(defparameter +baseurl-symbol+ "*BASEURL*")
(defparameter +submodules-symbol+ "*SUBMODULES*")
(defparameter +render-method-symbol+ "*DEFAULT-RENDER-METHOD*")
(defparameter +content-type-symbol+ "*DEFAULT-CONTENT-TYPE*")

(defun string-symbol-value (string &optional (package *package*))
  (symbol-value (find-symbol string package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; submodule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass submodule ()
  ((module :initarg :module :initform nil)
   (context :initarg :context :initform (make-context))
   (parent :initarg :parent :initform nil)))

(defmacro with-submodule-context (submodule &body body)
  `(with-context (slot-value ,submodule 'context)
     ,@body))

(defun submodule-baseurl (submodule)
  (with-submodule-context submodule
    (symbol-value (find-symbol +baseurl-symbol+
                               (slot-value submodule 'module)))))

(defun submodule-full-baseurl (submodule)
  (let ((prefix (submodule-baseurl submodule))
        (parent (slot-value submodule 'parent)))
    (if parent
        (concatenate 'list
                     (submodule-full-baseurl parent)
                     prefix)
        prefix)))

(defun submodule-toplevel (submodule)
  (let ((parent (slot-value submodule 'parent)))
    (if parent
        (submodule-toplevel parent)
        submodule)))
    
(defgeneric module-routes (module submodule)
  (:documentation "List routes of the module")
  (:method ((module symbol) submodule)
    (module-routes (find-package module)
                   submodule)))

(defun submodule-routes (submodule)
  (with-submodule-context submodule
    (iter (for route in (module-routes (slot-value submodule 'module)
                                       submodule))
          (collect route))))

(defun connect-submodule (submodule mapper)
  (iter (for route in (submodule-routes submodule))
          (routes:connect mapper route)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; package as module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-module (name &rest options)
  (let* ((export (cdr (assoc :export options)))
         (impl-package-name (format nil "~:@(~A.IMPL.ROUTES~)" name))
         (defpackage-options (remove-if #'(lambda (opt)
                                            (member (car opt)
                                                    (list :export 
                                                          :default-render-method 
                                                          :default-content-type)))
                                        options)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((*package* (defpackage ,name
                          ,@defpackage-options
                          (:export #:*baseurl* #:*default-render-method* ,@export)
                          (:import-from #:restas #:*request-pool* #:*bindings* #:genurl #:define-route))))
         (flet ((defparam (name &optional value)
                  (eval `(defparameter ,(intern name) ,value))))
           (defparam +routes-symbol+ (defpackage ,impl-package-name (:use)))
           (defparam +baseurl-symbol+)
           (defparam +submodules-symbol+ (make-hash-table))
           (defparam +render-method-symbol+ ,(second (assoc :default-render-method options)))
           (defparam +content-type-symbol+ ,(second (assoc :default-content-type options )))
           *package*)))))

(defmacro define-initialization ((context) &body body)
  `(defmethod initialize-module-instance ((module (eql ,*package*)) ,context)
     ,@body))

(defmacro define-finalization ((context) &body body)
  `(defmethod finalize-module-instance ((module (eql ,*package*)) ,context)
     ,@body))

(defmethod module-routes ((module package) submodule)
  (alexandria:flatten (list* (iter (for route-symbol in-package (symbol-value (find-symbol +routes-symbol+ module)))
                                   (collect (create-route-from-symbol (find-symbol (symbol-name route-symbol)
                                                                                   module)
                                                                      submodule )))
                             (iter (for (key sub) in-hashtable (symbol-value (find-symbol +submodules-symbol+ module)))
                                   (collect (submodule-routes (make-instance 'submodule
                                                                             :module (slot-value sub 'module)
                                                                             :context (slot-value sub 'context)
                                                                             :parent submodule)))))))

(defmacro define-submodule (name (module) &body bindings)
  (let ((submodules (find-symbol +submodules-symbol+)))
    `(progn
       (let ((submodule (gethash ',name ,submodules)))
         (when submodule
           (finalize-module-instance ',module
                                     (slot-value submodule 'context))))
       (let ((context (make-context ,@bindings)))
         (setf (gethash ',name ,submodules)
               (make-instance 'submodule
                              :module ',module
                              :context context))
         (initialize-module-instance ',module context))
       (eval-when (:execute)
         (reconnect-all-routes)))))