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
    (initialize-module-instance (find-package module)
                                context)))

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
  ((symbol :initarg :symbol :initform nil :reader submodule-symbol)
   (module :initarg :module :initform nil :reader submodule-module)
   (context :initarg :context :initform (make-context))
   (parent :initarg :parent :initform nil :reader submodule-parent)
   (submodules :initform nil :accessor submodule-submodules)))

(defmethod shared-initialize :after ((obj submodule) slot-names &rest initargs &key)
  (declare (ignore initargs))
  (setf (submodule-submodules obj)
        (iter (for (key sub) in-hashtable (symbol-value (find-symbol +submodules-symbol+
                                                                     (submodule-module obj))))
              (collect (make-instance 'submodule
                                      :symbol key
                                      :module  (submodule-module sub)
                                      :context (slot-value sub 'context)
                                      :parent obj)))))

(defun find-submodule (symbol &optional (parent *submodule*))
  (find symbol
        (submodule-submodules parent)
        :key #'submodule-symbol))

(defmacro with-submodule-context (submodule &body body)
  `(with-context (slot-value ,submodule 'context)
     ,@body))

(defun submodule-baseurl (submodule)
  (with-submodule-context submodule
    (symbol-value (find-symbol +baseurl-symbol+
                               (submodule-module submodule)))))

(defun submodule-full-baseurl (submodule)
  (let ((prefix (submodule-baseurl submodule))
        (parent (submodule-parent submodule)))
    (if parent
        (concatenate 'list
                     (submodule-full-baseurl parent)
                     prefix)
        prefix)))

(defun submodule-toplevel (submodule)
  (let ((parent (submodule-parent submodule)))
    (if parent
        (submodule-toplevel parent)
        submodule)))

(defun submodule-routes (submodule)
  (let ((module (submodule-module submodule)))
    (with-submodule-context submodule
      (alexandria:flatten (list* (iter (for route-symbol in-package (symbol-value (find-symbol +routes-symbol+ module)))
                                       (collect (create-route-from-symbol (find-symbol (symbol-name route-symbol)
                                                                                       module)
                                                                          submodule )))
                                 (iter (for ss in (submodule-submodules submodule))
                                       (collect (submodule-routes ss))))))))

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
           (defparam +render-method-symbol+ ',(second (assoc :default-render-method options)))
           (defparam +content-type-symbol+ ',(second (assoc :default-content-type options )))
           *package*)))))

(defmacro define-submodule (name (module) &body bindings)
  (let ((submodules (find-symbol +submodules-symbol+))
        (submodule (gensym))
        (context (gensym)))
    `(progn
       (let ((,submodule (gethash ',name ,submodules)))
         (when ,submodule
           (finalize-module-instance ',module
                                     (slot-value ,submodule 'context))))
       (let ((,context (make-context ,@bindings)))
         (setf (gethash ',name ,submodules)
               (make-instance 'submodule
                              :module ',module
                              :context ,context))
         (initialize-module-instance ',module ,context))
       (eval-when (:execute)
         (reconnect-all-routes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Macros for simplify develop modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-initialization ((context) &body body)
  `(defmethod initialize-module-instance ((module (eql ,*package*)) ,context)
     ,@body))

(defmacro define-finalization ((context) &body body)
  `(defmethod finalize-module-instance ((module (eql ,*package*)) ,context)
     ,@body))

(defmacro define-default-render-method ((data) &body body)
  `(setf ,(find-symbol +render-method-symbol+ *package*)
         #'(lambda (,data)
             ,@body)))