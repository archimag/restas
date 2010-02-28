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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; submodule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass submodule ()
  ((module :initarg :module :initform nil)
   (context :initarg :context :initform (make-preserve-context))
   (parent :initarg :parent :initform nil)))

(defmacro with-submodule-context (submodule &body body)
  `(with-context (slot-value ,submodule 'context)
     ,@body))
    
(defgeneric module-routes (module)
  (:documentation "List routes of the module")
  (:method ((module symbol))
    (module-routes (find-package module))))

(defun submodule-routes (submodule)
  (with-submodule-context submodule
    (iter (for route in (module-routes (slot-value submodule 'module)))
          (unless (slot-value route 'submodule)
            (setf (slot-value route 'submodule)
                  submodule))
          (collect route))))


(defun connect-submodule (submodule mapper)
  (iter (for route in (submodule-routes submodule))
          (routes:connect mapper route)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; package as module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +routes-symbol+ "*ROUTES*")
(defparameter +baseurl-symbol+ "*BASEURL*")
(defparameter +submodules-symbol+ "*SUBMODULES*")

(defmacro define-module (name &rest options)
  (let* ((use (cdr (assoc :use options)))
         (export (cdr (assoc :export options)))
         (impl-package-name (format nil "~:@(~A.IMPL.ROUTES~)" name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((*package* (defpackage ,name
                          (:use ,@use)
                          (:export #:*baseurl* ,@export)
                          (:import-from #:restas #:*request-pool* #:*bindings* #:genurl #:define-route))))
         (flet ((defparam (name &optional value)
                  (eval `(defparameter ,(intern name) ,value))))
           (defparam +routes-symbol+ (defpackage ,impl-package-name (:use)))
           (defparam +baseurl-symbol+)
           (defparam +submodules-symbol+ (make-hash-table))
           *package*)))))

(defmacro define-initialization ((context) &body body)
  `(defmethod initialize-module-instance ((module (eql ,*package*)) ,context)
     ,@body))

(defmacro define-finalization ((context) &body body)
  `(defmethod finalize-module-instance ((module (eql ,*package*)) ,context)
     ,@body))

(defmethod module-routes ((module package))
  (alexandria:flatten (list* (iter (for route-symbol in-package (symbol-value (find-symbol +routes-symbol+ module)))
                                   (collect (funcall (get (find-symbol (symbol-name route-symbol)
                                                                       module)
                                                          :initialize))))
                             (let ((submodules (symbol-value (find-symbol +submodules-symbol+ module))))
                               (if submodules
                                   (iter (for (key submodule) in-hashtable submodules)
                                         (collect (submodule-routes submodule))))))))

(defmacro define-submodule (name (module) &body bindings)
  (let ((submodules (find-symbol +submodules-symbol+)))
    `(progn
       (let ((submodule (gethash ',name ,submodules)))
         (when submodule
           (finalize-module-instance ',module
                                     (slot-value submodule 'context))))
       (let ((context (make-preserve-context ,@bindings)))
         (setf (gethash ',name ,submodules)
               (make-instance 'submodule
                              :module ',module
                              :context context))
         (initialize-module-instance ',module context))
       (eval-when (:execute)
         (reconnect-all-sites)))))