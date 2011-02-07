;;;; module.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas)

(defgeneric make-submodule (module &key context)
  (:documentation "Make submodule from module"))

(defgeneric submodule-routes (submodule)
  (:documentation "List routes for the submodule"))


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
(defparameter +headers-symbol+ "*DEFAULT-HEADERS*")

(defun string-symbol-value (string &optional (package *package*))
  (symbol-value (find-symbol string package)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; submodule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass submodule ()
  ((symbol :initarg :symbol :initform nil :reader submodule-symbol)
   (module :initarg :module :initform nil :reader submodule-module)
   (context :initarg :context :initform (make-context) :reader submodule-context)
   (parent :initarg :parent :initform nil :reader submodule-parent)
   (submodules :initform nil :accessor submodule-submodules)))

(defmethod reinitialize-instance :before ((obj submodule) &rest initargs &key)
  (declare (ignore initargs))
  (let ((*submodule* obj))
    (finalize-module-instance (submodule-module obj)
                              (slot-value obj 'context)))
  (iter (for thing in (submodule-submodules obj))
        (let ((*submodule* thing))
          (finalize-module-instance (submodule-module obj)
                                    (slot-value obj 'context)))))

(defmethod shared-initialize :after ((obj submodule) slot-names &rest initargs &key)
  (declare (ignore initargs))
  (setf (submodule-submodules obj)
        (iter (for (key thing) in-hashtable (symbol-value (find-symbol +submodules-symbol+
                                                                       (submodule-module obj))))
              (collect (make-instance 'submodule
                                      :symbol key
                                      :module (find-package (car thing))
                                      :context (copy-restas-context (cdr thing))
                                      :parent obj))))
  (let ((*submodule* obj))
    (initialize-module-instance (submodule-module obj)
                                (slot-value obj 'context))))

(defun find-submodule (symbol &optional (parent *submodule*))
  (find symbol
        (submodule-submodules parent)
        :key #'submodule-symbol))

(defmacro with-submodule-context (submodule &body body)
  `(with-context (slot-value ,submodule 'context)
     ,@body))

(defmacro with-submodule (submodule &body body)
  (alexandria:once-only (submodule)
    `(if (eq *submodule* ,submodule)
         (progn ,@body)
         (let ((*submodule* ,submodule))
           (with-submodule-context *submodule*
             ,@body)))))

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

(defun find-upper-submodule (module &optional (current-submodule *submodule*)
                             &aux (package (find-package module)))
  (unless current-submodule
    (error "Can not find a submodule: ~A" package))
  (if (eql (submodule-module current-submodule) package)
      current-submodule
      (find-upper-submodule module (submodule-parent current-submodule))))

(defun submodule-toplevel (submodule)
  (let ((parent (submodule-parent submodule)))
    (if parent
        (submodule-toplevel parent)
        submodule)))

(defmethod submodule-routes ((submodule submodule))
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


(defmethod make-submodule ((module symbol) &key (context (make-context)))
  (make-submodule (or (find-package module)
                      (error "Package ~A not found" module))
                  :context context))

(defmethod make-submodule ((package package) &key (context (make-context)))
  (make-instance 'submodule
                 :module package
                 :context context))

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
                          (:export #:*baseurl* #:*default-render-method* #:*default-headers* ,@export))))
         (flet ((defparam (name &optional value)
                  (eval `(defparameter ,(intern name) ,value))))
           (defparam +routes-symbol+ (defpackage ,impl-package-name (:use)))
           (defparam +baseurl-symbol+)
           (defparam +submodules-symbol+ (make-hash-table))
           (defparam +render-method-symbol+ ',(second (assoc :default-render-method options)))
           (defparam +content-type-symbol+ ',(second (assoc :default-content-type options)))
           (defparam +headers-symbol+ ',(second (assoc :default-headers options)))
           *package*)))))

(defmacro mount-submodule (name (module) &body bindings)
  (let ((submodules (find-symbol +submodules-symbol+)))
    `(progn
       (setf (gethash ',name ,submodules)
             (cons ',module
                   (make-context ,@bindings)))
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
