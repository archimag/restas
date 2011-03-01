;;;; pkg-module.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Use packages as modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +routes-symbol+ "*ROUTES*")
(defparameter +baseurl-symbol+ "*BASEURL*")
(defparameter +submodules-symbol+ "*SUBMODULES*")
(defparameter +render-method-symbol+ "*DEFAULT-RENDER-METHOD*")
(defparameter +content-type-symbol+ "*DEFAULT-CONTENT-TYPE*")
(defparameter +headers-symbol+ "*DEFAULT-HEADERS*")
(defparameter +decorators-sybmol+ "*DECORATORS*")

(defclass pkg-submodule (submodule)
  ((submodules :accessor submodule-submodules)))

(defmethod shared-initialize :after ((obj pkg-submodule) slot-names &key)
  (setf (submodule-submodules obj)
        (iter (for (key thing) in-hashtable (symbol-value (find-symbol +submodules-symbol+ (submodule-module obj))))
              (destructuring-bind (pkg ctxt decorators) thing
                (collect (make-instance 'pkg-submodule
                                        :symbol key
                                        :module (find-package pkg)
                                        :context (copy-restas-context ctxt)
                                        :parent obj
                                        :decorators decorators))))))

(defmethod initialize-module-instance ((module package) context)
  (iter (for child in (submodule-submodules *submodule*))
        (let ((*submodule* child))
          (initialize-module-instance (submodule-module child)
                                      (submodule-context child)))))

(defmethod finalize-module-instance ((module package) context)
  (iter (for child in (submodule-submodules *submodule*))
        (let ((*submodule* child))
          (finalize-module-instance (submodule-module child)
                                    (submodule-context child)))))

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

(defmethod submodule-baseurl ((submodule pkg-submodule))
  (with-submodule-context submodule
    (symbol-value (find-symbol +baseurl-symbol+
                               (submodule-module submodule)))))

(defmethod find-child-submodule ((symbol symbol) (parent pkg-submodule))
  (find symbol
        (submodule-submodules parent)
        :key #'submodule-symbol))

(defun find-upper-submodule (module &optional (current-submodule *submodule*)
                             &aux (package (find-package module)))
  (unless current-submodule
    (error "Can not find a submodule: ~A" package))
  (if (eql (submodule-module current-submodule) package)
      current-submodule
      (find-upper-submodule module (submodule-parent current-submodule))))

(defmethod module-routes ((module package) submodule
                          &aux (decorators (symbol-value (find-symbol +decorators-sybmol+ module))))
  (with-submodule-context submodule
    (mapcar
     (lambda (route)
       (apply-decorators route decorators))
     (alexandria:flatten (list* (iter (for route-symbol in-package (symbol-value (find-symbol +routes-symbol+ module)))
                                      (collect (create-route-from-symbol (find-symbol (symbol-name route-symbol)
                                                                                      module)
                                                                         submodule )))
                                (iter (for ss in (submodule-submodules submodule))
                                      (collect (submodule-routes ss))))))))

(defmethod make-submodule ((module symbol) &key (context (make-context)))
  (make-submodule (or (find-package module)
                      (error "Package ~A not found" module))
                  :context context))

(defmethod make-submodule ((package package) &key (context (make-context)))
  (make-instance 'pkg-submodule
                 :module package
                 :context context))


(defmacro define-module (name &rest options)
  (let* ((export (cdr (assoc :export options)))
         (impl-package-name (format nil "~:@(~A.IMPL.ROUTES~)" name))
         (defpackage-options (remove-if #'(lambda (opt)
                                            (member (car opt)
                                                    '(:export :default-render-method :default-content-type :decorators)))
                                        options)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((*package* (defpackage ,name
                          ,@defpackage-options
                          (:export #:*baseurl* #:*default-render-method* #:*default-headers* ,@export))))
         (flet ((defparam (name &optional value &aux (s (intern name)))
                  (proclaim `(special ,s))
                  (setf (symbol-value s) value)))
           (defparam +routes-symbol+ (defpackage ,impl-package-name (:use)))
           (defparam +baseurl-symbol+)
           (defparam +submodules-symbol+ (make-hash-table))
           (defparam +render-method-symbol+ ,(second (assoc :default-render-method options)))
           (defparam +content-type-symbol+ ,(second (assoc :default-content-type options)))
           (defparam +headers-symbol+ ',(second (assoc :default-headers options)))
           (defparam +decorators-sybmol+ (list ,@(cdr (assoc :decorators options))))
           (eval-when (:execute)
             (reconnect-all-routes))
           *package*)))))

(defmacro mount-submodule (name (module &rest decorators) &body bindings)
  (let ((submodules (find-symbol +submodules-symbol+)))
    `(progn
       (setf (gethash ',name ,submodules)
             (list ',module
                   (make-context ,@bindings)
                   ',decorators))
       (eval-when (:execute)
         (reconnect-all-routes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Macros for simplify develop modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-default-render-method ((data) &body body)
  `(setf ,(find-symbol +render-method-symbol+ *package*)
         #'(lambda (,data)
             ,@body)))