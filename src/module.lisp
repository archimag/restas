;;;; module.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas)

(defgeneric make-submodule (module &key context)
  (:documentation "Make submodule from module"))

(defgeneric submodule-sybmol (submodule)
  (:documentation "Submodule identifier"))

(defgeneric submodule-module (submodule)
  (:documentation "module"))

(defgeneric submodule-context (submodule)
  (:documentation "Context of the submodule"))

(defgeneric submodule-parent (submodule)
  (:documentation "Parent submodule"))

(defgeneric submodule-baseurl (submodule)
  (:documentation "Retrun URL where the submodule is mounted"))

(defgeneric submodule-routes (submodule)
  (:documentation "List routes for the submodule"))

(defgeneric find-child-submodule (symbol parent-submodule)
  (:documentation "Find child submodule by symbol"))

(defgeneric module-routes (module submodule)
  (:documentation "Create list of routes for module with of the parent submodule"))

(defgeneric initialize-module-instance (module context)
  (:documentation "Call for module initialization"))

(defgeneric finalize-module-instance (module context)
  (:documentation "Call for module finalization"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; submodule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass submodule ()
  ((symbol :initarg :symbol :initform nil :reader submodule-symbol)
   (module :initarg :module :initform nil :reader submodule-module)
   (context :initarg :context :initform (make-context) :reader submodule-context)
   (parent :initarg :parent :initform nil :reader submodule-parent)
   (decorators :initarg :decorators :initform nil :reader submodule-decorators)))

(defun find-submodule (symbol &optional (parent *submodule*))
  (find-child-submodule symbol parent))

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

(defmethod submodule-routes ((submodule submodule)
                             &aux (decorators (submodule-decorators submodule)))
  (mapcar (lambda (route)
            (apply-decorators route decorators))
          (module-routes (submodule-module submodule) submodule)))

(defun connect-submodule (submodule mapper)
  (iter (for route in (submodule-routes submodule))
        (routes:connect mapper route)))


