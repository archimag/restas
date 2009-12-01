;;;; site.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :restas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; plugin-instance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric calculate-user-login (instance request))

(defgeneric adopt-route-result (instance obj))

(defclass plugin-instance ()
  ((plugin :initarg :plugin :initform nil)   
   (context :initarg :context :initform (make-preserve-context))))

(defmethod calculate-user-login ((instance plugin-instance) request)
  nil)

(defmethod adopt-route-result ((instance plugin-instance) (code integer))
  (setf (hunchentoot:return-code*)
        code))

(defmethod adopt-route-result ((instance plugin-instance) (path pathname))
  (hunchentoot:handle-static-file path))

(defmethod adopt-route-result ((instance plugin-instance) obj)
  obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sites* nil)

(defmacro defsite (name &rest args)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((package (define-plugin ,name ,@args)))
       (eval `(defvar ,(intern "*SITE-PLUGINS*" package) (make-hash-table)))
       (eval `(defvar ,(intern "*MAPPER*" package) (make-instance 'routes:mapper)))       
       package)))
  
(defun connect-plugin-instance (instance &optional (mapper *mapper*))
  (with-slots (plugin context) instance
    (let ((plugin-routes (symbol-value (find-symbol "*ROUTES*" plugin))))
      (do-symbols (route-symbol plugin-routes)
        (let* ((s (find-symbol (symbol-name route-symbol)
                               plugin))
               (route (with-context context
                        (funcall (get s :initialize)))))
          (setf (slot-value route 'plugin-instance)
                instance)
          (routes:connect (if (eql (get s :protocol) :chrome)
                              *chrome-mapper*
                              mapper)
                          route))))))


(defun reconnect-all-sites ()
  (routes:reset-mapper *chrome-mapper*)
  (iter (for site in *sites*)
        (let ((mapper (symbol-value (find-symbol "*MAPPER*" site))))
          (routes:reset-mapper mapper)
          (iter (for (name instance) in-hashtable (symbol-value (find-symbol "*SITE-PLUGINS*" site)))
                (connect-plugin-instance instance
                                         mapper)))))


(defmacro define-site-plugin (name (plugin &optional (plugin-instance-class 'plugin-instance)) &body bindings)
  (let ((site-plugins (find-symbol "*SITE-PLUGINS*")))
    `(progn
       (let ((instance (gethash ',name ,site-plugins))
             (finish-func (find-symbol "%PLUGIN-FINALIZE-FUNCTION%" ',plugin)))
         (when (and instance finish-func (symbol-function finish-func))
           (funcall finish-func (slot-value instance 'context))))
       (let ((context (make-preserve-context)))
         (iter (for (symbol value) in ',bindings)
               (context-add-variable context symbol)
               (setf (context-symbol-value context symbol)
                     (eval value)))
         (setf (gethash ',name ,site-plugins)
               (make-instance ',plugin-instance-class
                              :plugin ',plugin
                              :context context))
         (let ((init-func (find-symbol "%PLUGIN-INITIALIZE-FUNCTION%" ',plugin)))
           (when (and init-func
                      (symbol-function init-func))
             (funcall init-func context))))
       (eval-when (:execute)
         (reconnect-all-sites)))))

(defun site-url (plugin-instance route-symbol &rest args)
  (with-context (slot-value plugin-instance 'context)
     (apply 'genurl route-symbol args)))

