;;;; vhost.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

(defvar *vhosts* nil)

(defclass vhost ()
  ((hostname :initarg :hostname :reader vhost-hostname)
   (port :initarg :port :reader vhost-port)
   (mapper :initform (make-instance 'routes:mapper))
   (modules :initform nil)))

(defun vhost-hostname-port (vhost)
  (cons (vhost-hostname vhost)
        (vhost-port vhost)))

(defun find-vhost (hostname.port)
  (or (find hostname.port
            *vhosts*
            :key #'vhost-hostname-port
            :test #'equal)
      (find (cons nil (cdr hostname.port))
            *vhosts*
            :key #'vhost-hostname-port
            :test #'equal)))

(defun ensure-vhost-exist (hostname port)
  "Tests whether the vhost exist, and attempts to create them if they do not."
  (or (find (cons hostname port)
            *vhosts*
            :key #'vhost-hostname-port
            :test #'equal)
      (car (push (make-instance 'vhost
                                :hostname hostname
                                :port port)
                 *vhosts*))))

(defgeneric add-toplevel-module (module hostname port &key context url
                                                        render-method decorators))

(defmethod add-toplevel-module ((module symbol) hostname port
                                &key context url render-method decorators)
  (add-toplevel-module (find-package module) hostname port
                       :context context
                       :url url
                       :render-method render-method
                       :decorators decorators))

(defmethod add-toplevel-module ((package package) hostname port
                                &key context url render-method decorators)
  (let ((vhost (ensure-vhost-exist hostname port))
        (module (make-instance 'pkgmodule
                               :package package
                               :context context
                               :url (if url (routes:parse-template url))
                               :render-method render-method
                               :decorators decorators)))
    (push module (slot-value vhost 'modules))
    (let ((*module* module))
      (connect-module module (slot-value vhost 'mapper))
      (initialize-module-instance module (module-context module))
      ))
  (values))

(defun reconnect-all-routes ()
  (iter (for vhost in *vhosts*)
        (let ((mapper (slot-value vhost 'mapper)))
          (routes:reset-mapper mapper)
          (iter (for module in (slot-value vhost 'modules))
                (finalize-module-instance module (module-context module))
                (reinitialize-instance module)
                (initialize-module-instance module (module-context module))
                (connect-module module mapper))))
  (values))


(defun clear-all-vhost ()
  (iter (for vhost in *vhosts*)
        (routes:reset-mapper (slot-value vhost 'mapper))
        (setf (slot-value vhost 'modules) nil)))
