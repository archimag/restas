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

(defun add-toplevel-submodule (submodule hostname port)
  (let ((vhost (or (find (cons hostname port)
                         *vhosts*
                         :key #'vhost-hostname-port
                         :test #'equal)
                   (car (push (make-instance 'vhost
                                             :hostname hostname
                                             :port port)
                              *vhosts*)))))
    (push submodule
          (slot-value vhost 'modules)))
  (values))
  

(defun reconnect-all-routes (&key (reinitialize t))
  (iter (for vhost in *vhosts*)
        (let ((mapper (slot-value vhost 'mapper)))
          (routes:reset-mapper mapper)
          (iter (for module in (slot-value vhost 'modules))
                (when reinitialize
                  (reinitialize-instance module))
                (connect-submodule module mapper))))
  (values))
