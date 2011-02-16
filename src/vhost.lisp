;;;; vhost.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

(defvar *vhosts* nil)

(defclass vhost ()
  ((host :initarg :host :reader vhost-host)
   (mapper :initform (make-instance 'routes:mapper))
   (modules :initform nil)))

(defun find-vhost (host)
  (or (find host
            *vhosts*
            :key #'vhost-host
            :test #'string=)
      (find-if #'null
               *vhosts*
               :key #'vhost-host)))

(defun add-toplevel-submodule (host submodule)
  (let ((vhost (or (if host
                       (find host
                             *vhosts*
                             :key #'vhost-host
                             :test #'string=)
                       (find-if #'null
                                *vhosts*
                                :key #'vhost-host))
                   (car (push (make-instance 'vhost
                                             :host host)
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
