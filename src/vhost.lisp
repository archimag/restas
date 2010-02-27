;;;; vhost.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :restas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; submodule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *vhosts* nil)

(defmacro defhost (name &rest args)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((package (define-module ,name ,@args)))
       (eval `(defvar ,(intern "*MAPPER*" package) (make-instance 'routes:mapper)))
       package)))
  

(defun reconnect-all-sites ()
  (iter (for site in *vhosts*)
        (let ((mapper (symbol-value (find-symbol "*MAPPER*" site))))
          (routes:reset-mapper mapper)
          (connect-submodule (make-instance 'submodule
                                            :module site
                                            :context (make-preserve-context))
                             mapper))))

          ;; (iter (for (name instance) in-hashtable (symbol-value (find-symbol "*SITE-PLUGINS*" site)))
          ;;       (connect-submodule instance
          ;;                          mapper)))))


(defun site-url (submodule route-symbol &rest args)
  (if submodule
      (with-context (slot-value submodule 'context)
        (apply 'genurl route-symbol args))
      (apply 'genurl route-symbol args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start-site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun start-site (site &key hostname (port 80))
  (unless (find site *vhosts*)
    (setf (gethash (list hostname port)
                   *hosts-mappers*)
          (symbol-value (find-symbol "*MAPPER*" site)))
    (push (find-package site) *vhosts*)
    (reconnect-all-sites))
  (unless (find port *acceptors* :key #'hunchentoot:acceptor-port )
    (push (hunchentoot:start (make-instance 'restas-acceptor
                                            :port port))
          *acceptors*)))
    
    
