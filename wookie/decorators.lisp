;;;; wookie.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.wookie)

(defclass keep-context-route (routes:proxy-route) ())

(defmethod restas:process-route :around ((route keep-context-route) bindings)
  (let ((bb:*promise-keep-specials* (append (list* 'restas:*request*
                                                   'restas:*reply*
                                                   'restas:*module*
                                                   (restas:context-all-vars
                                                    (restas:module-context
                                                     (restas:route-module route))))
                                            bb:*promise-keep-specials*)))
    (call-next-method)))

(defun @keep-context (route)
  (make-instance 'keep-context-route :target route))
