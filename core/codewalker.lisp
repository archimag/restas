;;;; codewalker.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:restas)

(defmacro with-keep-restas-context-transformation (form)
  (alexandria:with-unique-names (request reply route module)
    (labels ((visitor (item)
               (when (typep item 'cl-walker:lambda-function-form)
                 (modify-lambda item))
               item)
             #|---------------------------------------------------------------|#
             (make-bind (x y)
               (make-instance 'cl-walker:variable-binding-entry-form
                              :name x
                              :value (make-instance 'cl-walker:free-variable-reference-form
                                                    :name y)))
             #|---------------------------------------------------------------|#
             (make-with-module (body)
               (make-instance 'cl-walker:free-application-form
                              :operator 'with-module
                              :arguments (list* (make-instance 'cl-walker:special-variable-reference-form
                                                               :name module)
                                                body)))
             #|---------------------------------------------------------------|#
             (modify-lambda (lform)
               (let* ((request-bind (make-bind '*request* request))
                      (reply-bind   (make-bind '*reply* reply))
                      (route-bind   (make-bind '*route* route))
                      (let-form (make-instance 'cl-walker:let-form
                                               :parent lform
                                               :bindings (list request-bind
                                                               reply-bind
                                                               route-bind)
                                               :body (list (make-with-module (cl-walker:body-of lform))))))
                 #|-----------------------------------------------------------|#
                 (setf (cl-walker:parent-of request-bind) let-form
                       (cl-walker:parent-of reply-bind) let-form)
                 #|-----------------------------------------------------------|#
                 (iter (for x in (cl-walker:body-of lform))
                       (setf (cl-walker:parent-of x) let-form))
                 #|-----------------------------------------------------------|#
                 (setf (cl-walker:body-of lform)
                       (list let-form)))))
      #|----------------------------------------------------------------------|#
      (let* ((ast (cl-walker:walk-form form))
             #|---------------------------------------------------------------|#
             (request-bind (make-bind request '*request*))
             (reply-bind (make-bind reply '*reply*))
             (route-bind (make-bind route '*route*))
             (module-bind (make-bind module '*module*))
             #|---------------------------------------------------------------|#
             (result-form (make-instance  'cl-walker:let-form
                                          :parent (cl-walker:parent-of ast)
                                          :bindings (list request-bind
                                                          reply-bind
                                                          route-bind
                                                          module-bind)
                                          :body (list ast))))
        #|--------------------------------------------------------------------|#
        (cl-walker:map-ast #'visitor ast)
        (cl-walker:unwalk-form result-form)))))
