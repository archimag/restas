;;;; render.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.wookie)

(defmethod restas:render-object :around (designer (promise bb:promise))
  (let ((request restas:*request*)
        (reply restas:*reply*)
        (module restas:*module*))
    #|------------------------------------------------------------------------|#
    (bb:with-promise (resolve reject)
      (bb:catcher
       (bb:alet ((object promise))
         (resolve (let ((restas:*request* request)
                        (restas:*reply* reply))
                    (restas:with-module module
                      (restas:render-object designer object)))))
       (t (e)
          (reject e))))))


