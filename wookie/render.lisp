;;;; wookie.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.wookie)

(defmethod restas:render-object :around (designer (future asf:future))
  (let ((request restas:*request*)
        (reply restas:*reply*)
        (module restas:*module*)
        (result (bb:make-promise)))
    (asf:future-handler-case
     (asf:alet ((object future))
       (bb:finish result
                  (let ((restas:*request* request)
                        (restas:*reply* reply))
                    (restas:with-module module
                      (restas:render-object designer object)))))
     (t (e)
        (bb:signal-error result e)))))

(defmethod restas:render-object :around (designer (promise bb:promise))
  (let ((request restas:*request*)
        (reply restas:*reply*)
        (module restas:*module*)
        (result (bb:make-promise)))
    #|------------------------------------------------------------------------|#
    (bb:promise-handler-case
     (bb:alet ((object promise))
       (bb:finish result
                  (let ((restas:*request* request)
                        (restas:*reply* reply))
                    (restas:with-module module
                      (restas:render-object designer object)))))
     (t (e)
        (bb:signal-error result e)))))
