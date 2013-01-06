;;;; custom-acceptor.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(ql:quickload "restas")

(restas:define-module #:restas.custom-acceptor.example
  (:use #:cl))

(in-package #:restas.custom-acceptor.example)

(ensure-directories-exist #P"/tmp/hunchentoot/")

(defclass acceptor (restas:restas-acceptor)
  ()
  (:default-initargs
   :access-log-destination #P"/tmp/hunchentoot/access_log"
   :message-log-destination #P"/tmp/hunchentoot/error_log"))

(restas:define-route any ("*path")
  (declare (ignore path))
  "Hello world")

(restas:start '#:restas.custom-acceptor.example
              :port 8080
              :acceptor-class 'acceptor)
