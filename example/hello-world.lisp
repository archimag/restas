;;;; hello-world.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(ql:quickload "restas")

(restas:define-module #:restas.hello-world
  (:use #:cl))

(in-package #:restas.hello-world)

(restas:define-route main ("")
  "<h1>Hello world!</h1>")

(restas:start '#:restas.hello-world :port 8080)
