;;;; listener.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.hunchentoot)

(defmethod restas:listener-port ((listener hunchentoot:acceptor))
  (hunchentoot:acceptor-port listener))

(defmethod restas:listener-address ((listener hunchentoot:acceptor))
  (hunchentoot:acceptor-address listener))
