;;;; listener.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.wookie)

(defvar *listener*)

(defmethod restas:listener-port ((listener wookie:listener))
  (wookie:listener-port listener))

(defmethod restas:listener-address ((listener wookie:listener))
  (wookie:listener-bind listener))

(defmethod restas:listener-ssl-p ((listener wookie:listener))
  (typep *listener* 'wookie:ssl-listener))
