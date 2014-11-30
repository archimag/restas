;;;; restas.wookie.asd
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem #:restas.wookie
    :depends-on (#:restas.core #:wookie #:blackbird)
    :serial t
    :pathname "wookie"
    :components ((:file "package")
                 (:file "debug")
                 (:file "listener")
                 (:file "request")
                 (:file "reply")
                 (:file "static")
                 (:file "render")
                 (:file "decorators")
                 (:file "wookie")))
