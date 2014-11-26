;;;; restas.wookie.asd
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem #:restas.wookie
    :depends-on (#:restas.core #:wookie)
    :serial t
    :pathname "wookie"
    :components ((:file "package")
                 (:file "listener")
                 (:file "request")
                 (:file "reply")
                 (:file "static")
                 (:file "debug")
                 (:file "wookie")))
