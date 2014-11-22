;;;; restas.hunchentoot.asd
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem #:restas.hunchentoot
    :depends-on (#:restas.core #:hunchentoot)
    :serial t
    :pathname "hunchentoot"
    :components ((:file "package")
                 (:file "errors")
                 (:file "request")
                 (:file "reply")
                 (:file "hunchentoot")))
