;;;; restas.asd
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem #:restas-swank
    :depends-on (#:restas #:closer-mop)
    :serial t
    :components ((:file "fix-swank")
                 (:file "routes")
                 (:file "restas")))
