;;;; restas-doc.asd
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem restas-doc
  :depends-on (#:restas #:sphinx #:restas-directory-publisher)
  :components ((:file "doc")))
