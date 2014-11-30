;;;; restas.core.asd
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem #:restas.core
  :depends-on (#:bordeaux-threads #:routes #:alexandria #:data-sift #:cl-base64 #:cl-fad #:cffi)
  :pathname "core"
  :serial t
  :components ((:file "packages")
               (:file "special")
               (:file "util")

               (:file "http-codes")
               (:file "mime-types")
               
               (:file "declarations")

               (:file "listener")
               (:file "request")
               (:file "reply")
               (:file "cookie")
               
               (:file "status")
               (:file "render" )
               (:file "context")
               (:file "module")
               (:file "route")
               (:file "decorators" )
               (:file "vhost")
               
               (:file "policy")))
