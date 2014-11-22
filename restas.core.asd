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
               (:file "util")
               (:file "special")



               ;; (:file "rfc2388")
               (:file "http-codes")
               (:file "mime-types")

               ;; (:file "util")
               (:file "known-words")


               (:file "declarations")
               
               ;; (:file "errors" :depends-on ("special"))
               
               (:file "request")
               (:file "reply")

               
               (:file "render" )
               (:file "context")
               (:file "module")
               (:file "route")
               (:file "decorators" )
               (:file "vhost")
               
               ;; (:file "hunchentoot" :depends-on ("vhost" "module" "errors"))
               
               (:file "policy")))
