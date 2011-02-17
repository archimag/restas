;;;; restas.asd
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem restas
    :depends-on (#:hunchentoot #:bordeaux-threads #:routes #:alexandria #:cl-mongrel2 #:uuid #+swank-archimag #:closer-mop )
    :components
    ((:module "src"
              :components
              ((:file "packages")
               (:module "http"
                        :components ((:file "request")
                                     (:file "reply"))
                        :depends-on ("packages"))
               (:file "special" :depends-on ("http"))
               (:file "memoize" :depends-on ("special"))
               (:file "errors" :depends-on ("special"))
               (:file "render" :depends-on ("special"))
               (:file "context" :depends-on ("special"))
               (:file "module" :depends-on ("context"))               
               (:file "pkg-module" :depends-on ("module"))
               (:file "route" :depends-on ("pkg-module" "render"))
               (:file "vhost" :depends-on ("special"))
               (:file "mongrel2" :depends-on ("vhost" "module" "memoize" "errors"))
               (:file "hunchentoot" :depends-on ("vhost" "module" "memoize" "errors"))))
     #+swank-archimag
     (:module "slime" 
              :components ((:file "restas-swank"))
              :depends-on ("src"))))
