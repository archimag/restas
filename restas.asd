;;; restas.asd

(defpackage :restas-system
  (:use :cl :asdf))

(in-package :restas-system)

(defsystem :restas
    :depends-on (#:hunchentoot #:routes #:garbage-pools)
    :components
    ((:module :src
              :components
              ((:file "packages")
               (:file "core" :depends-on ("packages"))
               (:file "preserve-context" :depends-on ("core"))               
               (:file "route" :depends-on ("preserve-context"))
               (:file "site" :depends-on ("route"))
               (:file "hunchentoot" :depends-on ("route"))
               (:file "expand-text" :depends-on ("core"))
               (:file "plugins" :depends-on ("route"))
               ))))
