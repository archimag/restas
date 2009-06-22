;;; restas.asd

(defpackage :restas-system
  (:use :cl :asdf))

(in-package :restas-system)

(defsystem :restas
    :depends-on (#:hunchentoot #:routes #:cl-who #:cl-libxslt #:colorize)
    :components
    ((:module :src
              :components
              ((:file "packages")
               (:file "core" :depends-on ("packages"))
               (:file "route" :depends-on ("core"))
               (:file "hunchentoot" :depends-on ("route"))
               (:file "expand-text" :depends-on ("core"))
               (:file "overlay" :depends-on ("expand-text"))
               (:file "plugins" :depends-on ("core"))
               ;;(:file "test" :depends-on ("hunchentoot" "overlay"))
               ))))

(defsystem :restas-new
  :depends-on (#:restas))

