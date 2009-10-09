;;; restas.asd

(defpackage #:restas-system
  (:use #:cl #:asdf))

(in-package #:restas-system)

(when (find-system 'asdf-system-connections)
  (operate 'load-op 'asdf-system-connections))

(defsystem restas
    :depends-on (#:hunchentoot #:routes #:garbage-pools)
    :components
    ((:module "src"
              :components
              ((:file "packages")
               (:file "core" :depends-on ("packages"))
               (:file "preserve-context" :depends-on ("core"))               
               (:file "route" :depends-on ("preserve-context"))
               (:file "site" :depends-on ("route"))
               (:file "hunchentoot" :depends-on ("site"))
               (:file "expand-text" :depends-on ("core"))
               (:file "plugins" :depends-on ("route"))))
     (:module "optional"
              :components ((:file "optional"))
              :depends-on ("src"))))


#+asdf-system-connections
(defsystem-connection restas-xfactory
  :requires (restas xfactory)
  :components ((:module "optional"
                        :components ((:file "restas-xfactory")))))

#+asdf-system-connections
(defsystem-connection restas-ironclad
  :requires (restas ironclad)
  :components ((:module "optional"
                        :components ((:file "restas-ironclad")))))

#+asdf-system-connections
(defsystem-connection restas-zip
  :requires (restas zip)
  :components ((:module "optional"
                        :components ((:file "restas-zip")))))