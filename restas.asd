;;;; restas.asd
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:restas-system
  (:use #:cl #:asdf))

(in-package #:restas-system)

(when (find-system 'asdf-system-connections nil)
  (operate 'load-op 'asdf-system-connections))

(defsystem restas
    :depends-on (#:hunchentoot #:routes #:garbage-pools #:alexandria #+swank-archimag #:closer-mop )
    :components
    ((:module "src"
              :components
              ((:file "packages")
               (:file "special" :depends-on ("packages"))
               (:file "memoize" :depends-on ("special"))
               (:file "errors" :depends-on ("special"))
               (:file "render" :depends-on ("special"))
               (:file "context" :depends-on ("special"))
               (:file "module" :depends-on ("context"))
               (:file "route" :depends-on ("module" "render"))
               (:file "hunchentoot" :depends-on ("module" "memoize" "errors"))))
     #+swank-archimag
     (:module "slime" 
              :components ((:file "restas-swank"))
              :depends-on ("src"))
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

