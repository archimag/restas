;;;; restas-extra.asd

(defpackage #:restas-extra-system
  (:use #:cl #:asdf))

(in-package #:restas-extra-system)

(defsystem restas-extra
  :depends-on (#:restas #:closure-template #:local-time)
  :components ((:module "plugins"
                        :components ((:file "directory-publisher")))))
