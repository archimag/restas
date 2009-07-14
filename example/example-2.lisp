;;; example.lisp

(asdf:operate 'asdf:load-op :restas)
(asdf:operate 'asdf:load-op :cl-who)

(restas:define-plugin :restas.example-2
  (:use :cl))

(in-package :restas.example-2)

(restas:reconnect-all-plugins)
(restas:start-web-server 8080)
