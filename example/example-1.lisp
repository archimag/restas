;;;; example-1.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(ql:quickload "cl-who")
(ql:quickload "restas")

(restas:define-module #:restas.example-1
  (:use #:cl))

(in-package #:restas.example-1)

(restas:define-route main ("" :method :get)
  (who:with-html-output-to-string (out)
    (:html
     (:body
      ((:form :method :post)
       ((:input :name "message"))
       ((:input :type "submit" :value "Send")))))))

(restas:define-route main/post ("" :method :post)
  (who:with-html-output-to-string (out)
    (:html
     (:body
      (:div
       (:b (who:fmt "test message: ~A"
                    (hunchentoot:post-parameter "message"))))
      ((:a :href (restas:genurl 'main)) "Try again")))))

(restas:start '#:restas.example-1 :port 8080)
