;;;; example-3.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(ql:quickload "cl-who")
(ql:quickload "restas")

(restas:define-module #:restas.example-3
  (:use #:cl))

(in-package #:restas.example-3)

(restas:define-route main ("" :method :get)
  (who:with-html-output-to-string (out)
    (:html
     (:body
      ((:form :method :post 
              :enctype "multipart/form-data")
       ((:input :name "file"
                :type "file"))
       (:br)
       ((:input :type "submit"
                :value "Send")))))))

(restas:define-route main/post ("" :method :post)
  (let ((file-info (hunchentoot:post-parameter "file")))
    (if file-info
        (who:with-html-output-to-string (out)
          (:html
           (:body
            (:div
             (:b  "Name: ")
             (who:str (second file-info)))
            (:div
             (:b  "Content-Type: ")
             (who:str (third file-info)))
            (:div
             (:b "Content")
             (:br)
             (who:str (hunchentoot:escape-for-html (alexandria:read-file-into-string (first file-info)))))
            ((:a :href (restas:genurl 'main)) "Try again"))))
        (restas:redirect 'main))))

(restas:start '#:restas.example-3 :port 8080)
