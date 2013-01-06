;;;; example-2.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(ql:quickload "cl-who")
(ql:quickload "restas")

(restas:define-module #:restas.example-2
  (:use #:cl #:iter))

(in-package #:restas.example-2)

(restas:define-route root ("")
  (who:with-html-output-to-string (out)
    (:html
     (:head
      (:title "Example 2: Index"))
     (:body
      (:h1 "Index")
      (:ul
       (iter (for x from 1 to 10)
             (who:htm (:li
                       ((:a :href (restas:genurl 'chapter-?.html :id x))
                        (who:fmt "Chapter ~A" x))))))))))

(restas:define-route chapter-?.html ("chapter-:(id).html")
  (who:with-html-output-to-string (out)
    (:html
     (:head
      (:title (who:fmt "Example 2. Chapter ~A" id)))
     (:body
      (:h1 (who:fmt "Chapter ~A" id))
      (:ul
       (iter (for x from 1 to 10)
             (who:htm (:li
                       ((:a :href (restas:genurl 'chapter-?-?.html :id1 id :id2 x))
                        (who:fmt "Chapter ~A-~A" id x))))))
      ((:a :href (restas:genurl 'root))
       "Back to Index")))))

(restas:define-route chapter-?-?.html ("chapter-:(id1)-:(id2).html")
  (who:with-html-output-to-string (out)
    (:html
     (:head
      (:title (who:fmt "Example 2. Chapter ~A-~A" id1 id2)))
     (:body
      (:h1 (who:fmt "Chapter ~A-~A" id1 id2))
      (:p (who:fmt "This is a chapter ~A-~A" id1 id2))
      ((:a :href (restas:genurl 'chapter-?.html :id id1))
       (who:fmt "Back to Chapter ~A" id1))))))
  
(restas:start '#:restas.example-2 :port 8080)
