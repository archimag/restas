;;;; statistics-collection.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(asdf:operate 'asdf:load-op '#:cl-who)
(asdf:operate 'asdf:load-op '#:restas)

(restas:define-module #:restas.example.statistic-collection
  (:use #:cl))

(in-package #:restas.example.statistic-collection)

(defparameter *urlstats* (make-hash-table :test 'equal))

(defun after-dispatch-request-handler ()
  (incf (gethash (hunchentoot:request-uri*) *urlstats* 0)))

(defmethod restas:initialize-module-instance ((module (eql #.*package*)) context)
  (pushnew 'after-dispatch-request-handler restas:*before-dispatch-request-hook*))

(restas:define-route show-statistic ("")
  (who:with-html-output-to-string (out)
    (:html
     (:body
      (:table
          (:tbody
           (loop for url being the hash-keys in *urlstats* using (hash-value count)
                do (who:htm
                    (:tr
                        (:td (who:fmt url))
                        (:td (who:fmt "~A" count)))))))))))

