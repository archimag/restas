;;;; doc.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(restas:define-module #:restas.doc
  (:use #:cl #:iter)
  (:export #:make-documentation
           #:*restas-documentation-dir*))

(in-package #:restas.doc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; make documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *langs* '("en" "ru"))

(defparameter *default-lang* "en")

(defun make-documentation (target-dir &key verbose)
  (let ((docdir (make-pathname :directory (pathname-directory (asdf:component-pathname (asdf:find-system '#:restas-doc))))))
    (iter (for lang in *langs*)
          (sphinx:make-documentation (merge-pathnames (format nil "~A/contents.txt" lang)
                                                      docdir)
                                     (merge-pathnames (format nil "~A/" lang)
                                                      target-dir)
                                     :verbose verbose))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; publish documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *restas-documentation-dir* #P"/usr/share/doc/restas/")

(restas:mount-submodule -publisher- (#:restas.directory-publisher))

(restas:define-initialization (context)
  (restas:with-context context
    (make-documentation *restas-documentation-dir*)
    (setf (restas:context-symbol-value (restas:submodule-context (restas:find-submodule '-publisher-))
                                       'restas.directory-publisher:*directory*)
          *restas-documentation-dir*)))
  
(restas:define-route entry ("")
  (let* ((accept-languages (hunchentoot:header-in* :accept-language))
         (prefer-lang (if accept-languages
                          (subseq accept-languages
                                  0
                                  (position #\, accept-languages)))))
    (hunchentoot:redirect
     (restas:genurl-submodule '-publisher-
                              'restas.directory-publisher:route
                              :path (list
                                     (if (and prefer-lang
                                              (string= (subseq (string-downcase prefer-lang) 0 2)
                                                       "ru"))
                                         "ru"
                                         *default-lang*)
                                     "")))))
