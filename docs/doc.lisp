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

(defparameter *restas-documentation-dir*
  (merge-pathnames #P"html/" (asdf:component-pathname (asdf:find-system '#:restas-doc))))

;; make documentation

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

;; publish documentation

(restas:mount-module -html- (#:restas.directory-publisher)
  (:inherit-parent-context t))

(defmethod restas:initialize-module-instance ((module (eql #.*package*)) context)
  (restas:with-context context
    (make-documentation *restas-documentation-dir* :verbose t)
    (setf (restas:context-symbol-value context 'restas.directory-publisher:*directory*)
          *restas-documentation-dir*)))

(restas:define-route entry ("")
  (let* ((accept-languages (hunchentoot:header-in* :accept-language))
         (prefer-lang (if (and accept-languages
                               (> (length accept-languages) 1))
                          (string-downcase (subseq accept-languages 0 2)))))
    (hunchentoot:redirect
     (restas:genurl '-html-.route
                    :path (list (or (find prefer-lang *langs* :test #'string=)
                                    *default-lang*)
                                "")))))
