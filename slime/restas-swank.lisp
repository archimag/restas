;;;; restas-swank.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:restas.swank
  (:use #:cl #:iter)
  (:export #:inspect-module))

(in-package #:restas.swank)

(defstruct %restas-module
  package)

(defstruct %restas-route
  symbol)

(defmethod swank:emacs-inspect ((module %restas-module))
  (let ((package (find-package (%restas-module-package module))))
    `("" "Name: " (:value ,(package-name package))
         (:newline)
         (:newline)
         "Routes: " 
         (:newline)
         ,@(iter (for route in-package (symbol-value (find-symbol restas::+routes-symbol+ package)))
                 (collect (list :value
                                (find-symbol (symbol-name route)
                                             package)))
                 (collect '(:newline))))))

(swank:defslimefun inspect-module (package)
  (swank:inspect-object (make-%restas-module :package package)))
