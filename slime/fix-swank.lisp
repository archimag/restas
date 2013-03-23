;;;; fix-swank.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:swank)

(export 'inspect-object)
(export 'defslimefun)
(export 'object-source-location)

(defgeneric object-source-location (object)
  (:documentation "Generic method for search source location of object.
Default implementation call find-source-location.")
  (:method (object)
    (find-source-location object)))

(defslimefun find-definition-for-thing (thing)
  (object-source-location thing))
 
(defslimefun find-source-location-for-emacs (spec)
  (object-source-location (value-spec-ref spec)))

(unless (boundp 'swank::*inspector-history*)
  (swank::reset-inspector))
