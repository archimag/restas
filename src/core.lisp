;;; core.lisp

(in-package :restas)

(defparameter *mapper* (make-instance 'routes:mapper))

(defparameter *chrome-mapper* (make-instance 'routes:mapper))
