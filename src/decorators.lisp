;;;; decorators.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; no-cache-decorator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass no-cache-route (routes:proxy-route) ())

(defmethod process-route :before ((route no-cache-route) bindings)
  (setf (hunchentoot:header-out :expires)
        (hunchentoot:rfc-1123-date))  
  (setf (hunchentoot:header-out :cache-control)
        "max-age=0, no-store, no-cache, must-revalidate"))

(defun @no-cache (route)
  (make-instance 'no-cache-route :target route))

