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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Nginx X-Accel-Redirect support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass nginx-accel-redirect-route (routes:proxy-route) ())

(defmethod process-route ((route nginx-accel-redirect-route) bindings)
  (let ((result (call-next-method)))
    (cond
      ((pathnamep result)
       (setf (hunchentoot:header-out :x-accel-redirect)
             #+sbcl (sb-ext:native-namestring result)
             #-sbcl (namestring result))
       "")
      (t result))))

(defun @nginx-accel-redirect (origin)
  (make-instance 'nginx-accel-redirect-route :target origin))
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Apache X-Sendifle support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass apache-xsendfile-route (routes:proxy-route) ())

(defmethod process-route ((route apache-xsendfile-route) bindings)
  (let ((result (call-next-method)))
    (cond
      ((pathnamep result)
       (setf (hunchentoot:header-out :content-type)
             (or (hunchentoot:mime-type result)
                 (hunchentoot:content-type*)))
       (setf (hunchentoot:header-out :x-sendfile)
             #+sbcl (sb-ext:native-namestring result)
             #-sbcl (namestring result))
       "")
      (t result))))

(defun @apache-xsendfile (origin)
  (make-instance 'apache-xsendfile-route :target origin))

