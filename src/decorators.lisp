;;;; decorators.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; no-cache-decorator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass no-cache-route (routes:proxy-route) ())

(defmethod process-route :before ((route no-cache-route) bindings)
  (setf (hunchentoot:header-out :expires)
        (hunchentoot:rfc-1123-date))
  (setf (hunchentoot:header-out :cache-control)
        "max-age=0, no-store, no-cache, must-revalidate"))

(defun @no-cache (route)
  (make-instance 'no-cache-route :target route))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Nginx X-Accel-Redirect decorator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass nginx-accel-redirect-route (routes:proxy-route) ())

(defvar *nginx-internal-location* nil)
(defvar *nginx-internal-alias* nil)
(defvar *nginx-internal-root* nil)

(defun concat-pathnames (path1 path2)
  (merge-pathnames (make-pathname
                    :directory (cons :relative
                                     (cdr (pathname-directory path2))))
                   (fad:pathname-as-directory path1)))

(defmethod process-route ((route nginx-accel-redirect-route) bindings)
  (unless *nginx-internal-location*
    (error "*nginx-internal-location* is not set!"))
  (let ((result (call-next-method)))
    (cond
      ((pathnamep result)
       (setf (hunchentoot:header-out :content-type)
             (or (hunchentoot:mime-type result)
                 (hunchentoot:content-type*)))
       (setf (hunchentoot:header-out :x-accel-redirect)
             (or (and *nginx-internal-root*
                      (cffi-sys:native-namestring
                       (merge-pathnames
                        (enough-namestring result
                                           (concat-pathnames
                                            *nginx-internal-root*
                                            *nginx-internal-location*))
                                        (fad:pathname-as-directory
                                         *nginx-internal-location*))))
                 (and *nginx-internal-alias*
                      (merge-pathnames
                       (cffi-sys:native-namestring
                        (enough-namestring result *nginx-internal-alias*))
                                       *nginx-internal-location*))
                 (error "*nginx-internal-root* or *nginx-internal-alias* should be set!")))
       "")
      (t result))))

(defun @nginx-accel-redirect (origin)
  (make-instance 'nginx-accel-redirect-route :target origin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Apache X-Sendifle decorator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass apache-xsendfile-route (routes:proxy-route) ())

(defmethod process-route ((route apache-xsendfile-route) bindings)
  (let ((result (call-next-method)))
    (cond
      ((pathnamep result)
       (setf (hunchentoot:header-out :content-type)
             (or (hunchentoot:mime-type result)
                 (hunchentoot:content-type*)))
       (setf (hunchentoot:header-out :x-sendfile)
             (cffi-sys:native-namestring result))
       "")
      (t result))))

(defun @apache-xsendfile (origin)
  (make-instance 'apache-xsendfile-route :target origin))
