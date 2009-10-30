;;; plugins.lisp

(in-package :restas)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *plugins* (make-hash-table :test 'equal)))

(defun connect-plugin (plugin)
  (let ((plugin-routes (symbol-value (find-symbol "*ROUTES*" plugin))))
    (do-symbols (route-symbol plugin-routes)
      (let ((s (find-symbol (symbol-name route-symbol)
                            plugin)))
      (routes:connect (if (eql (get s :protocol) :chrome)
                          *chrome-mapper*
                          *mapper*)
                      (funcall (get s :initialize)))))))

(defun connect-all-plugins ()
  (iter (for (name plugin) in-hashtable *plugins*)
        (connect-plugin plugin)))

(defun reconnect-all-plugins ()
  (print "reconnect-all-plugins")
  (routes:reset-mapper *mapper*)
  (routes:reset-mapper *chrome-mapper*)
  (connect-all-plugins))

;;; plugin support

(defmacro defparameter/update (var val &optional doc)
  `(progn
       (defparameter ,var ,val ,doc)
     (eval-when (:execute)
       (restas:plugin-update))))

(defmacro defun/update (name args &body body)
  `(progn
       (defun ,name ,args ,@body)
     (eval-when (:execute)
       (restas:plugin-update))))


;;; define-plugin

(defparameter *route-macros* '(define-route))

(defmacro define-plugin (name &rest options)
  (let ((use (cdr (assoc :use options)))
        (export (cdr (assoc :export options)))
        (impl-package-name (format nil "~:@(~A.IMPL.ROUTES~)" name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((package (defpackage ,name (:use ,@use) (:export #:*baseurl* ,@export))))
         (flet ((set-package-var (vname &optional value)
                  (eval `(defparameter ,(intern vname package) ,value))))
           (iter (for s in (list* 'defun/update 'defparameter/update '*request-pool* '*bindings* 'genurl *route-macros*))
                 (import s package))
           (set-package-var "*ROUTES*" (defpackage ,impl-package-name (:use)))
           (set-package-var "*BASEURL*")
           (set-package-var "*DEFAULT-CONTENT-TYPE*" ,(cadr (assoc :default-content-type options)))
           (setf (gethash ',name *plugins*)
                 (find-package package))
           package)))))


;;;; define-initialization

(defmacro define-initialization (&body body)
  (let ((init-func-name (intern "%PLUGIN-INITIALIZE-FUNCTION%")))
    `(defun ,init-func-name () ,@body)))