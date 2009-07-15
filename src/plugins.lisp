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

(defparameter *route-macros* '(define-simple-route define-filesystem-route define-fs-xsl-route))

(defmacro define-plugin (name &rest options)
  (let ((package-name (symbol-name name))
        (use (cdr (assoc :use options)))
        (impl-package-name (format nil "~:@(~A.IMPL.ROUTES~)" name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((package (defpackage ,package-name (:use ,@use))))
         (flet ((set-package-var (name &optional value)
                  ;;(setf (symbol-value (intern name package)) value)
                  (eval `(defparameter ,(intern name package) ,value))
                  ))
           (iter (for s in (list* 'defun/update 'defparameter/update '*request-pool* '*bindings* 'genurl *route-macros*))
                 (import s package))
           (set-package-var "*ROUTES*" (defpackage ,impl-package-name))
           (set-package-var "*XPATH-FUNCTIONS*")
           (set-package-var "*XSLT-ELEMENTS*")
           (set-package-var "*BASEPATH*" ,(cadr (assoc :basepath options)))
           (set-package-var "*BASEURL*")
           (set-package-var "*DEFAULT-CONTENT-TYPE*" ,(cadr (assoc :default-content-type options)))
           (eval `(unless (functionp ',(intern "COMPUTE-USER-LOGIN-NAME" package))
                      (defun ,(intern "COMPUTE-USER-LOGIN-NAME" package) () nil)))
           (setf (gethash ,name *plugins*)
                 (find-package package))
           ))))) 
