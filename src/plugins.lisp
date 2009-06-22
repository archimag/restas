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
  (connect-all-plugins))

;;; plugin support

(defmacro defparameter/update (var val &optional doc)
  `(progn
;;     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,var ,val ,doc)
     (eval-when (:execute)
       (restas:plugin-update))))

(defmacro defun/update (name args &body body)
  `(progn
     ;;(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name ,args ,@body)
     (eval-when (:execute)
       (restas:plugin-update))))

;;; define-plugin
;; (defmacro define-plugin (name &rest options)
;;   (flet ((define-package (name &key use)
;;            (let ((old (find-package name)))
;;                 (if old (delete-package old)))
;;            (make-package name :use use)))
;;     (let ((package (define-package name :use (cdr (assoc :use options)))))
;;       `(progn 
;;          (iter (for s in '(defun/update defparameter/update *request-pool*))
;;                (import s ,package))
;;          (defparameter ,(intern "*ROUTES*" package) ,(define-package (format nil "~:@(~A.IMPL.ROUTES~)" name)))
;;          (defparameter ,(intern "*XPATH-FUNCTIONS*" package) nil)
;;          (defparameter ,(intern "*XSLT-ELEMENTS*" package) nil)
;;          (defparameter ,(intern "*BASEPATH*" package) ,(cadr (assoc :basepath options)))
;;          (defparameter ,(intern "*BASEURL*"  package) nil)
;;          (setf (gethash ,name *plugins*)
;;                ,package)
;;          ))))

;;; define-plugin

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
           (iter (for s in '(defun/update defparameter/update *request-pool*))
                 (import s package))
           (set-package-var "*ROUTES*" (defpackage ,impl-package-name))
           (set-package-var "*XPATH-FUNCTIONS*")
           (set-package-var "*XSLT-ELEMENTS*")
           (set-package-var "*BASEPATH*" ,(cadr (assoc :basepath options)))
           (set-package-var "*BASEURL*")
           (setf (gethash ,name *plugins*)
                 (find-package package))
           )))))
         
;;        (defparameter ,(intern "*ROUTES*" name) (defpackage ,impl-package-name))
;;        (defparameter ,(intern "*XPATH-FUNCTIONS*" name) nil)
;;        (defparameter ,(intern "*XSLT-ELEMENTS*" name) nil)
;;        (defparameter ,(intern "*BASEPATH*" name) ,(cadr (assoc :basepath options)))
;;        (defparameter ,(intern "*BASEURL*"  name) nil)
;;        (setf (gethash ,name *plugins*)
;;              (find-package ,name))
;;          )))

;; (define-package-var package "*ROUTES*" (define-package (format nil "~:@(~A.IMPL.ROUTES~)" ,name)))
;; (define-package-var package "*XPATH-FUNCTIONS*")
;; (define-package-var package "*XSLT-ELEMENTS*")
;; (define-package-var package "*BASEPATH*" ,(cadr (assoc :basepath options)))
;; (define-package-var package "*BASEURL*")
;;            (setf (gethash ,name *plugins*)
;;                  package))))))


(defmacro test (name)
  `(let ((package (defpackage ,name)))
     (intern "HELLO" package)))