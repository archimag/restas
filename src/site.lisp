;;;; site.lisp

(in-package :restas)

;;(defclass site () ())

(defun default-adopt-plugin-result (res)
  (typecase res
    (integer (setf (hunchentoot:return-code*) res))
    (pathname (hunchentoot:handle-static-file res))
    (otherwise res)))

(defclass plugin-instance ()
  ((plugin :initarg plugin :initform nil)
   (adopt-result-fun :initarg :adopt-result-fun :initform #'default-adopt-plugin-result)
   (context :initarg :context :initform nil)))

(defclass site ()
  ((plugins :initform nil :accessor site-plugins)
   (compute-user-name :initarg compute-user-name :initform nil)))


(defun site-add-plugin (site plugin &key plugin-vars (adopt-plugin-result #'default-atopt-plugin-result))
  (let ((context (make-preserve-context)))
    (iter (for (var . val) in plugin-vars)
          (context-add-variable context var)
          (when val
            (setf (context-symbol-value context var)
                  val)))
    (push (make-instance 'plugin-instance
                         :plugin plugin
                         :context context
                         :adopt-result-fun adopt-plugin-result)
          (site-plugins site))))

(defun connect-site (site)
  (iter (for instance in (site-plugins site))
        (with-slots (plugin context) instance
          (with-context context
            (connect-plugin plugin)))))
        