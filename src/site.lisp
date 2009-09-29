;;;; site.lisp

(in-package :restas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; plugin-instance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric calculate-user-login (instance request))

(defgeneric adopt-route-result (instance obj))

(defclass plugin-instance ()
  ((plugin :initarg :plugin :initform nil)   
   (context :initarg :context :initform (make-preserve-context))))

(defmethod calculate-user-login ((instance plugin-instance) request)
  nil)

(defmethod adopt-route-result ((instance plugin-instance) (code integer))
  (setf (hunchentoot:return-code*)
        code))

(defmethod adopt-route-result ((instance plugin-instance) (path pathname))
  (hunchentoot:handle-static-file path))

(defmethod adopt-route-result ((instance plugin-instance) obj)
  obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (defparameter *sites* (make-hash-table :test 'equal)))
(defparameter *sites* nil)

(defmacro defsite (&optional package)
  (let ((site-plugins-symbol (if package
                                (intern "*SITE-PLUGINS*" package)
                                (intern "*SITE-PLUGINS*"))))
  `(progn
     (defparameter ,site-plugins-symbol (make-hash-table))
     (push (or ,package *package*)
           *sites*))))
  


(defun connect-plugin-instance (instance)
  (with-slots (plugin context) instance
    (let ((plugin-routes (symbol-value (find-symbol "*ROUTES*" plugin))))
      (do-symbols (route-symbol plugin-routes)
        (let* ((s (find-symbol (symbol-name route-symbol)
                               plugin))
               (route (with-context context
                        (funcall (get s :initialize)))))
          (setf (slot-value route 'plugin-instance)
                instance)
          (routes:connect (if (eql (get s :protocol) :chrome)
                              *chrome-mapper*
                              *mapper*)
                          route))))))


(defun reconnect-all-sites ()
  (routes:reset-mapper *mapper*)
  (routes:reset-mapper *chrome-mapper*)
  (iter (for site in *sites*)
        (iter (for (name instance) in-hashtable (symbol-value (find-symbol "*SITE-PLUGINS*" site)))
              (connect-plugin-instance instance))))

(defun connect-site (site)
  (unless (find site *sites*)
    (push site *sites*))
  (reconnect-all-sites))


(defmacro define-site-plugin (name (plugin &optional (plugin-instance-class 'plugin-instance)) &body bindings)
  (let ((site-plugins (find-symbol "*SITE-PLUGINS*")))
    `(progn
       (setf (gethash ',name ,site-plugins)
           (let ((context (make-preserve-context)))
             (iter (for (symbol value) in ',bindings)
                   (context-add-variable context symbol)
                   (setf (context-symbol-value context symbol)
                         value))
             (make-instance ,plugin-instance-class
                            :plugin ,plugin
                            :context context)))
       (eval-when (:execute)
         (reconnect-all-sites))
       )))
