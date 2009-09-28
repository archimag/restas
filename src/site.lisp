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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *sites* (make-hash-table :test 'equal)))


                

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

(defun connect-site (site)
  (iter (for (name instance) in-hashtable (symbol-value (find-symbol "*SITE-PLUGINS*" site)))
        (connect-plugin-instance instance)))
        
(defun connect-all-sites ()
  (iter (for (name site) in-hashtable *sites*)
        (print name)
        (connect-site site)))

(defun reconnect-all-sites ()
  (print "reconnect-all-sites")
  (routes:reset-mapper *mapper*)
  (routes:reset-mapper *chrome-mapper*)
  (connect-all-sites))


(defmacro define-site-plugin (name (plugin &optional (plugin-instance-class 'plugin-instance)) &body bindings)
  (let ((site-plugins (find-symbol "*SITE-PLUGINS*")))
    `(progn
       (setf (gethash ',name ,site-plugins)
           (let ((context (make-preserve-context)))
             (iter (for (symbol value) in ,bindings)
                   (setf (context-symbol-value context symbol)
                         value))
             (make-instance ,plugin-instance-class
                            :plugin ,plugin)))
       (eval-when (:execute)
         (reconnect-all-sites))
       )))
