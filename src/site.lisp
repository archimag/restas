;;;; site.lisp

(in-package :restas)

;;; plugin-instance

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

;;;; site

(defclass site ()
  ((plugins :initform nil :accessor site-plugins)))


(defun site-add-plugin (site plugin-instance &key plugin-vars)
  (let ((context (slot-value plugin-instance 'context)))
    (iter (for (var . val) in plugin-vars)
          (context-add-variable context var)
          (when val
            (setf (context-symbol-value context var)
                  val)))
    (push plugin-instance
          (site-plugins site))))

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
  (iter (for instance in (site-plugins site))
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
