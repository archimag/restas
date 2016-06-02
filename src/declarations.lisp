;;;; declarations.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

(defvar *route-declarations* '(:sift-variables :additional-variables
                               :render-method :apply-render-method
                               :content-type :http-method
                               :requirement
                               :decorators))
(defvar *mount-module-declarations* '(:decorators :url :render-method
                                      :inherit-parent-context))

(defun split-code-declarations (body)
  (let ((declarations-map (make-hash-table))
        (code body))
    (iter (for rbody on body)
          (for form = (car rbody))
          (while (and (consp  form) (keywordp (car form))))
          (setf code (cdr rbody))
          (setf (gethash (car form) declarations-map)
                (append (cdr form)
                        (gethash (car form) declarations-map))))
    (values declarations-map code)))

(defgeneric parse-declarations (type declarations target traits))

(defmethod parse-declarations (type declarations target traits)
  (error "Unknown type of declaration: ~A" type))

(defmethod parse-declarations ((type (eql :sift-variables)) declarations target traits)
  (let ((variables (gethash :variables traits)))
    (setf (gethash :parse-vars traits)
          (cons 'list
                (iter (for (var rule) in declarations)
                      (collect (find var variables :test #'string=))
                      (collect `(data-sift:compile-parse-rule ,rule)))))))

(defmethod parse-declarations ((type (eql :additional-variables))
                               declarations target traits)
  (iter (for (var handler default) in declarations)
        (collect (if default
                     (list var default)
                     var)
          :into arglist)
        (collect (if default
                     `(list ,(alexandria:make-keyword var)
                        (alexandria:named-lambda additional-variable-handler ()
                          (or ,handler ,default)))
                     `(list ,(alexandria:make-keyword var)
                        (alexandria:named-lambda additional-variable-handler ()
                          ,handler)))
          :into handlers)
        (finally
         (setf (gethash :additional-variables-arglist traits) (cons '&key arglist)
               (gethash :additional-variables traits) (cons 'list handlers)))))

(defmethod parse-declarations ((type (eql :render-method)) declarations target traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A declaration" type))
  (setf (gethash type traits)
        `(alexandria:named-lambda make-route-render-method () ,(first declarations))))

(defmethod parse-declarations ((type (eql :apply-render-method))
                               declarations target traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A declaration" type))
  (setf (gethash :render-method traits)
        `(alexandria:named-lambda apply-render-method (data)
           (apply ,(first declarations) data))))

(defmethod parse-declarations ((type (eql :content-type)) declarations target traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A declaration" type))
  (setf (gethash type traits)
        (first declarations)))

(defmethod parse-declarations ((type (eql :decorators)) declarations target traits)
  (setf (gethash type traits)
        (cons 'list declarations)))

(defmethod parse-declarations ((type (eql :http-method)) declarations target traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A declaration" type))
  (setf (gethash :method traits)
        (first declarations)))

(defmethod parse-declarations ((type (eql :requirement)) declarations target traits)
  (setf (gethash type traits) (cons 'list declarations)))

(defmethod parse-declarations ((type (eql :url)) declarations target traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A declaration" type))
  (setf (gethash :url traits)
        (first declarations)))

(defmethod parse-declarations ((type (eql :inherit-parent-context))
                               declarations target traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A declaration" type))
  (setf (gethash :inherit-parent-context traits)
        (first declarations)))

(defun parse-all-declarations (declarations allowed-types target &optional traits)
  (let ((traits (or traits (make-hash-table))))
    (iter (for (key value) in-hashtable declarations)
          (cond
            ((member key allowed-types)
             (parse-declarations key value target traits))
            (t
             (error "Unknown declaration type: ~A" key))))
    traits))

(defmacro define-declaration (declaration scope (declarations target traits) &body body)
  `(progn
     (case ,scope
       (:route
        (pushnew ,declaration restas::*route-declarations*))
       (:mount-module
           (pushnew ,declaration restas::*mount-module-declarations*))
       (t
        (error "Unknown declaration scope: ~A, allowed are ROUTE and MOUNT-MODULE" ,scope)))
     (defmethod restas::parse-declarations ((type (eql ,declaration)) ,declarations ,target ,traits)
       ,@body)))
