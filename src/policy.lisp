;;;; policy.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage restas.policy.internal
  (:export #:rest))

(in-package :restas)

(defun make-internal-function (name policy-var interface-method lambda-list
                               &optional documentation)
  (let (simple-args optional-args key-args rest-args)
    (iter (for item in lambda-list)
          (cond
            ((or (eql item '&optional) optional-args)
             (push item optional-args))
            ((or (eql item '&rest) rest-args)
             (push item rest-args))
            ((member item '(&key &allow-other-keys))
             (push item key-args))
            (key-args
             (push (intern (string item) :keyword) key-args)
             (push item key-args))
            (t
             (push item simple-args))))

    (setf simple-args
          (nreverse simple-args))
    (setf optional-args
          (cdr (nreverse optional-args)))
    (setf key-args
          (cdr (nreverse key-args)))
    (setf rest-args
          (cdr rest-args))

    (let ((defun-expr
           (cond
             (optional-args
              `(defun ,name ,lambda-list
                 ,documentation
                 (,interface-method ,policy-var ,@simple-args ,@optional-args)))
             (rest-args
              `(defun ,name ,lambda-list
                 ,documentation
                 (apply ',interface-method ,policy-var
                        ,@simple-args ,(car rest-args))))
             ((and key-args (member '&allow-other-keys key-args))
              `(defun ,name (,@simple-args &rest restas.policy.internal:rest
                             &key &allow-other-keys)
                 (apply ',interface-method ,policy-var
                        ,@simple-args restas.policy.internal:rest)))
             (key-args
              `(defun ,name ,lambda-list
                 ,documentation
                 (,interface-method ,policy-var ,@simple-args ,@key-args)))
             (t
              `(defun ,name ,lambda-list
                 ,documentation
                 (,interface-method ,policy-var ,@simple-args))))))
      (eval defun-expr))))

(defun %define-policy (name methods &key
                                      interface-package
                                      (interface-method-template "~A")
                                      internal-package
                                      (internal-function-template "~A"))
  (let* ((%interface-package (if interface-package
                                 (or (find-package interface-package)
                                     (make-package interface-package))
                                 *package*))
         (%internal-package (if internal-package
                                (or (find-package internal-package)
                                    (make-package internal-package))
                                *package*))
         (%obj-symbol (intern (format nil "*~A*" (string name))
                              %internal-package)))

    (when internal-package
      (export %obj-symbol %internal-package))
    (proclaim (list 'special %obj-symbol))
    (unless (boundp %obj-symbol)
      (setf (symbol-value %obj-symbol) nil))

    (iter (for (method lambda-list docstring) in methods)
          (for interface-name = (intern (format nil interface-method-template
                                                (string method))
                                        %interface-package))
          (for internal-name = (intern (format nil internal-function-template
                                               (string method))
                                       %internal-package))
          (when (eql interface-name internal-name)
            (error "Internal and external names must be different"))

          (when interface-package
            (export interface-name %interface-package))
          (when internal-package
            (export internal-name %internal-package))

          (ensure-generic-function interface-name
                                   :lambda-list (cons name lambda-list)
                                   :documentation docstring)

	  (make-internal-function internal-name %obj-symbol interface-name
				  lambda-list docstring))))


(defun %parse-define-policy-options (options &aux methods keys values)
  (iter
    (for (head . tail) on options)
    (if (string= :define-method (car head))
	(push (cdr head) methods)
	(destructuring-bind (key val) head
	  (let ((type (second
		       (assoc key
			      '((:interface-package quote)
				(:interface-method-template t)
				(:internal-package quote)
				(:internal-function-template t))))))
	    (assert type () "Unknown argument ~s." key)
	    (assert (not (member key keys)) () "Duplicate argument ~s." key)
	    (push key keys)
	    (push (case type
		    (quote (list 'quote val))
		    (t val))
		  values))))
    (finally
     (return (values methods (mapcan #'list keys values))))))

(defmacro define-policy (name &body body)
  (multiple-value-bind (methods arguments) (%parse-define-policy-options body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%define-policy ',name ',methods ,@arguments))))
