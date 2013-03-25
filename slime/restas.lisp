;;;; restas-swank.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:restas.swank
  (:use #:cl #:iter)
  (:export #:inspect-vhosts))

(in-package #:restas.swank)

;;;; $route

(defclass $route ()
  ((route :initarg :route :reader $route-route)))

(defmethod swank:emacs-inspect (($route $route) &aux (route ($route-route $route)))
  (let* ((symbol (restas:route-symbol route))
         (package (symbol-package symbol))
         (info (gethash symbol (gethash :routes (gethash package restas::*pkgmodules-traits*)))))
    `("" "Symbol       = "  (:value ,symbol ,(symbol-name symbol)) (:newline)
         "Template     = "  ,(gethash :template info) (:newline)
         "Content-Type = "  ,(gethash :content-type info "text/html") (:newline)
         "Method       = " ,(symbol-name (gethash :method info :get)) (:newline)
         "Handler      = "  (:value ,(symbol-function symbol)))))

(defmethod swank:object-source-location ((route $route))
  (swank:object-source-location ($route-route route)))

(defmethod swank:object-source-location ((route restas::route))
  (swank:object-source-location (symbol-function (slot-value route 'symbol))))

;;;; $module

(defclass $module ()
  ((module :initarg :module :reader $module-module)))

(defun $module-package (module)
  (find-package (slot-value ($module-module module) 'package)))

(defmethod swank:object-source-location (($module $module))
  (swank:object-source-location (restas::module-symbol ($module-module $module))))

(defun find-module-method (module generic-method)
  (let ((package ($module-package module)))
    (iter (for method in (closer-mop:generic-function-methods generic-method))
          (finding method
                   such-that (let ((specializer (first (closer-mop:method-specializers method))))
                               (and (typep specializer
                                           'closer-mop:eql-specializer)
                                    (eql (closer-mop:eql-specializer-object specializer)
                                         package)))))))

(defvar *saved-global-context* nil)

(swank:defslimefun restore-global-context ()
  (iter (for (symbol . value) in *saved-global-context*)
        (setf (symbol-value symbol)
              value))
  (setf restas:*module* nil)
  (setf *saved-global-context* nil))


(defmethod swank:emacs-inspect (($module $module) &aux (module ($module-module $module)))
  `("" "Package: " ,(list :value (slot-value module 'restas::package))
       (:newline)
       (:newline)

       ;; initialization/finalization
       "Initialization: " ,(let ((m (find-module-method $module #'restas:initialize-module-instance)))
                                (if m
                                    (list :value m)
                                    "None"))
       (:newline)
       "Finalization:   " ,(let ((m (find-module-method $module #'restas:finalize-module-instance)))
                                (if m
                                    (list :value m)
                                    "None"))
       (:newline) (:newline)

       ;; routes
       "Routes: "
       (:newline)
       "--------------------------------------------------"
       (:newline)
       ,@(iter (for route in (sort (alexandria:hash-table-values (slot-value module 'restas::routes))
                                   #'string<
                                   :key #'restas:route-symbol))
               (when (eql (restas::route-module route) module)
                 (collect (list :value
                                (make-instance '$route :route route)
                                (string-downcase (symbol-name (restas:route-symbol route)))))
                 (collect '(:newline))))
       (:newline)

       ;; mounted modules
       "Mounted modules: "
       (:newline)
       "--------------------------------------------------"
       (:newline)
       ,@(or (iter (for child in (sort (alexandria:hash-table-values (slot-value module 'restas::children))
                                       #'string<
                                       :key #'restas::module-symbol))
                   (collect (list :value
                                  (make-instance '$module :module child)
                                  (string-downcase (symbol-name (restas::module-symbol child)))))
                   (collect '(:newline)))
             '("None" (:newline)))
       (:newline)
       (:newline)

       ;; context
       "Context:"
       (:newline)
       "--------------------------------------------------"
       (:newline)
       ,@(or (let* ((context (slot-value module 'restas::context))
                    (max (iter (for symbol in (restas::context-all-vars context))
                               (maximize (length (write-to-string symbol))))))
               ;;(setf restas::*submodule* submodule)
               (iter (for symbol in (restas::context-all-vars context))
                     (for value in (restas::context-all-values context))
                     (collect (list :value symbol))
                     (collect (make-string (- max
                                              (length (write-to-string symbol)))
                                           :initial-element #\Space))
       
                     (collect " = ")
                     (collect (list :value value))
                     (collect '(:newline))))
             '("None" (:newline)))
       (:newline)
       (:action "Use this context as a global"
                ,#'(lambda ()
                     (restore-global-context)
                     (setf restas:*module* module)
                     (setf *saved-global-context*
                           (let ((context (slot-value module 'restas::context)))
                             (iter (for symbol in (restas::context-all-vars context))
                                   (for value in (restas::context-all-values context))
                                   (collect (cons symbol
                                                  (symbol-value symbol)))
                                   (setf (symbol-value symbol)
                                         value))))))
       (:newline)
       ,@(if *saved-global-context*
             (list (list :action
                         "Restore global context"
                         'restore-global-context)))))
  

;;;; %vhost


(defclass %vhost ()
  ((vhost :initarg :vhost :reader %vhost-vhost)))

(defun %vhost-string (%vhost &aux (vhost (%vhost-vhost %vhost)))
  (let ((hostname (or (restas::vhost-hostname vhost)
                      "*"))
        (port (restas::vhost-port vhost)))
    (if port
        (format nil "http://~A:~A/" hostname port)
        (format nil "http://~A/" hostname))))

(defmethod swank:emacs-inspect ((%vhost %vhost) &aux (vhost (%vhost-vhost %vhost)))
  `("" "Host:     " ,(%vhost-string %vhost) (:newline)
       "Site Map: " (:value ,(slot-value  vhost 'restas::mapper)) (:newline)
       (:newline)
       "Modules: " (:newline)
       "--------------------------------------------------" (:newline)
       ,@(or (iter (for child in (slot-value vhost 'restas::modules))
                   (collect (list :value
                                  (make-instance '$module :module child)
                                  (package-name (slot-value child 'package))
                                  ))
                   (collect '(:newline)))
             '("None"))))
  
;;; %vhosts

(defclass %vhosts () ())

(defun %vhost-list ()
  (iter (for vhost in restas::*vhosts*)
        (collect (make-instance '%vhost :vhost vhost))))

(defmethod swank:emacs-inspect ((vhosts %vhosts))
  `("" (:newline)
       "List of Virtual Hosts" (:newline)
       "--------------------------------------------------" (:newline)
       ,@(iter (for %vhost in (%vhost-list))
               (collect (list :value
                              %vhost
                              (%vhost-string %vhost)))
               (collect '(:newline)))))

(swank:defslimefun inspect-vhosts ()
  (swank:inspect-object (make-instance '%vhosts)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank::send-to-emacs
   `(:eval-no-wait
     "(defun restas-inspector ()
       (interactive)
       (slime-eval-async `(restas.swank:inspect-vhosts) 'slime-open-inspector))")))
