;;;; restas-swank.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:restas.swank
  (:use #:cl #:iter)
  (:export #:inspect-module))

(in-package #:restas.swank)

(defstruct %restas-route
  symbol)

(defmethod swank:emacs-inspect ((route %restas-route))
  (let ((symbol (%restas-route-symbol route)))
    `("" "Symbol       = " (:value ,symbol ,(symbol-name symbol)) (:newline)
         "Template     = /" ,(get symbol :template)  (:newline)
         "Content-Type = " ,(or (get symbol :content-type) "text/html") (:newline)
         "Method       = " ,(symbol-name (or (get symbol :method) :get)) (:newline)
         "Handler      = " (:value ,(symbol-function symbol) ))))


(defmethod swank:object-source-location ((route %restas-route))
  (swank:object-source-location (symbol-function (%restas-route-symbol route))))

(defmethod swank:object-source-location ((route restas::route))
  (swank:object-source-location (symbol-function (slot-value route 'symbol))))


(defstruct %restas-module
  package)

(defun svalue (name module)
  (symbol-value (find-symbol name module)))

(defun module-routes (module)
  (sort (iter (for symbol in-package (svalue restas::+routes-symbol+ module))
              (collect symbol))
        #'string<))

(defun module-submodules (module)
  (sort (iter (for (symbol submodule) in-hashtable (svalue restas::+submodules-symbol+ module))
              (collect (cons symbol
                             submodule)))
        #'string<
        :key #'car))


(defun find-module-method (module generic-method)
  (let ((package (find-package module)))
    (iter (for method in (closer-mop:generic-function-methods generic-method))
          (finding method
                   such-that (let ((specializer (first (closer-mop:method-specializers method))))
                               (and (typep specializer
                                           'closer-mop:eql-specializer)
                                    (eql (closer-mop:eql-specializer-object specializer)
                                         package)))))))


(defmethod swank:emacs-inspect ((module %restas-module))
  (let ((package (find-package (%restas-module-package module))))
    `("" "Package:        " (:value ,package ,(package-name package)) (:newline)
         "Initialization: " ,(let ((m (find-module-method package #'restas::initialize-module-instance)))
                                  (if m
                                      (list :value m)
                                      "None"))
         (:newline)
         "Finalization:   " ,(let ((m (find-module-method package #'restas::finalize-module-instance)))
                                  (if m
                                      (list :value m)
                                      "None"))
         (:newline) (:newline)
         "Routes: "
         (:newline)
         "--------------------------------------------------"
         (:newline)
         ,@(iter (for route in (module-routes package))
                 (collect (let ((route-symbol (find-symbol (symbol-name route)
                                                           package)))
                            (list :value
                                  (make-%restas-route :symbol route-symbol)
                                  (symbol-name route-symbol))))
                 (collect '(:newline)))
         (:newline)         
         "Submodules: "
         (:newline)
         "--------------------------------------------------"
         (:newline)
         ,@(or (iter (for (symbol . submodule) in (module-submodules package))
                     (collect (list :value
                                    submodule
                                    (symbol-name symbol)))
                     (collect '(:newline)))
               '("None")))))


(defvar *saved-global-context* nil)

(swank:defslimefun restore-global-context ()
  (iter (for (symbol . value) in *saved-global-context*)
        (setf (symbol-value symbol)
              value))
  (setf restas::*submodule* nil)
  (setf *saved-global-context* nil))

(defmethod swank:emacs-inspect ((submodule restas:submodule))
  (flet ((module-link (module)
           (if module
               (list :value
                     (make-%restas-module :package module)
                     (package-name module))
               "NIL")))
    `("" "Module: " ,(module-link (slot-value submodule 'restas::module))
         (:newline)
         (:newline)
         ,@(let ((package  (slot-value submodule 'restas::module)))
               `("" ;;"Module:         " (:value ,package ,(package-name package)) (:newline)
                    "Initialization: " ,(let ((m (find-module-method package #'restas::initialize-module-instance)))
                                             (if m
                                                 (list :value m)
                                                 "None"))
                    (:newline)
                    "Finalization:   " ,(let ((m (find-module-method package #'restas::finalize-module-instance)))
                                             (if m
                                                 (list :value m)
                                                 "None"))
                    (:newline) (:newline)
                    "Routes: "
                    (:newline)
                    "--------------------------------------------------"
                    (:newline)
                    ,@(iter (for route in (module-routes package))
                            (collect (let ((route-symbol (find-symbol (symbol-name route)
                                                                      package)))
                                       (list :value
                                             (make-%restas-route :symbol route-symbol)
                                             (symbol-name route-symbol))))
                            (collect '(:newline)))
                    (:newline)))

         "Submodules: "
         (:newline)
         "--------------------------------------------------"
         (:newline)
         ,@(or (iter (for sub in (restas::submodule-submodules submodule))
                     (collect (list :value
                                    sub
                                    (symbol-name (restas:submodule-symbol sub ))))
                     (collect '(:newline)))
               '("None" (:newline)))
         (:newline)
         ;;(:newline)

         "Context:"
         (:newline)
         "--------------------------------------------------"
         (:newline)
         ,@(or (let* ((context (slot-value submodule 'restas::context))
                      (max (iter (for symbol in (restas::context-vars context))
                                 (maximize (length (write-to-string symbol))))))
                 (setf restas::*submodule* submodule)
                 (iter (for symbol in (restas::context-vars context))
                       (for value in (restas::context-values context))
                       (collect (list :value symbol))
                       (collect (make-string (- max
                                                (length (write-to-string symbol)))
                                             :initial-element #\Space))
                       
                       (collect " = ")
                       (collect (list :value value))
                       (collect '(:newline))))
               '("None"))
         (:newline)
         (:action "Use this context as a global"
                  ,#'(lambda ()
                           (restore-global-context)
                           (setf *saved-global-context*
                                 (let ((context (slot-value submodule 'restas::context)))
                                   (iter (for symbol in (restas::context-vars context))
                                         (for value in (restas::context-values context))
                                         (collect (cons symbol
                                                        (symbol-value symbol)))
                                         (setf (symbol-value symbol)
                                               value))))))
         (:newline)
         ,@(if *saved-global-context*
               (list (list :action
                           "Restore global context"
                           'restore-global-context))))))

(defstruct %restas-vhost
  vhost)

(defstruct %restas-vhost-list
  vhosts)

(defun %restas-vhost-string (%vhost)
  (let ((hostname (or (restas::vhost-hostname (%restas-vhost-vhost %vhost))
                      "*"))
        (port (restas::vhost-port (%restas-vhost-vhost %vhost))))
    (if port
        (format nil "http://~A:~A/" hostname port)
        (format nil "http://~A/" hostname))))


(defmethod swank:emacs-inspect ((vhosts %restas-vhost-list))
  `("" (:newline)
       "List of Virtual Host" (:newline)
       "--------------------------------------------------" (:newline)
       ,@(iter (for %vhost in (%restas-vhost-list-vhosts vhosts))
               (collect (list :value
                              %vhost
                              (%restas-vhost-string %vhost)))
               (collect '(:newline)))))

(defmethod swank:emacs-inspect ((vhost %restas-vhost))
  `("" "Host:     " ,(%restas-vhost-string vhost) (:newline)
       "Site Map: " (:value ,(slot-value (%restas-vhost-vhost vhost) 'restas::mapper)) (:newline)
       (:newline)
       "Submodules: " (:newline)
       "--------------------------------------------------" (:newline)
       ,@(or (iter (for submodule in (slot-value (%restas-vhost-vhost vhost) 'restas::modules))
                   (collect (list :value
                                  submodule
                                  (package-name (slot-value submodule 'restas::module))
                                  ))
                   (collect '(:newline)))
             '("None"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; slime functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(swank:defslimefun inspect-module (package)
  (swank:inspect-object (make-%restas-module :package package)))

(swank:defslimefun inspect-vhosts ()
  (swank:inspect-object
   (make-%restas-vhost-list
    :vhosts (iter (for vhost in restas::*vhosts*)
                  (collect (make-%restas-vhost :vhost vhost))))))
