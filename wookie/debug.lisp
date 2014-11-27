;;;; debug.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.wookie)

(defvar *debug-mode* nil)

(defun debug-mode-on ()
  (setf *debug-mode* t))

(defun debug-mode-off ()
  (setf *debug-mode* nil))
  
(defun log-route-error (route err)
  (let* ((rsymbol (restas:route-symbol route))
         (pkg (symbol-package rsymbol))
         (name (symbol-name rsymbol))
         (status (nth-value 1 (find-symbol name pkg))))
    (log:error "Error while processing ~A~A~A route: ~A"
               (package-name pkg)
               (if (eql status :external)
                   ":"
                   "::")
               name
               err)))
