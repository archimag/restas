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
  
