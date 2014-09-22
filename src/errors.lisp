;;;; errors.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

(defun kill-all-debugging-threads ()
  "Used for destroy all debugging threads"
  (bt:with-lock-held (*debugging-threads-lock*)
    (dolist (thread *debugging-threads*)
      (when (ignore-errors
              (bt:destroy-thread thread)
              t)
        (setf *debugging-threads*
              (remove thread *debugging-threads*))))))

(defun debug-mode-on ()
  "Enable debug mode"
  (setf *catch-errors-p* nil))

(defun debug-mode-off (&optional (kill-debugging-threads t))
  "Turn off debug mode"
  (setf *catch-errors-p* t)
  (when kill-debugging-threads
    (kill-all-debugging-threads)))

(defun maybe-invoke-debugger (condition)
  (cond
    ((null *catch-errors-p*)
     (when (< (length *debugging-threads*) *max-debugging-threads*)
       (let ((thread (bt:current-thread)))
         (bt:with-lock-held (*debugging-threads-lock*)
           (push thread *debugging-threads*))
         (unwind-protect
              (invoke-debugger condition)
           (bt:with-lock-held (*debugging-threads-lock*)
             (setf *debugging-threads*
                   (remove thread *debugging-threads*)))))))
    (t (hunchentoot:maybe-invoke-debugger condition))))

(defun after-close-swank-connection (connection)
  "Turns off debug mode and destroy debugging threads after closing the connection with the swank-server"
  (declare (ignore connection))
  (debug-mode-off t))

#+swank (swank::add-hook swank::*connection-closed-hook*
                         'after-close-swank-connection)
