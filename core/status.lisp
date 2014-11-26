;;;; status.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas)

(defparameter *status-page-template*
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>~A</title>
</head><body>
<h1>~A</h1>
~A
<hr>
<address>~A</address>
</body></html>")

(defun status-page-title (code)
  (format nil "~A ~A" code (gethash code *http-reason-phrase-map*)))

(defun status-page-reason (code)
  (gethash code *http-reason-phrase-map*))

(defun status-page-message (code)
  (cond
    ((= code +http-not-found+)
     (format nil
             "<p>The requested URL ~A was not found on this server.</p>"
             (request-uri*)))
    ((= code +http-internal-server-error+)
     "<p>The server encountered an internal error or misconfiguration and was unable to complete your request.</p>")
    (t
     "")))

(defun status-page-server-info ()
  (format nil
          "RESTAS at ~A"
          (host)))
      
(defun restas-status-message (code)
  (format nil
          *status-page-template*
          (status-page-title code)
          (status-page-reason code)
          (status-page-message code)
          (status-page-server-info)))
