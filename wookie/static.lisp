;;;; static.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.wookie)

(defun handle-static-file (route request reply path)
  (declare (ignore request))
  (let* ((response (origin-response reply))
         (buffer (make-array 1024 :element-type '(unsigned-byte 8)))
         (stream (wookie:start-response response
                                        :headers (list :content-type (or (restas:mime-type path)
                                                                         (restas:content-type* reply)
                                                                         "application/octet-stream")))))
    (handler-case 
        (with-open-file (fstream path :element-type '(unsigned-byte 8))  
          (iter (for n = (read-sequence buffer fstream))
                (while (< 0 n))
                (write-sequence (subseq buffer 0 n) stream)
                (force-output stream)))
      (error (err)
        (log-route-error route err)))
    (wookie:finish-response response)))
  
  
  ;; (setf (restas:content-type* reply)
  ;;   (or (restas:mime-type path)
  ;;           (restas:content-type* reply)))
  ;; (send-reply reply
  ;;             (read-file-into-byte-vector path)))
