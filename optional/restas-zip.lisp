;;;; restas-zip.lisp

(in-package #:restas.optional)

(defun/export write-string-into-gzip-file (string path)
  (with-open-file (ostream
                   path
                   :element-type '(unsigned-byte 8)
                   :direction :output
                   :if-exists :supersede)
    (salza2:with-compressor (compressor 'salza2:gzip-compressor
                                        :callback (salza2:make-stream-output-callback ostream))
      (salza2:compress-octet-vector (string-to-octets string)  
                                    compressor))))

(defun/export read-gzip-file-into-string (path)
  (octets-to-string (with-open-file (in path :element-type '(unsigned-byte 8))
                      (zip:skip-gzip-header in)
                      (flex:with-output-to-sequence (out)
                        (zip:inflate in out)))))
