;;;; request.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.wookie)

(defmethod restas:request-get-parameters ((request wookie:request))
  (let ((params (gethash :get (wookie:request-plugin-data request))))
    (if params
        (hash-table-alist params)
        nil)))


(defmethod restas:request-post-parameters ((request wookie:request))
  (let* ((params nil)
         (pdata (wookie:request-plugin-data request))
         (post (gethash :post pdata))
         (multipart (gethash :multipart pdata))
         (form (getf multipart :hash-form ))
         (files (getf multipart :hash-file)))
    #|------------------------------------------------------------------------|#
    (when post
      (appendf params (hash-table-alist post)))
    #|------------------------------------------------------------------------|#
    (when form
      (appendf params (hash-table-alist form)))
    #|------------------------------------------------------------------------|#
    (when files
      (iter (for (key value) in-hashtable files)
            (push (cons key
                        (list (getf value :tmp-file)
                              (getf value :filename)
                              (getf value :mime-type)))
                  params)))
    #|------------------------------------------------------------------------|#
    params))

(defmethod restas:request-cookies-in ((request wookie:request))
  (let ((params (gethash :cookie (wookie:request-plugin-data request))))
    (if params
        (hash-table-alist params)
        nil)))

(defun request-uri (request)
  (let ((uri (wookie:request-uri request)))
    (if (typep uri 'puri:uri)
        uri
        (puri:parse-uri (quri:render-uri uri)))))

(defmethod restas:request-query-string ((request wookie:request))
  (puri:uri-query (request-uri request)))

(defmethod restas:request-request-method ((request wookie:request))
  (wookie:request-method request))

(defmethod restas:request-request-uri ((request wookie:request))
  (puri:render-uri (request-uri request) nil))

(defmethod restas:request-server-protocol ((request wookie:request))
  (format nil
          "~A/~A"
          (if (typep *listener* 'wookie:ssl-listener)
              "https"
              "http")
          (http-parse:http-version (wookie:request-http request))))

(defmethod restas:request-headers-in ((request wookie:request))
  (alexandria:plist-alist (wookie:request-headers request)))

(defmethod restas:request-remote-address ((request wookie:request))
  nil)

(defmethod restas:request-remote-port ((request wookie:request))
  nil)

(defmethod restas:request-script-name ((request wookie:request))
  (puri:uri-path (request-uri request)))

(defmethod restas:request-raw-post-data ((request wookie:request))
  (http-parse:http-body (wookie:request-http request)))

(defmethod restas:request-listener ((request wookie:request))
  *listener*)
