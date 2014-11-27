;;;; request.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.wookie)

(defmethod restas:get-parameters ((request wookie:request))
  (let ((params (gethash :get (wookie:request-plugin-data request))))
    (if params
        (hash-table-alist params)
        nil)))


(defmethod restas:post-parameters ((request wookie:request))
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

(defmethod restas:cookies-in ((request wookie:request))
  (let ((params (gethash :cookie (wookie:request-plugin-data request))))
    (if params
        (hash-table-alist params)
        nil)))

(defmethod restas:query-string ((request wookie:request))
  (puri:uri-query (wookie:request-uri request)))

(defmethod restas:request-method ((request wookie:request))
  (wookie:request-method request))

(defmethod restas:request-uri ((request wookie:request))
  (puri:render-uri (wookie:request-uri request) nil))

(defmethod restas:server-protocol ((request wookie:request))
  (format nil
          "~A/~A"
          (if (typep *listener* 'wookie:ssl-listener)
              "https"
              "http")
          (http-parse:http-version (wookie:request-http request))))

(defmethod restas:headers-in ((request wookie:request))
  (alexandria:plist-alist (wookie:request-headers request)))

(defmethod restas:remote-address ((request wookie:request))
  nil)

(defmethod restas:remote-port ((request wookie:request))
  nil)

(defmethod restas:script-name ((request wookie:request))
  (puri:uri-path (wookie:request-uri request)))

(defmethod restas:raw-post-data ((request wookie:request))
  (http-parse:http-body (wookie:request-http request)))

(defmethod restas:request-listener ((request wookie:request))
  *listener*)
