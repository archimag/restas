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
  ;;(hunchentoot:cookies-in request)
  nil
  )

(defmethod restas:query-string ((request wookie:request))
  (puri:uri-query (wookie:request-uri request)))

(defmethod restas:request-method ((request wookie:request))
  (wookie:request-method request))

(defmethod restas:request-uri ((request wookie:request))
  (wookie:request-uri request))

(defmethod restas:server-protocol ((request wookie:request))
  ;;(hunchentoot:server-protocol request)
  :http
  )

(defmethod restas:headers-in ((request wookie:request))
  (alexandria:plist-alist (wookie:request-headers request)))

(defmethod restas:remote-address ((request wookie:request))
  ;;(hunchentoot:remote-addr request)
  "127.0.0.1"
  )

(defmethod restas:remote-port ((request wookie:request))
  ;;(hunchentoot:remote-port request)
  8080
  )

(defmethod restas:script-name ((request wookie:request))
  (puri:uri-path (wookie:request-uri request)))

(defmethod restas:raw-post-data ((request wookie:request) &key &allow-other-keys)
  nil)

;; (defmethod restas:raw-post-data (request &key encoding force-text force-binary &allow-other-keys)
;;   nil)
  ;; (hunchentoot:raw-post-data :request request
  ;;                            :external-format (encoding-hunchentoot-external-format encoding)
  ;;                            :force-text force-text
  ;;                            :force-binary force-binary))



(defmethod restas:request-listener ((request wookie:request))
  *listener*)