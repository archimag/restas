;;;; request.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; request protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-parameters (request)
  (:documentation "Returns an alist of all GET parameters (as provided via the request URI). The car of each element of this list is the parameter's name while the cdr is its value (as a string). The elements of this list are in the same order as they were within the request URI."))

(defgeneric post-parameters (request)
  (:documentation "Returns an alist of all POST parameters (as provided via the request's body). The car of each element of this list is the parameter's name while the cdr is its value. The elements of this list are in the same order as they were within the request's body."))

(defgeneric cookies-in (request)
  (:documentation "Returns an alist of all cookies associated with the REQUEST object request."))

(defgeneric query-string (request)
  (:documentation "Returns the query string of the REQUEST object request. That's the part behind the question mark (i.e. the GET parameters)."))

(defgeneric request-method (request)
  (:documentation "Returns the request method as a Lisp keyword."))

(defgeneric request-uri (request)
  (:documentation "Returns the request URI."))

(defgeneric server-protocol (request)
  (:documentation "Returns the request protocol as a Lisp keyword."))

(defgeneric headers-in (request)
  (:documentation "Returns the incoming header with name name. name can be a keyword (recommended) or a string."))

(defgeneric remote-address (request)
  (:documentation "Returns the address the current request originated from."))

(defgeneric remote-port (request)
  (:documentation "Returns the port the current request originated from."))

(defgeneric script-name (request)
  (:documentation "Returns the file name of the REQUEST object request. That's the requested URI without the query string (i.e the GET parameters)."))

(defgeneric raw-post-data (request)
  (:documentation "Returns the content sent by the client in the request body if there was any (unless the content type was multipart/form-data in which case NIL is returned)."))

(defgeneric request-listener (request)
  (:documentation "The listener which created this request object."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; request interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun script-name* (&optional (request *request*))
  "Returns the file name of the REQUEST object REQUEST. That's the
requested URI without the query string \(i.e the GET parameters)."
  (script-name request))

(defun query-string* (&optional (request *request*))
  "Returns the query string of the REQUEST object REQUEST. That's
the part behind the question mark \(i.e. the GET parameters)."
  (query-string request))

(defun get-parameters* (&optional (request *request*))
  "Returns an alist of the GET parameters associated with the REQUEST
object REQUEST."
  (get-parameters request))

(defun post-parameters* (&optional (request *request*))
  "Returns an alist of the POST parameters associated with the REQUEST
object REQUEST."
  (post-parameters request))

(defun headers-in* (&optional (request *request*))
  "Returns an alist of the incoming headers associated with the
REQUEST object REQUEST."
  (headers-in request))

(defun cookies-in* (&optional (request *request*))
  "Returns an alist of all cookies associated with the REQUEST object
REQUEST."
  (cookies-in request))

(defgeneric header-in (name request)
  (:documentation "Returns the incoming header with name NAME.  NAME
can be a keyword \(recommended) or a string.")
  (:method (name request)
   (cdr (assoc* name (headers-in request)))))

(defun header-in* (name &optional (request *request*))
  "Returns the incoming header with name NAME.  NAME can be a keyword
\(recommended) or a string."
  (header-in name request))

(defun authorization (&optional (request *request*))
  "Returns as two values the user and password \(if any) as encoded in
the 'AUTHORIZATION' header.  Returns NIL if there is no such header."
  (let* ((authorization (header-in :authorization request))
         (start (and authorization
                     (> (length authorization) 5)
                     (string-equal "Basic" authorization :end2 5)
                     (ppcre:scan "\\S" authorization :start 5))))
    (when start
      (destructuring-bind (&optional user password)
          (ppcre:split ":" (base64:base64-string-to-string (subseq authorization start)))
        (values user password)))))

(defun remote-address* (&optional (request *request*))
  "Returns the address the current request originated from."
  (remote-address request))

(defun remote-port* (&optional (request *request*))
  "Returns the port the current request originated from."
  (remote-port request))

(defun real-remote-address (&optional (request *request*))
  "Returns the 'X-Forwarded-For' incoming http header as the
second value in the form of a list of IP addresses and the first
element of this list as the first value if this header exists.
Otherwise returns the value of REMOTE-ADDR as the only value."
  (let ((x-forwarded-for (header-in :x-forwarded-for request)))
    (cond (x-forwarded-for (let ((addresses (ppcre:split "\\s*,\\s*" x-forwarded-for)))
                             (values (first addresses) addresses)))
          (t (remote-address request)))))

(defun host (&optional (request *request*))
  "Returns the 'Host' incoming http header value."
  (header-in :host request))

(defun request-uri* (&optional (request *request*))
  "Returns the request URI."
  (request-uri request))

(defun request-method* (&optional (request *request*))
  "Returns the request method as a Lisp keyword."
  (request-method request))

(defun server-protocol* (&optional (request *request*))
  "Returns the request protocol as a Lisp keyword."
  (server-protocol request))

(defun user-agent (&optional (request *request*))
  "Returns the 'User-Agent' http header."
  (header-in :user-agent request))

(defun cookie-in (name &optional (request *request*))
  "Returns the cookie with the name NAME \(a string) as sent by the
browser - or NIL if there is none."
  (cdr (assoc name (cookies-in request) :test #'string=)))

(defun referer (&optional (request *request*))
  "Returns the 'Referer' \(sic!) http header."
  (header-in :referer request))

(defun get-parameter (name &optional (request *request*))
  "Returns the GET parameter with name NAME \(a string) - or NIL if
there is none.  Search is case-sensitive."
  (cdr (assoc name (get-parameters request) :test #'string=)))

(defun post-parameter (name &optional (request *request*))
  "Returns the POST parameter with name NAME \(a string) - or NIL if
there is none.  Search is case-sensitive."
  (cdr (assoc name (post-parameters request) :test #'string=)))

(defun parameter (name &optional (request *request*))
  "Returns the GET or the POST parameter with name NAME \(a string) -
or NIL if there is none.  If both a GET and a POST parameter with the
same name exist the GET parameter is returned.  Search is
case-sensitive."
  (or (get-parameter name request)
      (post-parameter name request)))

(defun binary-post-data (&optional (request *request*))
  "Returns the content sent by the client in the request body if there
was any (unless the content type was multipart/form-data in which case
NIL is returned)."
  (raw-post-data request))

(defun text-post-data (&optional (encoding :utf-8) (request *request*))
  "Returns the content sent by the client in the request body if there
was any (unless the content type was multipart/form-data in which case
NIL is returned)."
  (babel:octets-to-string (raw-post-data request) :encoding encoding))
    
  
