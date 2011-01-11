;;;; request.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

(defvar *request*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adopt from hunchentoot/request.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric headers-in (request)
  (:documentation "An alist of the incoming headers."))

(defgeneric request-method (request)
  (:documentation "The request method as a keyword."))

(defgeneric request-uri (request)
  (:documentation "The request URI as a string."))

(defgeneric server-protocol (request)
  (:documentation "The HTTP protocol as a keyword."))

(defgeneric remote-address (request)
  (:documentation "The IP address of the client that initiated this request."))

(defgeneric remote-port (request)
  (:documentation "The TCP port number of the client socket from which this request originated."))

(defgeneric cookies-in (request)
  (:documentation "An alist of the cookies sent by the client."))

(defgeneric get-parameters (request)
  (:documentation "An alist of the GET parameters sent by the client."))

(defgeneric post-parameters (request)
  (:documentation "An alist of the POST parameters sent by the client."))

(defgeneric script-name (request)
  (:documentation "The URI requested by the client without the query string."))

(defgeneric query-string (request)
  (:documentation "The query string of this request."))

(defgeneric raw-post-data (request)
  (:documentation "The raw string sent as the body of a
POST request, populated only if not a multipart/form-data request."))


(defgeneric header-in (name request)
  (:documentation "Returns the incoming header with name NAME.  NAME
can be a keyword \(recommended) or a string.")
  (:method (name request)
   (cdr (assoc name (headers-in request)))))

;;;; aux

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

(defun header-in* (name &optional (request *request*))
  "Returns the incoming header with name NAME.  NAME can be a keyword
\(recommended) or a string."
  (header-in name request))

(defun remote-addr* (&optional (request *request*))
  "Returns the address the current request originated from."
  (remote-addr request))

(defun remote-port* (&optional (request *request*))
  "Returns the port the current request originated from."
  (remote-port request))

(defun real-remote-addr (&optional (request *request*))
  "Returns the 'X-Forwarded-For' incoming http header as the
second value in the form of a list of IP addresses and the first
element of this list as the first value if this header exists.
Otherwise returns the value of REMOTE-ADDR as the only value."
  (let ((x-forwarded-for (header-in :x-forwarded-for request)))
    (cond (x-forwarded-for (let ((addresses (split "\\s*,\\s*" x-forwarded-for)))
                             (values (first addresses) addresses)))
          (t (remote-addr request)))))

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

(defun handle-if-modified-since (time &optional (request *request*))
  "Handles the 'If-Modified-Since' header of REQUEST.  The date string
is compared to the one generated from the supplied universal time
TIME."
  (let ((if-modified-since (header-in :if-modified-since request))
        (time-string (rfc-1123-date time)))
    ;; simple string comparison is sufficient; see RFC 2616 14.25
    (when (and if-modified-since
               (equal if-modified-since time-string))
      (setf (return-code*) +http-not-modified+)
      (abort-request-handler))
    (values)))
