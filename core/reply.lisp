;;;; reply.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reply protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric headers-out (reply)
  (:documentation "Returns an alist of the outgoing headers associated with the REPLY object reply."))

(defgeneric header-out (name reply)
  (:documentation "Returns the current value of the outgoing http header named NAME."))

(defgeneric (setf header-out) (new-value name reply)
  (:documentation "Changes the current value of the outgoing http header named NAME"))

(defgeneric return-code (reply)
  (:documentation "Get the http return code of reply. The return code of each REPLY object is initially set to +HTTP-OK+."))

(defgeneric (setf return-code) (newvalue reply)
  (:documentation "Set the http return code of reply."))

;; TODO

(defgeneric cookies-out (reply)
  (:documentation "Return an alist of the outgoing cookies associated with the REPLY object reply."))

(defgeneric (setf cookies-out) (newvalue reply)
  (:documentation "Set an alist of the outgoing cookies associated with the REPLY object reply."))

(defgeneric reply-external-format (reply)
  (:documentation "Get the external format of reply which is used for character output."))

(defgeneric (setf reply-external-format) (newvalue reply)
  (:documentation "Set the external format of reply which is used for character output."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reply interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun headers-out* (&optional (reply *reply*))
  "Returns an alist of the outgoing headers associated with the
REPLY object REPLY."
  (headers-out reply))

(defun header-out-set-p (name &optional (reply *reply*))
  "Returns a true value if the outgoing http header named NAME has
been specified already.  NAME should be a keyword or a string."
  (assoc* name (headers-out reply)))

(defun header-out* (name &optional (reply *reply*))
  "Returns the current value of the outgoing http header named NAME.
NAME should be a keyword or a string."
  (cdr (assoc name (headers-out reply))))

(defun (setf header-out*) (new-value name &optional (reply *reply*))
  "Changes the current value of the outgoing http header named NAME"
  (setf (header-out name reply) new-value))

(defun cookies-out* (&optional (reply *reply*))
  "Returns an alist of the outgoing cookies associated with the
REPLY object REPLY."
  (cookies-out reply))

(defun (setf cookies-out*) (new-value &optional (reply *reply*))
  "Sets the alist of the outgoing cookies associated with the REPLY
object REPLY."
  (setf (cookies-out reply) new-value))

(defun content-type* (&optional (reply *reply*))
  "The outgoing 'Content-Type' http header of REPLY."
  (header-out :content-type reply))

(defun (setf content-type*) (new-value &optional (reply *reply*))
  "Sets the outgoing 'Content-Type' http header of REPLY."
  (setf (header-out :content-type reply) new-value))

(defun content-length* (&optional (reply *reply*))
  "The outgoing 'Content-Length' http header of REPLY."
  (header-out :content-length reply))

(defun (setf content-length*) (new-value &optional (reply *reply*))
  "Sets the outgoing 'Content-Length' http header of REPLY."
  (setf (header-out :content-length reply) new-value))

(defun return-code* (&optional (reply *reply*))
  "The http return code of REPLY.  The return codes Hunchentoot can
handle are defined in specials.lisp."
  (return-code reply))

(defun (setf return-code*) (new-value &optional (reply *reply*))
  "Sets the http return code of REPLY."
  (setf (return-code reply) new-value))

(defun reply-external-format* (&optional (reply *reply*))
  "The external format of REPLY which is used for character output."
  (reply-external-format reply))

(defun (setf reply-external-format*) (new-value &optional (reply *reply*))
  "Sets the external format of REPLY."
  (setf (reply-external-format reply) new-value))


(defun cookie-out (name &optional (reply *reply*))
  "Returns the current value of the outgoing cookie named
NAME. Search is case-sensitive."
  (cdr (assoc name (cookies-out reply) :test #'string=)))


(defun ssl-p ()
  (listener-ssl-p (request-listener restas:*request*)))

(defun starts-with-scheme-p (string)
  "Checks whether the string STRING represents a URL which starts with
a scheme, i.e. something like 'https://' or 'mailto:'."
  (loop with scheme-char-seen-p = nil
        for c across string
        when (or (char-not-greaterp #\a c #\z)
                 (digit-char-p c)
                 (member c '(#\+ #\- #\.) :test #'char=))
        do (setq scheme-char-seen-p t)
        else return (and scheme-char-seen-p
                         (char= c #\:))))

(defun redirect-url (target &key (host (host *request*))
                          port
                          (protocol (if (ssl-p) :https :http))
                          (code +http-moved-temporarily+))
  "Redirects the browser to TARGET which should be a string.  If
TARGET is a full URL starting with a scheme, HOST, PORT and PROTOCOL
are ignored.  Otherwise, TARGET should denote the path part of a URL,
PROTOCOL must be one of the keywords :HTTP or :HTTPS, and the URL to
redirect to will be constructed from HOST, PORT, PROTOCOL, and TARGET.
Adds a session ID if ADD-SESSION-ID is true.  If CODE is a 3xx
redirection code, it will be sent as status code."
  (check-type code (integer 300 399))
  (let ((url (if (starts-with-scheme-p target)
               target
               (format nil "~A://~A~@[:~A~]~A"
                       (ecase protocol
                         ((:http) "http")
                         ((:https) "https"))
                       (if port                         
                         (first (ppcre:split ":" (or host "")))
                         host)
                       port target))))
    (setf (header-out* :location) url
          (return-code*) code)
    (abort-route-handler nil)))
