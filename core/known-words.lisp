
(in-package #:restas)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +known-words+
    '(;; headers including WebDAV and some de facto standard headers
      "Accept"
      "Accept-Charset"
      "Accept-Encoding"
      "Accept-Language"
      "Accept-Ranges"
      "Age"
      "Allow"
      "Authorization"
      "Cache-Control"
      "Connection"
      "Content-Encoding"
      "Content-Language"
      "Content-Length"
      "Content-Location"
      "Content-MD5"
      "Content-Range"
      "Content-Type"
      "DAV"
      "Date"
      "Depth"
      "Destination"
      "ETag"
      "Expect"
      "Expires"
      "From"
      "Host"
      "If"
      "If-Match"
      "If-Modified-Since"
      "If-None-Match"
      "If-Range"
      "If-Unmodified-Since"
      "Last-Modified"
      "Location"
      "Lock-Token"
      "Max-Forwards"
      "Overwrite"
      "Pragma"
      "Proxy-Authenticate"
      "Proxy-Authorization"
      "Range"
      "Referer"
      "Retry-After"
      "Server"
      "TE"
      "TimeOut"
      "Trailer"
      "Transfer-Encoding"
      "Upgrade"
      "User-Agent"
      "Vary"
      "Via"
      "WWW-Authenticate"
      "Warning"
      ;; methods including WebDAV
      "CONNECT"
      "COPY"
      "DELETE"
      "GET"
      "HEAD"
      "LOCK"
      "MKCOL"
      "MOVE"
      "OPTIONS"
      "POST"
      "PROPFIND"
      "PROPPATCH"
      "PUT"
      "TRACE"
      "UNLOCK"
      ;; protocols
      "HTTP/1.1"
      "HTTP/1.0"
      ;; only a few and only the "preferred MIME names" - see
      ;; <http://www.iana.org/assignments/character-sets> for a
      ;; complete list
      "US-ASCII"
      "ISO-8859-1"
      "UTF-8"
      "UTF-16"
      "UTF-32BE"
      "UTF-32LE")
    "A list of words \(headers, methods, protocols, character sets)
that are typically seen in HTTP communication.  Mostly from RFC 2616,
but includes WebDAV stuff and other things as well."))

(defconstant +string-to-keyword-hash+
  (let ((hash (make-hash-table :test 'equal :size (length +known-words+))))
    (loop for word in +known-words+
          do (setf (gethash word hash) (make-keyword word nil)))
    hash)
  "A hash table which case-insensitively maps the strings from
+KNOWN-WORDS+ to keywords.")

(defconstant +keyword-to-string-hash+
  (let ((hash (make-hash-table :test 'eq :size (length +known-words+))))
    (loop for word in +known-words+
          do (setf (gethash (make-keyword word nil) hash)
                   (string-capitalize word)))
    hash)
  "A hash table which maps keywords derived from +KNOWN-WORDS+ to
capitalized strings.")

(defun as-keyword (string &key (destructivep t))
  "Converts the string STRING to a keyword where all characters are
uppercase or lowercase, taking into account the current readtable
case.  Might destructively modify STRING if DESTRUCTIVEP is true which
is the default.  \"Knows\" several HTTP header names and methods and
is optimized to not call INTERN for these."
  (or (gethash string +string-to-keyword-hash+)
      (make-keyword string destructivep)))

(defun as-capitalized-string (keyword)
  "Kind of the inverse of AS-KEYWORD.  Has essentially the same effect
as STRING-CAPITALIZE but is optimized for \"known\" keywords like
:CONTENT-LENGTH or :GET."
  (or (gethash keyword +keyword-to-string-hash+)
      (string-capitalize keyword)))
