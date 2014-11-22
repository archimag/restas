;;;; util.lisp

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package #:restas)

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defgeneric assoc* (thing alist)
  (:documentation "Similar to CL:ASSOC, but 'does the right thing' if
THING is a string or a symbol.")
  (:method ((thing symbol) alist)
   (assoc thing alist :test #'eq))
  (:method ((thing string) alist)
   (assoc thing alist :test #'string-equal))
  (:method (thing alist)
   (assoc thing alist :test #'eql)))


(defun make-keyword (string destructivep)
  "Converts the string STRING to a keyword where all characters are
uppercase or lowercase, taking into account the current readtable
case.  Destructively modifies STRING if DESTRUCTIVEP is true."
  (intern (funcall
           (if destructivep
             (if (eq (readtable-case *readtable*) :upcase)
               #'nstring-upcase
               #'nstring-downcase)
             (if (eq (readtable-case *readtable*) :upcase)
               #'string-upcase
               #'string-downcase))
           string)
          :keyword))

(defconstant +day-names+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "The three-character names of the seven days of the week - needed
for cookie date format.")

(defconstant +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "The three-character names of the twelve months - needed for cookie
date format.")

(defun rfc-1123-date (&optional (time (get-universal-time)))
  "Generates a time string according to RFC 1123.  Default is current time."
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref +day-names+ day-of-week)
            date
            (svref +month-names+ (1- month))
            year
            hour
            minute
            second)))


(defun escape-for-html (string)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML output."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#039;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))

(defun url-encode (string &optional encoding)
  "URL-encodes a string using the external format EXTERNAL-FORMAT."
  (with-output-to-string (s)
    (loop for c across string
          for index from 0
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        ;; note that there's no comma in there - because of cookies
                        (find c "$-_.!*'()" :test #'char=))
                     (write-char c s))
                   (t (loop for octet across (babel:string-to-octets string
                                                                     :start index
                                                                     :end (1+ index)
                                                                     :encoding encoding)
                            do (format s "%~2,'0x" octet)))))))


(defmacro upgrade-vector (vector new-type &key converter)
  "Returns a vector with the same length and the same elements as
VECTOR \(a variable holding a vector) but having element type
NEW-TYPE.  If CONVERTER is not NIL, it should designate a function
which will be applied to each element of VECTOR before the result is
stored in the new vector.  The resulting vector will have a fill
pointer set to its end.

The macro also uses SETQ to store the new vector in VECTOR."
  `(setq ,vector
         (loop with length = (length ,vector)
               with new-vector = (make-array length
                                             :element-type ,new-type
                                             :fill-pointer length)
               for i below length
               do (setf (aref new-vector i) ,(if converter
                                               `(funcall ,converter (aref ,vector i))
                                               `(aref ,vector i)))
               finally (return new-vector))))

(defun url-decode (string &optional (external-format :utf-8))
  "Decodes a URL-encoded STRING which is assumed to be encoded using
the external format EXTERNAL-FORMAT."
  (when (zerop (length string))
    (return-from url-decode ""))
  (let ((vector (make-array (length string) :element-type '(unsigned-byte 8) :fill-pointer 0))
        (i 0)
        unicodep)
    (loop
      (unless (< i (length string))
        (return))
      (let ((char (aref string i)))
       (labels ((decode-hex (length)
                  (prog1
                      (parse-integer string :start i :end (+ i length) :radix 16)
                    (incf i length)))
                (push-integer (integer)
                  (vector-push integer vector))
                (peek ()
                  (aref string i))
                (advance ()
                  (setq char (peek))
                  (incf i)))
         (cond
          ((char= #\% char)
           (advance)
           (cond
            ((char= #\u (peek))
             (unless unicodep
               (setq unicodep t)
               (upgrade-vector vector '(integer 0 65535)))
             (advance)
             (push-integer (decode-hex 4)))
            (t
             (push-integer (decode-hex 2)))))
          (t
           (push-integer (char-code (case char
                                      ((#\+) #\Space)
                                      (otherwise char))))
           (advance))))))
    (cond (unicodep
           (upgrade-vector vector 'character :converter #'code-char))
          (t (babel:octets-to-string vector :encoding external-format)))))
