;;;; mime-types.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas)

(defparameter *mime-type-hash*
  (let ((hash (make-hash-table :test #'equalp)))
    (iter (for line in-file (or (probe-file #P"/etc/mime.types")
                                (merge-pathnames #P"core/mime.types"
                                                 (asdf:system-definition-pathname '#:restas.core)))
               using #'read-line)
          #|------------------------------------------------------------------|#
          (let* ((parsed-line (split-sequence #\Tab line))
                 (type (first parsed-line))
                 (suffixes (split-sequence #\Space (alexandria:lastcar parsed-line))))
            #|----------------------------------------------------------------|#
            (iter (for suffix in suffixes)
                  (setf (gethash suffix hash)
                        type))))
    hash))

(defun mime-type (pathspec)
  "Given a pathname designator PATHSPEC returns the MIME type
\(as a string) corresponding to the suffix of the file denoted by
PATHSPEC \(or NIL)."
  (gethash (pathname-type pathspec) *mime-type-hash*))




