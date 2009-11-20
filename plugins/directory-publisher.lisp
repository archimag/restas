;;;; restas-direcotry-publisher.lisp
;;;;
;;;; RESTAS plugin for publish  directories:

(restas:define-plugin #:restas.directory-publisher
    (:use :cl :iter)
  (:export #:*directory*))

(in-package #:restas.directory-publisher)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; compile template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (closure-template:compile-template :common-lisp-backend
                                     (merge-pathnames "plugins/directory-publisher.tmpl"
                                                      (asdf:component-pathname (asdf:find-system '#:restas)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Plugin variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *directory* nil)

(defvar *directory-index-files* '("index.html" "index.htm"))

(defvar *autoindex* t)

(defvar *autoindex-template* 'restas.directory-publisher.view:autoindex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; directory info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun path-last-name (path)
  "File or directory name"
  (if (fad:directory-pathname-p path)
      (car (last (pathname-directory path)))
      (file-namestring path)))

(defun hidden-file-p (path)
  (char= (char (path-last-name path)
               0)
         #\.))

(defparameter *byte-units* '("kB" "MB" "GB" "TB" "PB" "EB" "ZB" "YB")
  "Symbols for show size of files in human readable format")

(defun format-size (bytes)
  "Convert number of bytes to human readable format (e.g. 1K 234M 2G)"
  (let* ((unit (floor (log bytes 1024)))
         (symbol (if (> unit 0) (nth (1- unit) *byte-units*))))
    (if symbol
        (format nil
                "~,1F ~A"
                (/ bytes (expt 1024 unit))
                symbol)
        (format nil "~A B" bytes))))

(defun path-info (path)
  "System information in plist by pathname"
  (let* ((stat #+sbcl (sb-posix:stat path)
               #-sbcl nil)
         (last-modified (local-time:format-timestring nil
                                                      (local-time:unix-to-timestamp (sb-posix:stat-mtime stat))
                                                      :format '((:day 2) #\- :short-month #\- :year #\Space (:hour 2) #\: (:min 2))))
         (dir (fad:directory-pathname-p path)))
    (list :type (if dir
                    "Directory"
                    (hunchentoot:mime-type path))
          :name (path-last-name path)
          :last-modified last-modified
          :size (if (not dir)
                    (format-size (sb-posix:stat-size stat))))))

(defun directory-autoindex-info (path rpath)
  "Info on directory for autoindex"
  (list :title (format nil "Index of /~A" rpath)
        :parent (unless (equal path *directory*) "..")
        :paths (iter (for item in (fad:list-directory (merge-pathnames path *directory*)))
                     (unless (hidden-file-p item)
                       (collect (path-info item))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(define-route route ("*path" :method :get)
  (let* ((relative-path (format nil "~{~A~^/~}" path))
         (path (merge-pathnames relative-path
                                *directory*)))
    (if (and (fad:directory-pathname-p path)
             (fad:directory-exists-p path))
        (or (iter (for index in *directory-index-files*)
                  (let ((index-path (merge-pathnames index path)))
                    (finding index-path
                             such-that (fad:file-exists-p index-path))))
            (if *autoindex*
                (funcall *autoindex-template*
                         (directory-autoindex-info path relative-path))
                hunchentoot:+http-not-found+))
        path)))
