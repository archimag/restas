;;;; restas-direcotry-publisher.lisp
;;;;
;;;; RESTAS plugin for publish  directories:
;;;; Sample:
;;;; (restas:define-site-plugin devdoc ('#:restas-direcotry-publisher)
;;;;    (restas-direcotry-publisher:*directory* #P"/var/www/localhost/htdocs/devdoc/"))
;;;;

(restas:define-plugin #:restas.directory-publisher
  (:use :cl :iter)
  (:export #:*directory*))

(in-package #:restas.directory-publisher)

(defvar *directory* #P"/tmp/")

(defvar *directory-index-files* '("index.html" "index.htm"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (closure-template:compile-template :common-lisp-backend
                                     (merge-pathnames "plugins/directory-publisher.tmpl"
                                                      (asdf:component-pathname (asdf:find-system '#:restas)))))

;;(local-time:format-http-timestring nil (local-time:unix-to-timestamp (sb-posix:time)))

(defun path-info (path)
  (let* ((stat #+sbcl (sb-posix:stat path)
              #-sbcl nil)
         (last-modified (local-time:format-timestring nil
                                                      (local-time:unix-to-timestamp (sb-posix:stat-mtime stat))
                                                      :format '((:day 2) #\- :short-month #\- :year
                                                                #\Space (:hour 2) #\: (:min 2))))
         (dir (fad:directory-pathname-p path)))
    (list :type (if dir
                    "Directory"
                    (hunchentoot:mime-type path))
          :name (if dir
                    (car (last (pathname-directory path)))
                    (file-namestring path))
          :last-modified last-modified
          :size (if (not dir)
                    (sb-posix:stat-size stat)))))

(defun directory-info (path)
     (list :title (format nil "Index of ~A" path)
           :parent (unless (equal path *directory*) "..")
           :paths (iter (for item in (fad:list-directory (merge-pathnames path *directory*)))
                        (collect (path-info item)))))

(define-route route ("*path" :method :get)
  (let ((path (merge-pathnames (format nil "~{~A~^/~}" path)
                               *directory*)))
    (if (and (fad:directory-pathname-p path)
             (fad:directory-exists-p path))
        (or (iter (for index in *directory-index-files*)
                  (let ((index-path (merge-pathnames index path)))
                    (finding index-path such-that (fad:file-exists-p index-path))))
            (restas.directory-publisher.view:autoindex (directory-info path)))
        path)))
