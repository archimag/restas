;; -*- mode: lisp -*-

(asdf:operate 'asdf:load-op '#:sphinx)
(asdf:operate 'asdf:load-op '#:cl-libxml2)

(defpackage #:restas.doc
  (:use #:cl #:iter #:split-sequence)
  (:export #:make-restas-doc))

(in-package #:restas.doc)

(defparameter *span-classes*
  '("symbol" "special" "keyword" "comment" "string" "character"))

(defun update-code-div (div)
  (labels
      ((bad-span-p (node)
         (and (string-equal (xtree:local-name node) "span")
              (not (member (xtree:attribute-value node "class") *span-classes* :test #'string-equal))))
       ;;---------------------------------------
       (comment-p (node)
         (and (string-equal (xtree:local-name node) "span")
              (string-equal (xtree:attribute-value node "class") "comment")))
       ;;---------------------------------------
       (br-p (node)
         (string-equal (xtree:local-name node) "br"))
       ;;---------------------------------------
       (flatten-spans (node)
         (iter (for el in (xtree:all-childs node))
               (flatten-spans el))
         ;;---------------------------------------
         (when (comment-p node)
           (setf (xtree:text-content node)
                 (xtree:text-content node))
           (xtree:insert-child-after (xtree:make-element "br") node))
         ;;---------------------------------------
         (when (bad-span-p node)
           (iter (for el in (xtree:all-childs node))
                 (xtree:insert-child-before (xtree:detach el) node))
           (xtree:remove-child node))))
    ;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    (flatten-spans div)
    ;;---------------------------------------
    (let* ((pre (xtree:make-element "div"))
           (ol (xtree:make-child-element pre "ol")))
      (setf (xtree:attribute-value pre "class")
            "prettyprint linenums")
      (setf (xtree:attribute-value ol "class")
            "linenums")
      ;;---------------------------------------
      (iter (for line in (split-sequence-if #'br-p (xtree:all-childs div)))
            (for i from 0)
            (let ((li (xtree:make-child-element ol "li")))
              (setf (xtree:attribute-value li "class")
                    (format nil "L~s" i))
              ;;---------------------------------------
              (iter (for el in line)
                    (xtree:append-child li (xtree:detach el)))))
      ;;---------------------------------------
      (xtree:insert-child-before pre div)
      (xtree:remove-child div))))

(defun fix-section (node)
  (let ((section (xtree:make-element "section")))
    (setf (xtree:attribute-value section "id")
          (xtree:attribute-value node "id"))
    (iter (for child in (xtree:all-childs node))
          (xtree:append-child section (xtree:detach child)))
    (xtree:insert-child-before section node)
    (xtree:remove-child node)))


(defun html-p (path)
  (string-equal (pathname-type path) "html"))

(defun update-html-for-bootstrap (path)
  (handler-bind ((warning #'muffle-warning))
    (html:with-parse-html (doc path)
      (iter (for div in (xpath:find-list doc "//div[@class='code']"))
            (update-code-div div))
      #|---------------------------------------|#
      (iter (for div in (xpath:find-list doc "//div[@class='section']"))
            (fix-section div))
      ;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      (html:serialize-html doc path))))

(defun make-restas-doc ()
  (let* ((restas-dir (make-pathname :directory (pathname-directory
                                                (asdf:system-definition-pathname '#:restas.core))))
         (origin (merge-pathnames #P"doc/contents.txt" restas-dir))
         (target (merge-pathnames #P"doc/build/" restas-dir)))
    #|------------------------------------------------------------------------------------------|#
    (sphinx:make-documentation origin target :verbose t)
    #|------------------------------------------------------------------------------------------|#
    (fad:walk-directory target 'update-html-for-bootstrap :test #'html-p)))

;;(make-restas-doc)
