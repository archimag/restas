;;;; restas-xfactory.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:restas.optional)

(defun/export eid (format &rest args)
  "Make id attribute"
  (xfactory:attributes :id
                       (apply-format-aux format args)))

(defun/export eclass (format &rest args)
  "Make class attribute"
  (xfactory:attributes :class
                       (apply-format-aux format args)))

(defun/export ehref (format &rest args)
  "Make href attribute"
  (xfactory:attributes :href
                       (apply-format-aux format args)))

(defun/export estyle (format &rest args)
  "Make style attributes"
  (xfactory:attributes :style
                       (apply-format-aux format args)))

(defun/export escript (src &optional (type "text/javascript"))
  "Make script element"
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "script")))
    (xfactory:attributes :src src
                         :type type)))

(defun/export ecss (format &rest args)
  "Make link css element"
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "link")))
    (xfactory:attributes :href (apply-format-aux format args)
                         :rel "stylesheet"
                         :type "text/css")))

(defun/export e-break-line ()
  "Make br element"
  (xtree:make-child-element xfactory:*node* "br"))

(defun/export estrong (format &rest args)
  "Make strong element"
  (xtree:make-child-text (xtree:make-child-element xfactory:*node*
                                          "strong")
                         (apply-format-aux format args)))

(defun/export e-text2html (text)
  "parse text as html and append to current element"
  (if text
      (html:with-parse-html (html text)
        (when html
          (iter (for node in (iter (for node in-child-nodes (xpath:find-single-node html "/html/body"))
                                   (collect node)))
                (xtree:detach node)
                (xtree:append-child xfactory:*node* node))))))

(defun/export etext (format &rest args)
  (apply #'xfactory:text
         format
         args))
