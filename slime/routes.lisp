;;;; routes-swank.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:routes)

(defparameter *indention* 4)

(defparameter *current-indention* nil)

(defgeneric route-emacs-inspect (route))

(defmethod route-emacs-inspect (route)
  (list (format nil "~A" route)))

(defmethod route-emacs-inspect ((route variable-template))
  `((:value ,route ,(format nil "$~(~A~)" (template-data route)))))

(defmethod route-emacs-inspect ((route wildcard-template))
  `((:value ,route ,(format nil "*~(~A~)" (template-data route)))))

(defmethod route-emacs-inspect ((route cons))
  (cond
    ((typep (car route) 'route)
     `("" (:value ,(car route) ,(route-name (car route)))))
    ((and (typep (cdr route) 'cons)
          (typep (car (cdr route)) 'route))
     (concatenate 'list                    
                  (route-emacs-inspect (car route))
                  (if (equal (car route) "")
                      nil
                      '(" "))
                  (route-emacs-inspect (cdr route))))
    (t (concatenate 'list                    
                    (route-emacs-inspect (car route))
                    '("/")
                    (route-emacs-inspect (cdr route))))))

(defmethod route-emacs-inspect ((route or-template))
  (let ((*current-indention* (if (null *current-indention*)
                                 0
                                 (+ *current-indention* *indention*))))
    (iter (for item in (template-data route))
          (collect '(:newline))
          (collect (make-string *current-indention* :initial-element #\Space))
          (dolist (x (route-emacs-inspect item))
            (collect x)))))


(defmethod route-emacs-inspect ((route concat-template))
  (iter (for item in (template-data route))
        (dolist (x (route-emacs-inspect item))
          (collect x))))
        

(defmethod swank::emacs-inspect ((mapper mapper))
  `("" (:newline)
       "Tree of routes" (:newline)
       "--------------------------------------------------" (:newline)
       ,@(route-emacs-inspect (slot-value mapper 'template))
       (:newline)
       (:newline)       
       (:action "Reset route map" ,#'(lambda () (reset-mapper mapper)))))
