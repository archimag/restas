;;;; declarations.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

(defun split-code-declarations (body)
  (let ((declarations-map (make-hash-table))
        (code body))
    (iter (for rbody on body)
          (for form = (car rbody))
          (while (and (consp  form) (keywordp (car form))))
          (setf code (cdr rbody))
          (setf (gethash (car form) declarations-map)
                (append (cdr form)
                        (gethash (car form) declarations-map))))
    (values declarations-map code)))

(defmethod parse-declarations (type declarations traits)
  (error "Unknown type of declaration: ~A" type))

(defmethod parse-declarations ((type (eql :sift-variables)) declarations traits)
  (let ((variables (gethash :variables traits)))
    (setf (gethash :parse-vars traits)
          (cons 'list
                (iter (for (var rule) in declarations)
                      (collect (find var variables :test #'string=))
                      (collect `(data-sift:compile-parse-rule ,rule)))))))

(defmethod parse-declarations ((type (eql :additional-variables))
                               declarations traits)
  (iter (for (var handler default) in declarations)
        (collect (if default
                     (list var default)
                     var)
          :into arglist)
        (collect (if default
                     `(list ,(alexandria:make-keyword var)
                        (alexandria:named-lambda additional-variable-handler ()
                          (or ,handler ,default)))
                     `(list ,(alexandria:make-keyword var)
                        (alexandria:named-lambda additional-variable-handler ()
                          ,handler)))
          :into handlers)
        (finally
         (setf (gethash :additional-variables-arglist traits) (cons '&key arglist)
               (gethash :additional-variables traits) (cons 'list handlers)))))

(defmethod parse-declarations ((type (eql :render-method)) declarations traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A declaration" type))
  (setf (gethash type traits)
        (first declarations)))

(defmethod parse-declarations ((type (eql :apply-render-method))
                               declarations traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A declaration" type))
  (setf (gethash :render-method traits)
        `(alexandria:named-lambda apply-render-method (data)
           (apply ,(first declarations) data))))

(defmethod parse-declarations ((type (eql :content-type)) declarations traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A declaration" type))
  (setf (gethash type traits)
        (first declarations)))

(defmethod parse-declarations ((type (eql :decorators)) declarations traits)
  (setf (gethash type traits)
        (cons 'list declarations)))

(defmethod parse-declarations ((type (eql :http-method)) declarations traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A declaration" type))
  (setf (gethash :method traits)
        (first declarations)))

(defmethod parse-declarations ((type (eql :requirement)) declarations traits)
  (setf (gethash type traits) (cons 'list declarations)))

(defmethod parse-declarations ((type (eql :url)) declarations traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A declaration" type))
  (setf (gethash :url traits)
        (first declarations)))

(defmethod parse-declarations ((type (eql :inherit-parent-context))
                               declarations traits)
  (when (cdr declarations)
    (error "Multiple instances of ~A declaration" type))
  (setf (gethash :inherit-parent-context traits)
        (first declarations)))

(defun parse-all-declarations (declarations allowed-types &optional traits)
  (let ((traits (or traits (make-hash-table))))
    (iter (for (key value) in-hashtable declarations)
          (cond
            ((member key allowed-types)
             (parse-declarations key value traits))
            (t
             (error "Unknown declaration type: ~A" key))))
    traits))
