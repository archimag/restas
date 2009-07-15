;;; example.lisp

(asdf:operate 'asdf:load-op :restas)
(asdf:operate 'asdf:load-op :xfactory)

(restas:define-plugin :restas.example-2
  (:use :cl))

(in-package :restas.example-2)

(defparameter *frame* "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">

<html xmlns=\"http://www.w3.org/1999/xhtml\">
	<head >
        <meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
	</head>

	<body>
       <div id=\"content\" />
    </body>
</html> ")

(defun in-pool (obj)
  (gp:object-register obj *request-pool*))

(defun apply-format-aux (format args)
  (if args
      (apply #'format nil (cons format args))
      format))

(defun eid (format &rest args)
  "Make id attribute"
  (xfactory:attributes :id
                       (apply-format-aux format args)))

(defun ehref (format &rest args)
  "Make href attribute"
  (xfactory:attributes :href
                       (apply-format-aux format args)))

(defun etitle (format &rest args)
  (xfactory:with-element-factory ((E))
    (E :head
       (E :title
          (xfactory:text (apply-format-aux format args))))))


(defun index-page ()
  (in-pool 
   (xfactory:with-document-factory ((html))
     (html :overlay
           (etitle "Example 2: Index")
           (html :div
                 (xfactory:attributes :id "content")
                 (eid "content")
                 (html :h1 "Index")
                 (html :ul
                       (loop for x from 1 to 10
                          do (html :li
                                   (html :a
                                         (ehref (genurl 'chapter-?.html :id x))
                                         (xfactory:text "Chapter ~A" x))))))))))

(define-simple-route root ("" :overlay-master *frame*)
  (index-page))

(define-simple-route index.html ("index.html" :overlay-master *frame*)
  (index-page))

(define-simple-route chapter-?.html ("chapter-:(id).html")
  (in-pool
   (xfactory:with-document-factory ((html))
     (html :overlay
           (etitle "Example 2: Chapter ~A" id)
           (html :div
                 (eid "content")
                 (html :h1
                       (xfactory:text "Chapter ~A" id))
                 (html :ul
                       (loop for x from 1 to 10
                          do (html :li
                                   (html :a
                                         (ehref (genurl 'chapter-?-?.html :id1 id :id2 x))
                                         (xfactory:text "Chapter ~A-~A" id x)))))
                 (html :a
                       (ehref (genurl 'root))
                       "Back to Index"))))))

(define-simple-route chapter-?-?.html ("chapter-:(id1)-:(id2).html" :overlay-master *frame*)
  (in-pool
   (xfactory:with-document-factory ((html))
     (html :overlay
           (etitle "Example 2: Chapter ~A-~A" id1 id2)
           (html :div
                 (eid "content")
                 (html :h1
                       (xfactory:text "Chapter ~A-~A" id1 id2))
                 (html :p
                       (xfactory:text "This is a chapter ~A-~A" id1 id2))
                 (html :a
                       (ehref (genurl 'chapter-?.html :id id1))
                       (xfactory:text "Back to Chapter ~A" id1)))))))
  
          

(restas:reconnect-all-plugins)
(restas:start-web-server 8080)
