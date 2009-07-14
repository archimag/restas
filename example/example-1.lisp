;;; example.lisp

(asdf:operate 'asdf:load-op :restas)
(asdf:operate 'asdf:load-op :xfactory)

(restas:define-plugin :restas.example-1
  (:use :cl))

(in-package :restas.example-1)

(defun in-pool (obj)
  (gp:object-register obj *request-pool*))

(defparameter *master* "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">

<html xmlns=\"http://www.w3.org/1999/xhtml\">
	<head >
        <meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
        <title>Welcome</title>
	</head>

	<body>
       <h1>Welcome!</h1>
       <div id=\"content\" />
    </body>
</html> ")


(define-simple-route main ("" :method :get :overlay-master *master*)
  (in-pool
   (xfactory:with-document-factory ((html))
     (html "overlay"
           (html :div
                 (xfactory:attributes :id "content")
                 (html :form
                       (xfactory:attributes :method "post")
                       (html :div "Input test message:")
                       (html :input
                             (xfactory:attributes :name "message"))
                       (html :input
                             (xfactory:attributes :value "Send"
                                                  :type "submit"))))))))
   
(define-simple-route main/post ("" :method :post :overlay-master *master*)
  (in-pool
   (xfactory:with-document-factory ((html))
     (html "overlay"
           (html :div
                 (xfactory:attributes :id "content")
                 (html :div
                       (html :b "test message: ")
                       (xfactory:text (hunchentoot:post-parameter "message")))
                 (html :a
                       (xfactory:attributes :href "")
                       "Try again"))))))


(restas:reconnect-all-plugins)
(restas:start-web-server 8080)


  