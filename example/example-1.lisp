(asdf:operate 'asdf:load-op :cl-who)
(asdf:operate 'asdf:load-op :restas)

(restas:defsite :restas.example-1
  (:use :cl))

(in-package :restas.example-1)

(restas:define-site-plugin  default (:restas.example-1))


(define-route main ("" :method :get)
  (who:with-html-output-to-string (out)
    (:html
     (:body
      ((:form :method :post)
       ((:input :name "message"))
       ((:input :type "submit" :value "Send")))))))

(define-route main/post ("" :method :post)
  (who:with-html-output-to-string (out)
    (:html
     (:body
      (:div
       (:b (who:fmt "test message: ~A"
                    (hunchentoot:post-parameter "message"))))
      ((:a :href (restas:genurl 'main)) "Try again")))))

(restas:start-site :restas.example-1 :port 8080)
