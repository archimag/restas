;;;; wookie.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.wookie)

(defun restas-dispatch-request (request reply)
  (flet ((not-found-if-null (thing)
           (unless thing
             #|---------------------------------------------------------------|#
             (setf (restas:return-code reply)
                   restas:+http-not-found+)
             #|---------------------------------------------------------------|#
             (setf (restas:content-type* reply)
                  "text/html")
             #|---------------------------------------------------------------|#
             (send-reply reply
                         (restas:restas-status-message restas:+http-not-found+))
             #|---------------------------------------------------------------|#
             (return-from restas-dispatch-request))))
    #|------------------------------------------------------------------------|#
    (let ((vhost (car restas::*vhosts*))
          (restas:*request* request)
          (restas:*reply* reply))
      #|----------------------------------------------------------------------|#
      (not-found-if-null vhost)
      #|----------------------------------------------------------------------|#
      (multiple-value-bind (route bindings)
          (routes:match (slot-value vhost 'restas::mapper) (restas:request-uri request))
        #|--------------------------------------------------------------------|#
        (not-found-if-null route)
        #|--------------------------------------------------------------------|#
        (bb:promise-handler-case
         (bb:alet ((result (restas:process-route route bindings)))
           (cond
             #|---------------------------------------------------------------|#
             ((pathnamep result)
              (handle-static-file route request reply result))
             #|---------------------------------------------------------------|#
             (t
              (send-reply reply result))))
         (t (e)
            (log-route-error route e)
            #|----------------------------------------------------------------|#
            (when *debug-mode*
              (break "~A" e))
            #|----------------------------------------------------------------|#
            (setf (restas:return-code reply)
                  restas:+http-internal-server-error+)
            #|----------------------------------------------------------------|#
            (setf (restas:content-type* reply)
                  "text/html")
            #|----------------------------------------------------------------|#
            (send-reply reply
                        (restas:restas-status-message restas:+http-internal-server-error+))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start/stop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *port-thread-map* (make-hash-table))

(defun  store-request-body-hook (request)
  (let* ((headers (wookie:request-headers request))
         (content-type (getf headers :content-type)))
    (unless (or (search "application/x-www-form-urlencoded" content-type)
                (search "multipart/form-data;" content-type))
      (setf (http-parse:http-store-body (wookie:request-http request)) t))))


(defun init-wookie-state ()
  (let ((wookie:*state* (make-instance 'wookie:wookie-state)))
    #|------------------------------------------------------------------------|#
    (wookie:add-hook :parsed-headers 'store-request-body-hook)
    #|------------------------------------------------------------------------|#
    (wookie:defroute (:* ".*") (req res)
      (restas-dispatch-request req (make-instance 'reply :origin res)))
    #|------------------------------------------------------------------------|#
    (wookie:load-plugins)
    #|------------------------------------------------------------------------|#
    wookie:*state*))
    

(defun start (module &key hostname (port 80) (separate-thread t))
  (restas:add-toplevel-module module hostname port)
  #|--------------------------------------------------------------------------|#
  (flet ((event-loop ()
           (let ((wookie:*state* (init-wookie-state))
                 (*listener* (make-instance 'wookie:listener :port port)))
             (as:with-event-loop ()
               (wookie:start-server *listener*)))))
    (cond
      #|----------------------------------------------------------------------|#
      (separate-thread
       (unless (gethash port *port-thread-map*)
         (setf (gethash port *port-thread-map*)
               (bt:make-thread #'event-loop :name "RESTAS-Wookie Thread"))))
      #|----------------------------------------------------------------------|#
      (t (event-loop)))))

(defun stop-all ()
  (iter (for port in (alexandria:hash-table-keys *port-thread-map*))
        (bt:destroy-thread (gethash port *port-thread-map*))
        (remhash port *port-thread-map*)))


