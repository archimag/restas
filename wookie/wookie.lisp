;;;; wookie.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.wookie)

(defmethod restas:render-object :around (designer (future asf:future))
  (let ((result (asf:make-future)))
    (asf:future-handler-case
     (asf:alet ((object future))
       (asf:finish result (restas:render-object designer object)))
     (t (e)
        (asf:signal-error result e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dispatch request
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun restas-dispatch-request (request reply)
  (flet ((not-found-if-null (thing)
           (unless thing
             (setf (restas:return-code reply)
                   restas:+http-not-found+)
             (send-reply reply
                         (restas:restas-status-message restas:+http-not-found+))
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
        (asf:future-handler-case
         (asf:alet ((result (restas:process-route route bindings)))
           (cond
             #|---------------------------------------------------------------|#
             ((pathnamep result)
              (handle-static-file request reply result))
             #|---------------------------------------------------------------|#
             ((and (string= result "") (not (= (restas:return-code reply) restas:+http-ok+)))
              (send-reply reply
                          (restas:restas-status-message (restas:return-code reply))))
             #|---------------------------------------------------------------|#
             (t
              (send-reply reply result))))
         (t (e)
            #|----------------------------------------------------------------|#
            (when *debug-mode*
              (break "~A" e))
            #|----------------------------------------------------------------|#
            (setf (restas:return-code reply)
                  restas:+http-internal-server-error+)
            #|----------------------------------------------------------------|#
            (send-reply reply
                        (restas:restas-status-message restas:+http-internal-server-error+))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start/stop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *port-thread-map* (make-hash-table))

(defun start (module &key hostname (port 80) (separate-thread t))
  (restas::add-toplevel-module module
                               hostname
                               port
                               :context (restas:make-context))
  #|--------------------------------------------------------------------------|#
  (flet ((event-loop ()
           (let ((wookie:*state* (make-instance 'wookie:wookie-state))
                 (*listener* (make-instance 'wookie:listener :port port)))
             #|---------------------------------------------------------------|#
             (wookie:load-plugins)
             #|---------------------------------------------------------------|#
             (wookie:defroute (:* ".*") (req res)
               (restas-dispatch-request req (make-instance 'reply :origin res)))
             #|---------------------------------------------------------------|#
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


