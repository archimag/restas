;;;; wookie.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas.wookie)

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

(defun restas-dispatch-request (request response)
  (flet ((not-found-if-null (thing)
           (unless thing
             (wookie:send-response response
                                   :status restas:+http-not-found+
                                   :body "What you're looking for isn't here.")
             (return-from restas-dispatch-request))))
    #|------------------------------------------------------------------------|#
    (let ((vhost (car restas::*vhosts*))
          (restas:*request* request)
          (restas:*reply* response))
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
           (wookie:send-response response
                                 :status restas:+http-ok+
                                 :body result))
         (t ()
            ;;(invoke-debugger e)
            (wookie:send-response response
                                 :status restas:+http-internal-server-error+
                                 :body "Internal Server Error")
            ))))))

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
           (let ((wookie:*state* (make-instance 'wookie:wookie-state)))
             #|---------------------------------------------------------------|#
             (wookie:load-plugins)
             #|---------------------------------------------------------------|#
             (wookie:defroute (:* ".*") (req res)
               (restas-dispatch-request req res))
             #|---------------------------------------------------------------|#
             (as:with-event-loop ()
               (wookie:start-server (make-instance 'wookie:listener :port port))))))
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


