;;;; restas-daemon.lisp
;;;;
;;;; Usage:
;;;; sbcl --noinform --no-userinit --no-sysinit --load /path/to/restas-daemon.lisp /path/to/daemon.conf COMMAND
;;;; where COMMAND one of: start stop zap kill restart nodaemon
;;;;
;;;; If successful, the exit code is 0, otherwise 1
;;;;
;;;; Error messages look in /var/log/messages (usually, depend on syslog configuration)
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:sbcl.daemon
  (:use #:cl #:sb-alien #:sb-ext))

(in-package #:sbcl.daemon)

(defvar *daemon-config-pathname* (second *posix-argv*))
(defvar *daemon-command* (third *posix-argv*))

(defparameter *as-daemon* (not (string= *daemon-command* "nodaemon")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WARNING!
;;;; plantform-depends constant :(
;;;; changes for you platform... or make path for sbcl ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (boundp 'sb-unix:tiocnotty)
  (defconstant sb-unix:tiocnotty 21538))

(defconstant +PR_SET_KEEPCAPS+ 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; aux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-exit-on-error (&body body)
  `(if *as-daemon*
       (handler-case (progn ,@body)
         (error (err)
           (with-output-to-string (*standard-output*)
             (let ((*print-escape* nil))
               (print-object err *error-output*)
               (write #\Newline :stream *error-output*)
               (sb-ext:quit :unix-status 1)))))
       (progn ,@body)))

(defmacro with-silence (&body body)
  `(with-output-to-string (*trace-output*)
     (with-output-to-string (*standard-output*)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; basic parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:sbcl.daemon.preferences
  (:use #:cl)
  (:export #:*name*
           #:*user*
           #:*group*
           #:*fasldir*
           #:*pidfile*
           #:*swankport*
           #:*default-host-redirect*
           #:*asdf-central-registry*
           #:*quicklisp-home*
           #:*asdf-load-systems*
           #:*sites*))

(with-exit-on-error
  (let ((*package* (find-package '#:sbcl.daemon.preferences)))
    (load *daemon-config-pathname*)))

(defmacro defpref (name &optional default)
  `(with-exit-on-error
     (defparameter ,name
       (let ((symbol (find-symbol (symbol-name ',name) '#:sbcl.daemon.preferences)))
         (if (boundp symbol)
             (symbol-value symbol)
             ,default)))))

(defpref *name* (error "The param *name* is unbound"))

(defpref *user* *name*)

(defpref *group*)

(defpref *fasldir*
    (make-pathname :directory (list :absolute "var" "cache" *name* "fasl")))

(defpref *pidfile* (format nil "/var/run/~A/~A.pid" *name* *name*))

(defpref *swankport*)

(defpref *asdf-central-registry*)

(defpref *quicklisp-home*)

(defpref *asdf-load-systems*)

(defpref *sites*)

(defpref *default-host-redirect*)

(defpref *acceptor-class*)

(delete-package '#:sbcl.daemon.preferences)

;;; set fasl dir

(require 'asdf)

(setf asdf::*user-cache* *fasldir*)
#-asdf2 (setf asdf::*system-cache* asdf::*user-cache*)

;;;; create necessary directories

(with-silence
  (require 'sb-posix))

(ensure-directories-exist *fasldir*)
(ensure-directories-exist *pidfile*)

(let ((uid (sb-posix:passwd-uid (sb-posix:getpwnam *user*)))
      (gid (if *group*
               (sb-posix:group-gid (sb-posix:getgrnam *group*))
               (sb-posix:passwd-gid (sb-posix:getpwnam *user*)))))
  (sb-posix:chown *fasldir* uid gid)
  (sb-posix:chown (directory-namestring *pidfile*) uid gid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Processing command line arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; command-line COMMAND

;;;; quit if COMMAND is unknown

(unless (find *daemon-command* '("start" "stop" "zap" "kill" "restart" "nodaemon") :test #'string-equal)
  (with-exit-on-error
    (error "Bad command-line options")))

;;;; zap - remove pid file

(when (string-equal *daemon-command* "zap")
  (with-exit-on-error
    (delete-file *pidfile*)
    (sb-ext:quit :unix-status 0)))

;;;; stop - send to daemon sigusr1 signal, wait and remove pid file

(defun read-pid ()
  (with-open-file (in *pidfile*)
    (read in)))

(defun stop-daemon ()
  (let ((pid (read-pid)))
    (sb-posix:kill pid sb-posix:sigusr1)
    (loop
       while (not (null (ignore-errors (sb-posix:kill pid 0))))
       do (sleep 0.1)))
  (delete-file *pidfile*))

(when (string-equal *daemon-command* "stop")
  (with-exit-on-error
    (stop-daemon)
    (sb-ext:quit :unix-status 0)))

;;;; kill - send to daemon kill signal and remove pid file

(when (string-equal *daemon-command* "kill")
  (with-exit-on-error
    (sb-posix:kill (read-pid)
                   sb-posix:sigkill)
    (delete-file *pidfile*)
    (sb-ext:quit :unix-status 0)))

;;;; restart daemon

(when (string-equal *daemon-command* "restart")
  (with-exit-on-error
    (stop-daemon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Start daemon!
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; required path for sbcl :(
(sb-posix::define-call "grantpt" int minusp (fd sb-posix::file-descriptor))
(sb-posix::define-call "unlockpt" int minusp (fd sb-posix::file-descriptor))
(sb-posix::define-call "ptsname" c-string null (fd sb-posix::file-descriptor))
(sb-posix::define-call "initgroups" int minusp (user c-string) (group sb-posix::gid-t))

(defun switch-to-slave-pseudo-terminal (&optional (out #P"/dev/null") (err #P"/dev/null"))
  (flet ((c-bit-or (&rest args)
           (reduce #'(lambda (x y) (boole boole-ior x y))
                   args)))
    (let* ((fdm (sb-posix:open #P"/dev/ptmx" sb-posix:O-RDWR))
           (slavename (progn
                        (sb-posix:grantpt fdm)
                        (sb-posix:unlockpt fdm)
                        (sb-posix:ptsname fdm)))
           (fds (sb-posix:open slavename sb-posix:O-RDONLY))
           (out-fd (sb-posix:open out
                               (c-bit-or sb-posix:O-WRONLY sb-posix:O-CREAT sb-posix:O-TRUNC)
                               (c-bit-or sb-posix:S-IREAD sb-posix:S-IWRITE sb-posix:S-IROTH)))
           (err-fd (if (not (equal err out))
                       (sb-posix:open err
                                      (c-bit-or sb-posix:O-WRONLY sb-posix:O-CREAT sb-posix:O-TRUNC)
                                      (c-bit-or sb-posix:S-IREAD sb-posix:S-IWRITE sb-posix:S-IROTH))
                       (if out (sb-posix:dup out-fd)))))
      (sb-posix:dup2 fds 0)
      (sb-posix:dup2 out-fd 1)
      (sb-posix:dup2 err-fd 2))))

(defun change-user (name &optional group)
  (let ((gid)
        (uid))
    (when group
      (setf gid
            (sb-posix:group-gid (sb-posix:getgrnam group))))
    (let ((passwd (sb-posix:getpwnam name)))
      (unless group
        (setf gid
              (sb-posix:passwd-gid passwd)))
        (setf uid
              (sb-posix:passwd-uid passwd)))
    (sb-posix:setresgid gid gid gid)
    (sb-posix:initgroups name gid)
    (sb-posix:setresuid uid uid uid)))

(defvar *status* nil)

(defun signal-handler (sig info context)
  (declare (ignore info context))
  (setf *status* sig))

(when *as-daemon*
  (sb-sys:enable-interrupt sb-posix:sigusr1 #'signal-handler)
  (sb-sys:enable-interrupt sb-posix:sigchld #'signal-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; change uid and gid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; required for start hunchentoot on port 80
(sb-posix::define-call "prctl" int minusp (option int) (arg int))
(sb-posix:prctl +PR_SET_KEEPCAPS+ 1)

(change-user *user* *group*)

;;;; required for start hunchentoot on port 80
(load-shared-object (or
		     (find-if #'probe-file
                              (or
                               #+sbcl (mapcar #'(lambda (item)
                                                  (concatenate 'string (if (member :x86-64 cl:*features*) "/lib64/" "/lib/") item))
                                              '("libcap.so.2" "libcap.so" "libcap.so.1"))
			      '("/lib/libcap.so.2" "/lib/libcap.so" "/lib/libcap.so.1")))
		     (error "No supported libcap found")))

(sb-posix::define-call "cap_from_text" (* char) null-alien (text c-string))
(sb-posix::define-call "cap_set_proc" int minusp (cap_p (* char)))
(sb-posix::define-call "cap_free" int minusp (cap_p (* char)))

(let ((cap_p (sb-posix:cap-from-text "CAP_NET_BIND_SERVICE=ep")))
  (sb-posix:cap-set-proc cap_p)
  (sb-posix:cap-free cap_p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; fork!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when *as-daemon*
  (unless (= (sb-posix:fork) 0)
    (loop
       while (null *status*)
       do (sleep 0.1))
    (quit :unix-status (if (= *status* sb-posix:sigusr1)
                           0
                           1))))


(defparameter *ppid* (sb-posix:getppid))

;;;; set global error handler
(defun global-error-handler (condition x)
  (declare (ignore x))
  (let ((err (with-output-to-string (out)
                     (let ((*print-escape* nil))
                       (print-object condition out)))))
    (print err *error-output*)
    (sb-posix:syslog sb-posix:log-err
                     err))
  (quit :unix-status 1))

(when *as-daemon*
  (setf *debugger-hook* #'global-error-handler)

  (sb-sys:enable-interrupt sb-posix:sigusr1 :default)
  (sb-sys:enable-interrupt sb-posix:sigchld :default))

;;;; change current directory
(sb-posix:chdir #P"/")

;;;; umask
(sb-posix:umask 0)

;;;; detach from tty
(when *as-daemon*
  (let ((fd (ignore-errors (sb-posix:open #P"/dev/tty" sb-posix:O-RDWR))))
    (when fd
      (sb-posix:ioctl fd sb-unix:tiocnotty)
      (sb-posix:close fd))))

;;;; rebind standart input, output and error streams
(when *as-daemon*
  (switch-to-slave-pseudo-terminal))

;;;; start new session
(when *as-daemon*
  (sb-posix:setsid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; load asdf/quicklisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop
   for path in *asdf-central-registry*
   do (push path asdf:*central-registry*))

(when *quicklisp-home*
 (load (merge-pathnames "setup.lisp" *quicklisp-home*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; start swank server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :swank-loader
  (:use :cl)
  (:export :init
           :dump-image
           :*source-directory*
           :*fasl-directory*))

(when *swankport*
  (when *fasldir*
    (defparameter swank-loader:*fasl-directory* *fasldir*))
  (asdf:oos 'asdf:load-op :swank))

(when *swankport*
  (setf (symbol-value (read-from-string "swank:*use-dedicated-output-stream*")) nil)
  (funcall (read-from-string "swank:create-server") :port *swankport*
                       :coding-system "utf-8-unix"
                       :dont-close t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Start restas server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:operate 'asdf:load-op '#:restas)

(setf (symbol-value (read-from-string "restas:*default-host-redirect*"))
      *default-host-redirect*)

(loop
   for system in *asdf-load-systems*
   do (asdf:operate 'asdf:load-op system))

(loop
   for site in *sites*
   do (if (consp site)
          (apply #'restas:start
		 (first site)
         :acceptor-class (if *acceptor-class* (read-from-string *acceptor-class*))
		 :hostname (second site)
		 :port (third site)
		 (let* ((ssl-files (fourth site)))
		   (list :ssl-certificate-file (first ssl-files)
			 :ssl-privatekey-file (second ssl-files)
			 :ssl-privatekey-password (third ssl-files))))
          (restas:start site)))

(when *as-daemon*
  (sb-sys:enable-interrupt sb-posix:sigusr1
                           #'(lambda (sig info context)
                               (declare (ignore sig info context))
                               (handler-case
                                   (progn
                                     (sb-posix:syslog sb-posix:log-info "Stop ~A daemon" *name*)
                                     (error "~A stop" *name*)
                                     )
                                 (error (err)
                                   (sb-posix:syslog sb-posix:log-err
                                                    (with-output-to-string (out)
                                                      (let ((*print-escape* nil))
                                                        (print-object err out))))))
                               (sb-ext:quit :unix-status 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end daemon initialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; write pid file
(when *as-daemon*
  (with-open-file (out *pidfile* :direction :output :if-exists :error :if-does-not-exist :create)
    (write (sb-posix:getpid) :stream out))

  (sb-posix:kill *ppid* sb-posix:sigusr1)
  (setf *debugger-hook* nil)

  (sb-posix:syslog sb-posix:log-info "Start ~A daemon" *name*))
