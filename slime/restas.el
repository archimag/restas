;;;; restas-swank.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(require 'slime)

;;;; Modules

(defun restas-inspect-module (module)
  "Inspect module."
  (interactive (list (slime-read-package-name "Module: ")))
  (when (not module)
    (error "No module given"))
  (slime-eval-async `(restas.swank:inspect-module ,module) 'slime-open-inspector))

(defun restas-inspect-vhost-list ()
  "Inspect vhost list."
  (interactive)
  (slime-eval-async `(restas.swank:inspect-vhosts) 'slime-open-inspector))

(defun restas-restore-global-context ()
  "Restore default context"
  (interactive)
  (slime-eval '(restas.swank:restore-global-context)))

(provide 'restas)