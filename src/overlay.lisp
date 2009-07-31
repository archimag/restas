;; overlay.lisp

(in-package :restas)

;;; apply-overlay

(defun prepare-master (master bindings)
  (iter (for node in-xpath-result "/html:html/html:head/html:link" on master)
        (setf (xtree:attribute-value node "href")
              (expand-text (xtree:attribute-value node "href")
                           bindings)))
  (iter (for node in-xpath-result "/html:html/html:head/html:script" on master)
        (setf (xtree:attribute-value node "src")
              (expand-text (xtree:attribute-value node "src")
                           bindings)))
  master)

(defun apply-overlay/impl (master overlay)
  (if master
      (iter (for child in-child-nodes overlay)
            (cond 
              ((string= (xtree:attribute-value child "asfirst") "true") 
               (xtree:insert-child-before (xtree:copy child)
                                          (xtree:first-child master)))
               (t (xtree:append-child master (xtree:copy child)))))))



(defgeneric apply-overlay (origin overlay bindings))

(defmethod apply-overlay ((origin xtree:document) (overlay xtree:document) bindings)
  (prepare-master origin bindings)
  (xtree:process-xinclude origin (xtree:parse-options :xml-parse-noxincnode :xml-parse-nobasefix))
  (xtree:process-xinclude overlay (xtree:parse-options :xml-parse-noxincnode :xml-parse-nobasefix))
  (iter (for child in-child-nodes (xtree:root overlay) with (:type :xml-element-node))
        (apply-overlay/impl (or (if (string= "head" (xtree:local-name child))
                                    (xtree:find-node (xtree:first-child (xtree:root origin))
                                                     (xtree:node-filter :local-name "head")))
                                (let ((id (xtree:attribute-value child "id")))
                                  (if id
                                      (xpath:find-single-node origin
                                                              (format nil "//*[@id='~A']" id)))))
                            child))
  (if (> (xtree:version) 20700)
      (xtree:append-document-property origin :xml-doc-html))
  origin)

;; (defmethod apply-overlay ((origin pathname) overlay bindings)
;;   (apply-overlay (gp:object-register (xtree:parse origin)
;;                                      *request-pool*)
;;                  overlay
;;                  bindings))

;; (defmethod apply-overlay ((origin puri:uri) overlay bindings)
;;   (apply-overlay (gp:object-register (xtree:parse origin)
;;                                      *request-pool*)
;;                  overlay
;;                  bindings))

(defmethod apply-overlay (origin overlay bindings)
  (apply-overlay (gp:object-register (xtree:parse origin)
                                     *request-pool*)
                 overlay
                 bindings))

(defmethod apply-overlay (origin (overlay-path pathname) bindings)
  (xtree:with-parse-document (overlay overlay-path)
    (apply-overlay origin
                   overlay
                   bindings)))



;; (defun apply-overlay (master-path overlay bindings)
;;   (let ((master (gp:object-register (prepare-master (xtree:parse master-path)
;;                                                     bindings)
;;                                     *request-pool*)))

;;     (iter (for child in-child-nodes (xtree:root overlay) with (:type :xml-element-node))
;;           (apply-overlay/impl (or (if (string= "head" (xtree:local-name child))
;;                                       (xtree:find-node (xtree:first-child (xtree:root master))
;;                                                        (xtree:node-filter :local-name "head")))
;;                                   (let ((id (xtree:attribute-value child "id")))
;;                                     (if id
;;                                         (xpath:find-single-node master
;;                                                                 (format nil "//*[@id='~A']" id)))))
;;                               child))
;;     master))
