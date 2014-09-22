;;;; render.lisp
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :restas)

(defgeneric render-object (designer object)
  (:documentation "Render object via designer"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; default render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-object (designer (file pathname))
  "Default handler for pathname"
  (declare (ignore designer))
  file)

(defmethod render-object :before (designer (code integer))
  (setf *standard-special-page-p* nil
        (hunchentoot:return-code*) code))

(defmethod render-object (designer (code integer))
  "Default handler for HTTP status code"
  (declare (ignore designer))
  (setf *standard-special-page-p* t)
  "")

(defmethod render-object (designer (string string))
  "Default handler for string"
  (declare (ignore designer))
  string)

(defmethod render-object (designer (octets vector))
  (check-type octets (vector (unsigned-byte 8)))
  octets)

(defmethod render-object (designer (obj (eql nil)))
  (render-object designer hunchentoot:+http-not-found+))

(defmethod render-object (designer object)
  (error "Unknown as render ~A via ~A" object designer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; render via function object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-object ((designer function) object)
  (render-object nil
                 (funcall designer object)))

(defmethod render-object ((designer function) (code integer))
  (declare (ignore designer))
  (render-object nil code))

(defmethod render-object ((designer function) (file pathname))
  (declare (ignore designer))
  (render-object nil file))

(defmethod render-object ((designer function) (string string))
  (declare (ignore designer))
  (render-object nil string))

(defmethod render-object ((designer function) (octets vector))
  (declare (ignore designer))
  (render-object nil octets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; render via symbol as function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-object ((designer symbol) object)
  (if (null designer)
      (call-next-method)
      (render-object (symbol-function designer)
                     object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; render via package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-object ((designer package) object)
  (render-object (symbol-function (find-symbol
                                   (symbol-name (slot-value *route* 'symbol))
                                               designer))
                 object))

(defmethod render-object ((designer package) (code integer))
  (declare (ignore designer))
  (render-object nil code))

(defmethod render-object ((designer package) (file pathname))
  (declare (ignore designer))
  (render-object nil file))

(defmethod render-object ((designer package) (string string))
  (declare (ignore designer))
  (render-object nil string))

(defmethod render-object ((designer package) (octets vector))
  (declare (ignore designer))
  (render-object nil octets))
