(defpackage :example
  (:use :cl)
  (:export :ex-fnc))

(in-package :example)
(defun ex-fnc ()
  (format t "example function"))
