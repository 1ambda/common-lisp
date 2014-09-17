(defpackage :app
  (:use :cl)
  (:export :start))

(in-package :app)
(defun start()
  (format t "Welcome, ASDF"))
