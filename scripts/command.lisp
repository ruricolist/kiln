(defpackage :kiln/scripts/command
  (:use :cl :cmd)
  (:import-from :kiln/flags :exit-code)
  (:documentation "Launch a subprocess with cmd"))
(in-package :kiln/scripts/command)

(defun main (args)
  (cmd args :check nil))
