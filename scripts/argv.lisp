(defpackage :kiln/scripts/argv
  (:use :cl)
  (:documentation "Print arguments as a list.
Useful for debugging how your shell tokenizes input."))
(in-package :kiln/scripts/argv)

(defun main (args)
  (format t "~s" args))
