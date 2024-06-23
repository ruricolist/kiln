(defpackage :kiln/scripts/count
  (:use :cl :alexandria :serapeum)
  (:export :main)
  (:documentation "Print number of arguments and input lines"))
(in-package :kiln/scripts/count)

(defun main (args)
  (loop for line = (and (listen *standard-input*)
                        (read-line *standard-input* nil nil))
        for count upfrom (length args)
        while line
        finally (format t "~a~%" count)))
