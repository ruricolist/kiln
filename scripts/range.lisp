(defpackage :kiln/scripts/range
  (:use :cl :serapeum :trivia)
  (:shadowing-import-from :serapeum :@)
  (:documentation "Print a range of numbers"))
(in-package :kiln/scripts/range)

(defun main (args)
  (let* ((*read-default-float-format* 'double-float)
         (range (apply #'range (mapcar #'parse-number args))))
    (do-each (n range)
      (format t "~a~%" n))))
