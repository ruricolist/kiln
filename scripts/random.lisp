(defpackage :kiln/scripts/random
  (:use :cl :alexandria :trivia :serapeum)
  (:shadow :@)
  (:documentation "Generate random numbers in a given range"))
(in-package :kiln/scripts/random)

(defun main (args)
  (let ((*read-default-float-format* 'double-float))
    (format t "~a~%"
            (ematch args
              ((list) (random (expt 2 15)))
              ((list arg) (random (parse-number arg)))
              ((list lo hi)
               (random-in-range
                (parse-number lo)
                (parse-number hi)))))))
