(defpackage :kiln/scripts/eval
  (:use :cl)
  (:documentation "Eval arguments as Lisp forms"))
(in-package :kiln/scripts/eval)

(defvar *eof* "eof")

(defun main (args)
  (let ((*package* (find-package :cl-user)))
    (dolist (arg args)
      (with-input-from-string (in arg)
        (loop for form = (read in nil *eof*)
              until (eq form *eof*)
              do (format *error-output* "~s~%=> " form)
                 (format t "~s~%" (eval form)))))))
