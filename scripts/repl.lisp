(defpackage :kiln/scripts/repl
  (:use :cl :alexandria :serapeum)
  (:export :repl)
  (:documentation "Launch a simple REPL"))
(in-package :kiln/scripts/repl)

(defun repl ()
  "Launch a simple REPL."
  ;; TODO More sophisticated?
  (format *error-output* "Write ~s or send EOF to quit" :quit)
  (with-standard-io-syntax
    (let ((*print-circle* t)
          (uiop:*lisp-interaction* t))
      (loop (format *error-output* "~%> ")
            (let ((input (read *standard-input* nil :quit)))
              (if (eql input :quit)
                  (progn
                    (terpri *error-output*)
                    (return))
                  (let ((values (multiple-value-list (eval input))))
                    (dolist (value values)
                      (format t "~s" value)
                      (finish-output)))))))))

(defun main (args)
  (declare (ignore args))
  (repl))
