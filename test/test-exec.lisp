#!/usr/bin/env kiln

(defun main (argv)
  (let ((unwind? (and argv t)))
    (format t "Before exec~%")
    (finish-output)
    (unwind-protect
         (kiln/utils:exec "echo exec happened" :unwind unwind?)
      (progn
        (format t "Unwinding happened~%")
        (finish-output)))))
