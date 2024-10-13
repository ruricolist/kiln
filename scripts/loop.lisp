(defpackage :kiln/scripts/loop
  (:documentation "Write shell loops with the loop macro")
  (:use :cl)
  (:import-from :cmd)
  (:import-from :serapeum :string^= :drop :string+ :null-if-empty)
  (:shadow :t)
  (:export :main))
(in-package :kiln/scripts/loop)

(defun parse-loop (args)
  (let* ((shell
           (or (null-if-empty (uiop:getenv "SHELL"))
               "/bin/sh"))
         (vars))
    (flet ((shell (arg)
             `(cmd:cmd
               "env"
               ,@(mapcar (lambda (var)
                           `(list (string+
                                   ',(string-downcase var)
                                   "="
                                   ,var)))
                         vars)
               ',(list shell) "-c" ',(list (drop 1 arg)))))
      (cons 'loop
            (loop for arg in args
                  collecting
                  (cond ((string^= "!" arg)
                         (shell arg))
                        ((string^= "@" arg)
                         (let* ((var-name (string-upcase (drop 1 arg)))
                                (var (intern var-name)))
                           (push var vars)
                           var))
                        ((every #'digit-char-p arg)
                         (parse-integer arg))
                        (cl:t (intern (string-upcase arg)))))))))

(defun main (args)
  (eval (parse-loop args)))
