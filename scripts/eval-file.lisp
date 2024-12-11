(defpackage :kiln/scripts/eval-file
  (:use :cl)
  (:documentation "Eval arguments as Lisp files"))
(in-package :kiln/scripts/eval-file)

(defun main (args)
  (let ((*package* (find-package :cl-user)))
    (dolist (file args)
      (unless (uiop:file-exists-p file)
        (error "No such file as ~a" file))
      (uiop:with-temporary-file (:pathname tmp-file)
        (compile-file file :output-file tmp-file)
        (load tmp-file)))))
