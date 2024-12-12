(defpackage :kiln/scripts/load-file
  (:use :cl)
  (:documentation "Compile and load file arguments"))
(in-package :kiln/scripts/load-file)

(defun main (args)
  (with-standard-io-syntax
    (dolist (file args)
      (unless (uiop:file-exists-p file)
        (error "No such file as ~a" file))
      (uiop:with-temporary-file (:pathname tmp-file)
        (compile-file file :output-file tmp-file)
        (load tmp-file)))))
