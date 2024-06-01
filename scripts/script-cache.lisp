(defpackage :kiln/scripts/script-cache
  (:use :cl :alexandria :serapeum :kiln/script-cache)
  (:documentation "Inspect script cache"))
(in-package :kiln/scripts/script-cache)

(defun main (args)
  (econd ((equal args '("--list"))
          (print (hash-table-keys *script-cache*)))))
