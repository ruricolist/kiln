(defpackage :kiln/scripts/math
  (:use :cl :alexandria :serapeum :with-c-syntax
        :named-readtables)
  (:import-from :floating-point-contractions
   :lg :lb :ln :sq :hypot)
  (:local-nicknames
   (:c :with-c-syntax)
   (:fp :floating-point-contractions))
  (:documentation "Do math using C-style syntax.")
  (:export :main))
(in-package :kiln/scripts/math)

(defun main (args)
  (let* ((string (string-join args ""))
         ;; Print double floats without suffix.
         (*read-default-float-format* 'double-float)
         (*readtable*
           (find-readtable 'c:with-c-syntax-readtable))
         (*package*
           (find-package :kiln/scripts/math)))
    (format t "~a~%"
            (handler-bind ((warning #'muffle-warning))
              (eval
               (read-from-string
                (string+ "#2{" string "}#")))))))

(defsubst log1p (x) (fp:log1+ x))
(defsubst log1m (x) (fp:log1- x))
(defsubst expm1 (x) (fp:exp-1 x))
(defsubst exptm1 (a z) (fp:expt-1 a z))
(defsubst log10 (x) (lg x))
(defsubst log2 (x) (lb x))
