(defpackage :kiln/scripts/math
  (:use :cl :alexandria :serapeum :with-c-syntax
        :named-readtables)
  (:import-from :floating-point-contractions
   :lg :lb :ln :sq :hypot)
  (:local-nicknames
   (:c :with-c-syntax)
   (:cli :clingon)
   (:fp :floating-point-contractions))
  (:documentation "Do math with C-style syntax but a Lisp numeric tower")
  (:export :main))
(in-package :kiln/scripts/math)

(def +options+
  (list
   (cli:make-option
    :flag
    :description "Print ratios"
    :short-name #\R
    :long-name "print-ratios"
    :key :ratiop)))

(def +command+
  (cli:make-command
   :name "kiln-math"
   :options +options+))

(defun main (args)
  (let* ((opts (cli:parse-command-line +command+ args))
         (args (cli:command-arguments opts))
         (string (string-join args ""))
         ;; Print double floats without suffix.
         (*read-default-float-format* 'double-float)
         (*readtable*
           (find-readtable 'c:with-c-syntax-readtable))
         (*package*
           (find-package :kiln/scripts/math))
         (result
           (handler-bind ((warning #'muffle-warning))
             (eval
              (read-from-string
               ;; 2 means to split C operators inside Lisp symbols.
               (string+ "#2{" string "}#")))))
         (result
           (if (typep result 'ratio)
               (if (cli:getopt opts :ratiop)
                   result
                   (coerce result *read-default-float-format*))
               result)))
    (format t "~a~%" result)))

(defsubst log1p (x) (fp:log1+ x))
(defsubst log1m (x) (fp:log1- x))
(defsubst expm1 (x) (fp:exp-1 x))
(defsubst exptm1 (a z) (fp:expt-1 a z))
(defsubst log10 (x) (lg x))
(defsubst log2 (x) (lb x))
