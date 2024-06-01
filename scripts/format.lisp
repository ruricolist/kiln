(defpackage :kiln/scripts/format
  (:use :cl :alexandria :serapeum :kiln)
  (:import-from :clingon)
  (:documentation "Use format like awk"))
(in-package :kiln/scripts/format)

(def options
  (list
   (clingon:make-option
    :string
    :description "Field separator (char or regexp)"
    :initial-value " "
    :short-name #\F
    :long-name "field-separator"
    :key :field-separator)
   (clingon:make-option
    :flag
    :description "Treat arguments as a list"
    :short-name #\L
    :long-name "list"
    :key :list)))

(def command
  (clingon:make-command
   :name "kiln-format"
   :description "Use format like AWK"
   :options options))

(defun main (args)
  (let* ((opts (clingon:parse-command-line command args))
         (field-separator (clingon:getopt opts :field-separator #\Space)))
    (destructuring-bind (control-string)
        (clingon:command-arguments command)
      (let ((list? (clingon:getopt opts :list)))
        (with-boolean (list?)
          (let ((formatter (compile nil (eval `(formatter ,control-string)))))
            (do-lines (line)
              (let ((fields (fields line field-separator)))
                (:if list?
                     (format t formatter fields)
                     (format t "~?" formatter fields))))))))))
