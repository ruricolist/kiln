(defpackage :kiln/scripts/help
  (:use :cl :alexandria :serapeum :kiln/path)
  (:documentation "Print brief information about what scripts do."))
(in-package :kiln/scripts/help)

(defun format-script-help (script &key one-line (indent 0))
  (when-let (docs (script-package-docs script))
    (format t "~@[~a:~]~@[~vt~]~a~%"
            (and one-line
                 (script-name script))
            (and (> indent 0) indent)
            (let ((docs (script-package-docs script)))
              (if one-line
                  (first (lines docs :count 1))
                  docs)))))

(defun main (args)
  (destructuring-bind (&optional (script nil script-supplied?))
      args
    (let ((all-scripts
            (sort-new (list-all-scripts)
                      #'string<
                      :key #'script-name)))
      (if script-supplied?
          (when-let (script (find script all-scripts :key #'script-name :test #'equal))
            (format-script-help script))
          (multiple-value-bind (helpful helpless)
              (partition #'script-package-docs all-scripts)
            (let ((indent (+ 2 (reduce #'max helpful :key (op (length (script-name _)))))))
              (map nil (op (format-script-help _ :one-line t :indent indent)) helpful))
            (when helpless
              (format t "No help: ~{~a~^, ~}~%"
                      (map 'list #'script-name helpless))))))))
