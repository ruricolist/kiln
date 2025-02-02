(defpackage :kiln/scripts/apropos
  (:use :cl :alexandria :serapeum :kiln/path)
  (:documentation "Scan Kiln scripts for keyword matches"))
(in-package :kiln/scripts/apropos)

(defun main (args)
  (let* ((tokens (mapcar #'string-downcase args))
         (query (lambda (string)
                  (let ((string (string-downcase string)))
                    (some (lambda (token)
                            (string~= token string))
                          args))))
         (matches
           (filter (lambda (script)
                     (or (funcall query (script-name script))
                         (when-let (desc (script-package-docs script))
                           (funcall query desc))))
                   (list-all-scripts))))
    (dolist (m matches)
      (format t "~a~%" (script-name m)))))
