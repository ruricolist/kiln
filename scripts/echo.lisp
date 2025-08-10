(defpackage :kiln/scripts/echo
  (:use :cl :alexandria :serapeum :kiln/utils)
  (:local-nicknames
   (:args :kiln/args)
   (:cli :clingon))
  (:export :main)
  (:documentation "Echo arguments to standard output"))
(in-package :kiln/scripts/echo)

(def options
  (list
   (cli:make-option
    :flag
    :short-name #\n
    :description "do not output trailing newline"
    :key :n)
   (cli:make-option
    :flag
    :short-name #\e
    :description "enable interpolation of backslashes"
    :key :e)))

(def command
  (cli:make-command
   :name "kiln-echo"
   :description "Echo arguments to standard output."
   :long-description "Echo arguments to standard output, separated by spaces.
Accepts -- to signal the end of the options."
   :options options))

(defun main (args)
  (args:with-argument-destructuring
      (&rest strings
       &key (n nil no-newline) (e nil interpolate))
      (:argv args)
    (declare (ignore n e))
    (let* ((stdout *standard-output*))
      (list interpolate no-newline)
      (with-boolean (interpolate)
        (loop for (string . more?) on strings
              do (:if interpolate
                      (write-string (interpolate-escapes string) stdout)
                      (write-string string stdout))
                 (when more?
                   (write-char #\Space stdout))))
      (unless no-newline
        (terpri)))))
