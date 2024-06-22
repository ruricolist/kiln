(defpackage :kiln/scripts/echo
  (:use :cl :alexandria :serapeum :kiln/utils)
  (:local-nicknames (:cli :clingon))
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
  (let* ((pure-args (rest (member "--" args :test #'equal)))
         (opt-args (ldiff args pure-args))
         (opts (cli:parse-command-line command opt-args))
         (strings
           (append
            (cli:command-arguments opts)
            pure-args))
         (no-newline (cli:getopt opts :n))
         (interpolate (cli:getopt opts :e))
         (stdout *standard-output*))
    (with-boolean (interpolate)
      (loop for (string . more?) on strings
            do (:if interpolate
                    (write-string (interpolate-escapes string) stdout)
                    (write-string string stdout))
               (when more?
                 (write-char #\Space stdout))))
    (unless no-newline
      (terpri))))
