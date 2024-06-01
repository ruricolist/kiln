(uiop:define-package :kiln/scripts/complete
  (:mix :cl :alexandria :serapeum :trivia)
  (:local-nicknames
   (:flags :kiln/flags)
   (:path :kiln/path))
  (:import-from :cmd)
  (:import-from :shlex))
(in-package :kiln/scripts/complete)

;;; To use in Bash: `command -C 'kiln complete'`.

(defun main (args)
  (declare (ignore args))
  (when-let ((line (uiop:getenvp "COMP_LINE"))
             (point (uiop:getenvp "COMP_POINT")))
    (let* ((point (parse-integer point))
           (before (take point line))
           (pre-tokens
             (shlex:split before :whitespace-split nil
                                 :punctuation-chars t)))
      (when pre-tokens
        (when (equal (first pre-tokens) (uiop:argv0))
          (let ((tokens (rest pre-tokens)))
            (let* ((script-names
                     (mapcar #'path:script-name
                             (path:list-all-scripts)))
                   (subcommand (first tokens))
                   (completions
                     (if (emptyp subcommand)
                         script-names
                         (filter (op (string^= subcommand _))
                                 script-names))))
              (format t "~{~a~%~}" completions))))))))
