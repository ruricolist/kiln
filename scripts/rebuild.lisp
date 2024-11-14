(defpackage :kiln/scripts/rebuild
  (:use :cl :alexandria :serapeum :cmd
        :kiln/path :kiln/flags
        :lisp-invocation)
  (:local-nicknames
   (:cli :clingon))
  (:import-from :kiln/utils :exec)
  (:import-from :uiop :getenv)
  (:documentation "Rebuild kiln executable")
  (:export :main))
(in-package :kiln/scripts/rebuild)

(def options
  (list
   (cli:make-option
    :string
    :description "Lisp implementation"
    :long-name "lisp"
    :key :lisp)
   (cli:make-option
    :string
    :description "Target system"
    :long-name "target-system"
    :key :target-system)
   (cli:make-option
    :string
    :description "Target file"
    :long-name "target-file"
    :key :target-file)
   (cli:make-option
    :flag
    :description "Print version"
    :long-name "no-version"
    :initial-value :false
    :key :no-version)
   (cli:make-option
    :integer
    :long-name "heap-size"
    :description "Lisp heap size"
    :initial-value nil
    :key :heap-size)
   (cli:make-option
    :integer
    :long-name "stack-size"
    :description "Lisp stack size (MB)"
    :initial-value nil
    :env-vars '("KILN_STACK_SIZE")
    :key :stack-size)
   (cli:make-option
    :flag
    :description "Quicklisp"
    :long-name "quicklisp"
    :initial-value :false
    :key :quicklisp)))

(def command
  (cli:make-command
   :name "kiln-rebuild"
   :description "Rebuild Kiln"
   :options options))

(defun main (args)
  (let ((opts (cli:parse-command-line command args)))
    (format *error-output* "Rebuilding~@[ ~a~]~%"
            (cli:getopt opts :target-file))
    (force-output *error-output*)
    (when-let (lisp (cli:getopt opts :lisp))
      (setf (getenv "LISP") lisp))
    (when-let (target-system (cli:getopt opts :target-system))
      (setf (getenv "KILN_TARGET_SYSTEM") target-system))
    (when-let (target-file (cli:getopt opts :target-file))
      (setf (getenv "KILN_TARGET_FILE") target-file))
    (when (cli:getopt opts :no-version)
      (setf (getenv "NO_PRINT_VERSION") "1"))
    (when-let (heap-size (cli:getopt opts :heap-size))
      (setf (getenv "KILN_HEAP_SIZE")
            (princ-to-string heap-size)))
    (when-let (stack-size (cli:getopt opts :stack-size))
      (setf (getenv "KILN_STACK_SIZE")
            (princ-to-string stack-size)))
    (when (cli:getopt opts :quicklisp)
      (setf (getenv "KILN_QUICKLISP")
            (if (find-package :ql)
                (asdf:system-relative-pathname "quicklisp"
                                               "../setup.lisp")
                (error "Quicklisp requested but not available"))))
    (let ((path (asdf:system-relative-pathname "kiln" "")))
      (uiop:chdir (namestring path))
      (exec "sh build.sh"))))
