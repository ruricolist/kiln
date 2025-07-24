(defpackage :kiln/scripts/rebuild
  (:use :cl :alexandria :serapeum :cmd
        :kiln/path :kiln/flags
        :lisp-invocation)
  (:local-nicknames
   (:cli :clingon))
  (:import-from :kiln/os :exec)
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
    :env-vars (list +kiln-lisp+)
    :key :lisp)
   (cli:make-option
    :string
    :description "Target system"
    :long-name "target-system"
    :key :target-system)
   (cli:make-option
    :string
    :description "Target package (defaults to system)"
    :long-name "target-package"
    :key :target-package)
   (cli:make-option
    :string
    :description "Executable to generate"
    :long-name "target-file"
    :short-name #\o
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
    :description "Lisp heap size (MB)"
    :initial-value nil
    :env-vars (list +kiln-heap-size+)
    :key :heap-size)
   (cli:make-option
    :integer
    :long-name "stack-size"
    :description "Lisp stack size (MB)"
    :initial-value nil
    :env-vars (list +kiln-stack-size+)
    :key :stack-size)
   (cli:make-option
    :integer
    :long-name "nursery-size-limit"
    :description "Nursery size limit (B)"
    :initial-value nil
    :env-vars (list +kiln-nursery-max-bytes+)
    :key :nursery-max-size)
   (cli:make-option
    :flag
    :description "Use Quicklisp"
    :long-name "quicklisp"
    :initial-value :false
    :env-vars (list +kiln-quicklisp+)
    :key :quicklisp)
   (cli:make-option
    :flag
    :description "Skip systems that fail to compile"
    :long-name "tolerant"
    :initial-value :false
    :env-vars (list +kiln-tolerant+)
    :key :tolerant)
   (cli:make-option
    :flag
    :description "Use POIU"
    :long-name "poiu"
    :initial-value :false
    :env-vars (list +kiln-poiu+)
    :key :poiu)))

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
      (setf (getenv +kiln-lisp+) lisp))
    (when-let* ((target-system (cli:getopt opts :target-system))
                (target-package
                 (or (cli:getopt opts :target-package)
                     (string-invert-case target-system))))
      (setf (getenv +kiln-target-system+) target-system
            (getenv +kiln-target-package+) target-package))
    (when (cli:getopt opts :target-package)
      (error "Cannot provide --target-package without --target-system"))
    (when-let (target-file (cli:getopt opts :target-file))
      (setf (getenv +kiln-target-file+)
            (uiop:native-namestring
             (path-join *default-pathname-defaults* target-file))))
    (when (or (cli:getopt opts :no-version)
              (cli:getopt opts :target-system))
      (setf (getenv +kiln-no-print-version+) "1"))
    (when-let (heap-size (cli:getopt opts :heap-size))
      (setf (getenv +kiln-heap-size+)
            (princ-to-string heap-size)))
    (when-let (stack-size (cli:getopt opts :stack-size))
      (setf (getenv +kiln-stack-size+)
            (princ-to-string stack-size)))
    (when-let (nursery-size-limit (cli:getopt opts :nursery-size-limit))
      (setf (getenv +kiln-nursery-max-bytes+)
            (princ-to-string nursery-size-limit)))
    (when (cli:getopt opts :quicklisp)
      (setf (getenv +kiln-quicklisp+)
            (if (find-package :ql)
                (asdf:system-relative-pathname "quicklisp"
                                               "../setup.lisp")
                (error "Quicklisp requested but not available"))))
    (when (cli:getopt opts :tolerant)
      (setf (getenv +kiln-tolerant+) "1"))
    (when (cli:getopt opts :poiu)
      (setf (getenv +kiln-poiu+) "1"))
    (let ((path (asdf:system-relative-pathname "kiln" "")))
      (uiop:chdir (namestring path))
      (exec "sh build.sh"))))
