(defpackage :kiln/flags
  (:use :cl :serapeum :alexandria)
  (:import-from :kiln/os :getenv :getenvp)
  (:export
    :+kiln-debug+
    :+kiln-heap-size+
    :+kiln-lisp+
    :+kiln-no-print-version+
    :+kiln-stack-size+
    :+kiln-path-systems+
    :+kiln-poiu+
    :+kiln-quicklisp+
    :+kiln-target-file+
    :+kiln-target-package+
    :+kiln-target-system+
    :+kiln-tolerant+
    :*flags*
    :dbg?
    :dbg
    :set-flags
    :portable?
    :repl-on-error?
    :exit-code
    :backtrace?
    :with-debug-output))
(in-package :kiln/flags)

(defvar *flags* nil)

(defvar *exit-code* 0)

(def +kiln-debug+ "KILN_DEBUG")
(def +kiln-heap-size+ "KILN_HEAP_SIZE")
(def +kiln-lisp+ "KILN_LISP")
(def +kiln-no-print-version+ "KILN_NO_PRINT_VERSION")
(def +kiln-stack-size+ "KILN_STACK_SIZE")
(def +kiln-path-systems+ "KILN_PATH_SYSTEMS")
(def +kiln-poiu+ "KILN_POIU")
(def +kiln-quicklisp+ "KILN_QUICKLISP")
(def +kiln-target-file+ "KILN_TARGET_FILE")
(def +kiln-target-package+ "KILN_TARGET_PACKAGE")
(def +kiln-target-system+ "KILN_TARGET_SYSTEM")
(def +kiln-tolerant+ "KILN_TOLERANT")

(defplace exit-code ()
  *exit-code*)

(defun set-flags (value)
  (setf *flags*
        (filter-map #'find-keyword
                    (mapcar #'string-upcase
                            (mapcar (op (drop-prefix "--" _))
                                    value))))
  (when (memq :debug *flags*)
    (setf (getenv "KILN_DEBUG") "1"))
  *flags*)

(defun portable? ()
  (memq :portable *flags*))

(defun dbg? ()
  (or (memq :debug *flags*)
      (getenvp "KILN_DEBUG")))

(defun call/debug-output (fn)
  (let ((*standard-output*
          (if (dbg?) *standard-output*
              (make-broadcast-stream)))
        (*error-output*
          (if (dbg?) *error-output*
              (make-broadcast-stream))))
    (funcall fn)))

(defmacro with-debug-output ((&key) &body body)
  `(call/debug-output (lambda () ,@body)))

(defun (setf dbg?) (value)
  (if (assure boolean value)
      (pushnew :debug *flags*)
      (alexandria:removef *flags* :debug)))

(defun backtrace? ()
  (or (dbg?) (memq :backtrace *flags*)))

(defun repl-on-error? ()
  (memq :repl-on-error *flags*))

(defun dbg (control-string &rest args)
  (when (dbg?)
    (format *error-output* "~&~?~%" control-string args)))

(define-compiler-macro dbg (&whole call control-string &rest args)
  (if (stringp control-string)
      `(when (dbg?)
         (format *error-output* "~&~@?~%" ,control-string ,@args))
      call))
