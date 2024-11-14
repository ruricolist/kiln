(defpackage :kiln/flags
  (:use :cl :serapeum :alexandria)
  (:export
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

(defplace exit-code ()
  *exit-code*)

(defun set-flags (value)
  (setf *flags*
        (filter-map #'find-keyword
                    (mapcar #'string-upcase
                            (mapcar (op (drop-prefix "--" _))
                                    value))))
  (when (memq :debug *flags*)
    (setf (uiop:getenv "KILN_DEBUG") "1"))
  *flags*)

(defun portable? ()
  (memq :portable *flags*))

(defun dbg? ()
  (or (memq :debug *flags*)
      (uiop:getenvp "KILN_DEBUG")))

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
