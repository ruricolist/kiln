(defpackage :kiln/utils
  (:documentation "Utilities for writing scripts")
  (:use :cl :alexandria :serapeum :named-readtables)
  (:import-from :cffi)
  (:import-from :cmd :resolve-dir)
  (:import-from :cl-ppcre)
  (:import-from :kiln/dispatch :exec :invoke-script)
  (:import-from :kiln/flags :dbg)
  (:local-nicknames
   (:interpol :cl-interpol))
  ;; TODO Remove when Quicklisp updates
  (:shadow :parse-cmd-dsl)
  (:export
   :do-lines
   :walk-lines
   :fields
   :cd
   :red
   :green
   :yellow
   :bold
   :tty?
   :interpolate-escapes
   :invoke-script
   :invoke-argv
   :exec))
(in-package :kiln/utils)
(in-readtable :interpol-syntax)

(defun walk-lines (fn &optional (source *standard-input*))
  (etypecase source
    (null
     (walk-lines fn *standard-input*))
    (stream
     (fbind fn
       (loop for line = (read-line source nil nil)
             while line
             do (fn line))))
    (string
     (with-input-from-string (stream source)
       (walk-lines fn stream)))))

(defmacro do-lines ((line &optional source return) &body body)
  `(do-lines-1 (,line ,source ,return)
     ,@body))

(define-do-macro do-lines-1 ((line source &optional return) &body body)
  (with-thunk (body line)
    `(walk-lines ,body ,source)))

(defun fields (string &optional (split-on #'whitespacep))
  (etypecase split-on
    (function
     (split-sequence-if split-on string))
    ((eql #\Space)
     (fields string #'whitespacep))
    (character
     (split-sequence split-on string))
    (string
     (if (single split-on)
         (split-sequence (character split-on) string)
         (cl-ppcre:split split-on string)))))

(defun cd (&optional (dir (user-homedir-pathname)))
  (let ((dir (resolve-dir dir)))
    (uiop:chdir dir)
    (setf *default-pathname-defaults* dir)))

(defun tty? ()
  "Return T if there is a controlling TTY."
  (handler-case
      (progn
        (open #p"/dev/tty" :direction :probe)
        t)
    (file-error () nil)))

(declaim (ftype (function (string) string)
                red green yellow bold
                interpolate-escapes))

(defun red (s)
  (if (no (tty?)) s
      (string+ #?"\x1b[0;31m" s #?"\x1b[0m")))

(defun green (s)
  (if (no (tty?)) s
      (string+ #?"\x1b[0;32m" s #?"\x1b[0m")))

(defun orange (s)
  (if (no (tty?)) s
      (string+ #?"\x1b[0;35m" s #?"\x1b[0m")))

(defun yellow (s)
  (if (no (tty?)) s
      (string+ #?"\x1b[1;33m" s #?"\x1b[0m")))

(defun bold (s)
  (if (no (tty?)) s
      (string+ #?"\x1b[1m" s #?"\x1b[0m")))

(defun interpolate-escapes (string)
  (let ((interpol:*inner-delimiters* nil)
        (*readtable*
          (find-readtable :interpol-syntax))
        (*read-eval* nil))
    (assure string
      (with-input-from-string (in (string+ "#?\"" string #\"))
        (read in)))))

(defun invoke-argv (argv)
  "Invoke another script."
  (apply #'invoke-script (first argv) (rest argv)))

(cffi:defcfun (%execv "execv") :int
  (path :string)
  (args (:pointer :string)))

(defun execv (executable arglist)
  (assert (every #'stringp arglist))
  (cffi:with-foreign-string (path executable)
    (let ((argv
            ;; We don't care about cleanup, the end is nigh.
            (cffi:foreign-alloc :string
                                :initial-contents
                                arglist
                                :null-terminated-p t)))
      (when-let (code (%execv path argv))
        (error "execv(3) returned: ~a" code)))))

(defun execvp (executable arglist)
  (let ((executable
          (namestring
           (resolve-executable executable))))
    (execv executable arglist)))

(defun exec-no-dsl (command &key (unwind t))
  (dbg "Replacing current process with ~{~a~^ ~}"
       command)
  (if unwind
      (throw 'exec
        (lambda ()
          (exec-no-dsl command :unwind nil)))
      (execvp (car command) command)))

;;; TODO Remove when Quicklisp updates.
(defun parse-cmd-dsl (command)
  (let ((cmd (cmd::parse-cmd command)))
    (values (cmd::flatten-string-tokens (cmd::cmd-argv cmd))
            (cmd::flatten-string-tokens (cmd::cmd-kwargs cmd)))))

(defun exec (&rest command)
  "Replace the current process with COMMAND.
COMMAND is parsed as if by `cmd:cmd'.

Unless `:unwind nil` is passed as part of COMMAND, `unwind-protect'
forms in the user's program are run before the current process is
replaced. Otherwise the program is simply replaced without unwinding.

This is like the `exec' shell built-in, rather than `execvp(3)`, in
that `arg0' is set automatically."
  (mvlet* ((command kwargs (parse-cmd-dsl command))
           (unwind (getf kwargs :unwind t))
           (kwargs (remove-from-plist kwargs :unwind)))
    (when kwargs
      (error "Keyword arguments not supported by ~s: ~a"
             'exec
             kwargs))
    (exec-no-dsl command :unwind unwind)))
