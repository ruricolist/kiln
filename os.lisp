(defpackage :kiln/os
  (:documentation "Utilities for OS interface")
  (:use
   :cl
   :alexandria
   :serapeum)
  (:import-from :cffi)
  (:import-from :cmd)
  (:import-from :kiln/dispatch :exec)
  (:import-from :kiln/flags :dbg)
  (:export
   :cd
   :exec
   :setpgrp))
(in-package :kiln/os)

#.(if (uiop:os-unix-p)
      '(cffi:defcfun "setpgrp" :int)
      '(defun setpgrp ()))

(defun cd (&optional (dir (user-homedir-pathname)))
  "Set the operating system directory and sync
`*default-pathname-defaults*' to it."
  (let ((dir (cmd::resolve-dir dir)))
    (uiop:chdir dir)
    (setf *default-pathname-defaults* dir)))

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
