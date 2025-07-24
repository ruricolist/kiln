(defpackage :kiln/os
  (:documentation "Utilities for OS interface")
  (:use
   :cl
   :alexandria
   :cmd
   :serapeum)
  (:import-from :cffi)
  (:import-from :cmd)
  (:import-from :kiln/dispatch :exec :exit)
  (:import-from :kiln/flags :dbg)
  (:import-from
   :uiop
   :file-exists-p
   :directory-exists-p
   :getenv
   :getenvp
   :hostname)
  (:shadow
   ;; TODO Remove when Quicklisp updates
   :parse-cmd-dsl)
  (:export
   :chdir
   :directory-exists-p
   :exec
   :exit
   :file-exists-p
   :getcwd
   :getenv
   :getenvp
   :getpid
   :hostname
   :os-linux-p
   :setpgrp
   :with-chdir
   :with-save-directory))
(in-package :kiln/os)

#.(if (uiop:os-unix-p)
      '(cffi:defcfun "setpgrp" :int)
      '(defun setpgrp ()))

(defun os-linux-p ()
  (and (uiop:os-unix-p)
       (search "linux" ($cmd "uname") :test #'char-equal)))

(setf (documentation #'setpgrp 'function)
      "Make this process the leader of a new process group.")

(defun chdir (&optional (dir (user-homedir-pathname)))
  "Set the operating system directory and sync
`*default-pathname-defaults*' to it."
  (let ((dir (cmd::resolve-dir dir)))
    (uiop:chdir dir)
    (setf *default-pathname-defaults* dir)))

(defun getcwd ()
  (uiop:getcwd))

(defun (setf getcwd) (dir)
  (chdir dir))

(defun call/save-directory (fn)
  (let ((start-dir (uiop:getcwd)))
    (unwind-protect
         (funcall fn)
      (chdir start-dir))))

(defmacro with-save-directory ((&key) &body body)
  "Run BODY, restoring the current directory afterwards."
  (with-thunk (body)
    `(call/save-directory ,body)))

(defmacro with-chdir ((dir) &body body)
  "Set current directory to DIR, run BODY, restore current directory."
  `(with-save-directory ()
     (chdir ,dir)
     ,@body))

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

(defun exec-no-dsl (command &key (unwind t) (log t))
  (when log
    (dbg "Replacing current process with ~{~a~^ ~}"
         command))
  (if unwind
      (throw 'exec
        (lambda ()
          (exec-no-dsl command :unwind nil :log nil)))
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

(defun exit (&key (code 0) (unwind t))
  "Exit with code CODE."
  (check-type code integer)
  (if unwind
      (throw 'exit code)
      (uiop:quit code)))

(defun getpid ()
  ;; Adapted from the sources of Sly. Should there be a trivial-getpid
  ;; library?
  #+ccl (ccl::getpid)
  #+sbcl (sb-posix:getpid)
  #+ecl (ext:getpid)
  #+clisp (os:process-id)
  #+cmucl (unix:unix-getpid)
  #+abcl (ext:get-pid)
  #+allegro (excl.osi:getpid)
  #+(and lispworks win32) (win32:get-current-process-id)
  #+(and lispworks (not win32))
  (system::getpid)
  #+mkcl (mkcl:getpid)
  #+scl (unix:unix-getpid)
  #+clasp (si:getpid)
  #+cormanlisp ccl:*current-process-id*)
