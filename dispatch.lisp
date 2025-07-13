(defpackage :kiln/dispatch
  (:use
   :cl :alexandria :serapeum :named-readtables
   :kiln/stamp :kiln/path :kiln/system
   :kiln/flags
   :kiln/hot-reload
   :kiln/script-cache
   :with-user-abort)
  (:import-from :cmd)
  (:import-from :kiln/user)
  (:import-from :uiop)
  (:export
   :*entry-point*
   :dispatch
   :exec
   :invoke-entry-point
   :invoke-script
   :missing-main
   :no-such-package
   :no-such-script
   :script-error
   :script-error.name
   :script-package-error
   :script-package-error.package))
(in-package :kiln/dispatch)

(deftype script-search-result ()
  '(or script asdf:system null))

(deftype args-state ()
  '(member :flags :script-name :script-args))

(defparameter *entry-point* 'dispatch/argv)

(-> parse-args (list) (values list (or null string) list))
(defun parse-args (args)
  (nlet parse-args ((args args)
                    (state :flags)
                    (flags nil)
                    (script-name nil)
                    (script-args nil))
    (if (no args)
        (values (nreverse flags)
                script-name
                (nreverse script-args))
        (ecase-of args-state state
          (:flags
           (if (string^= "--" (first args))
               (parse-args (rest args)
                           :flags
                           (cons (first args) flags)
                           script-name
                           script-args)
               (parse-args args
                           :script-name
                           flags
                           script-name
                           script-args)))
          (:script-name
           (parse-args (rest args)
                       :script-args
                       flags
                       (first args)
                       script-args))
          (:script-args
           (parse-args (rest args)
                       :script-args
                       flags
                       script-name
                       (cons (first args) script-args)))))))

(defun find-script (script-name)
  (trivia:ematch script-name
    ((and script (type script))
     script)
    ((and script-name (type string))
     (let ((builtins-by-system-table *builtins-by-system-table*)
           (portable? (portable?)))
       (with-boolean (portable?)
         (flet ((find-script-in-system (system-name script-name)
                  (or (when-let (system-scripts (gethash system-name builtins-by-system-table))
                        (gethash script-name system-scripts))
                      (:unless portable?
                        (asdf:find-system (string+ system-name "/" script-name)
                                          nil)))))
           (some (op (find-script-in-system _ script-name))
                 (scripts-path))))))))

(defun find-script-package (script)
  (let* ((script-name (script-name script))
         (script-subsystem (script-subsystem script))
         (package-name
           ;; TODO "Modern" mode?
           (string-upcase script-subsystem)))
    (or (find-package package-name)
        (if (portable?)
            (error 'no-such-script :name script-name)
            (progn
              (dbg "Could not find ~a, attempting to load" script-name)
              ;; If the system is not present in the
              ;; image, try to load it anyway.
              (load-system script-subsystem)
              (or (find-package package-name)
                  (error 'no-such-package :package package-name)))))))

(defun script-changed? (script)
  (with-accessors ((path script-path)
                   (stamp script-stamp)
                   (subsystem script-subsystem))
      script
    (dbg "Path: ~a" path)
    (when (uiop:file-exists-p path)
      (let ((new-stamp (file-stamp path)))
        (dbg "Old stamp: ~a" stamp)
        (dbg "New stamp: ~a" new-stamp)
        (not (stamp= new-stamp stamp))))))

(define-condition script-error (error)
  ((name :initarg :name :reader script-error.name)))

(define-condition no-such-script (script-error)
  ()
  (:report (lambda (c s)
             (with-slots (name) c
               (format s "No such script as ~a" name)))))

(define-condition script-package-error (script-error)
  ((package :initarg :package :reader script-package-error.package)))

(define-condition no-such-package (script-package-error)
  ()
  (:report (lambda (c s)
             (with-slots (package) c
               (format s "No such package as ~a" package)))))

(define-condition missing-main (script-package-error)
  ()
  (:report (lambda (c s)
             (with-slots (package) c
               (format s "No main function defined in ~a" package)))))

(define-condition no-arguments (script-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "No arguments.~%Try one of: ")
             (format s "~{~(~a~)~^ ~}~%"
                     (sort
                      (copy-list (list-loaded-scripts))
                      #'string<)))))

(defgeneric print-error (error stream)
  (:documentation "Print ERROR to STREAM")
  (:method (e stream)
    (format stream "~a" e)))

(defgeneric error-exit-code (error)
  (:method ((e t)) 1)
  (:method ((e uiop:subprocess-error))
    "If there is an unhandled subprocess error, return with its exit code."
    (or (uiop:subprocess-error-code e)
        (call-next-method)))
  ;; See https://tldp.org/LDP/abs/html/exitcodes.html
  (:method ((e no-arguments)) 2)
  (:method ((e no-such-package)) 126)      ;command cannot execute
  (:method ((e missing-main)) 126)      ;command cannot execute
  (:method ((e no-such-script)) 127)    ;command not found
  (:method ((e user-abort)) 130))

(defun invoke-script (script-name &rest args)
  "Invoke another script"
  (dispatch/package script-name args))

(defun call-main (main-fn args)
  (let ((result
          (catch 'exit
            (funcall main-fn args))))
    (if (integerp result)
        (progn
          (setf (exit-code) result)
          result)
        result)))

(defun dispatch/package (script-name args)
  (labels ((run-package (package-name)
             (let* ((package
                      (or (find-package package-name)
                          (error "No such package as ~a" package-name)))
                    (main-sym
                      (or (find-symbol (string 'main) package)
                          (error "Package ~a does not define ~a"
                                 package 'main)))
                    (main-fn (symbol-function main-sym))
                    ;; Use the script's package when running.
                    (*package* package))
               (call-main main-fn args))))
    (let ((result (find-script script-name)))
      (etypecase-of script-search-result result
        (script
         (let* ((script result)
                (package (find-script-package script)))
           ;; If the script has changed since the image was built,
           ;; reload it. (Dependent systems won't be reloaded
           ;; unless they are new.)
           (when (script-changed? script)
             (dbg "Unequal stamps")
             (hot-reload (script-subsystem script)
                         (script-package script)
                         (script-path script)))
           (run-package package)))
        (asdf:system
         (assert (not (portable?)))
         ;; TODO Handle package variance?
         (let* ((subsystem (asdf:component-name result)))
           (load-system subsystem)
           (run-package (string-upcase subsystem))))
        (null
         (error 'no-such-script :name script-name))))))

(defun dispatch/shebang (script args)
  (let ((script-path (path-join (uiop:getcwd) script))
        (*package* (find-package :kiln-user))
        (*readtable* (find-readtable 'kiln-user:kiln-readtable)))
    (run-cached-script script-path args)))

(defun dispatch/argv (&aux (args (uiop:command-line-arguments)))
  (let ((executable-name
          (pathname-name
           (uiop:parse-native-namestring
            (uiop:argv0)))))
    (cond
      ;; Multicall behavior
      ((find-script executable-name)
       (dispatch/package executable-name args))
      ((find-script (drop-prefix "kiln-" executable-name))
       (dispatch/package (drop-prefix "kiln-" executable-name) args))
      ((no args) (error 'no-arguments))
      ;; Run a subcommand.
      (t
       (multiple-value-bind (flags script-name script-args)
           (parse-args args)
         (set-flags flags)
         (dbg "Flags: ~a~%Script name: ~a~%Args: ~a" *flags* script-name args)
         (if (no script-name)
             (error "No script name")
             (if (find #\/ script-name)
                 (dispatch/shebang script-name script-args)
                 (dispatch/package script-name script-args))))))))

(defun invoke-entry-point ()
  "Invoke the function set as `*entry-point*' in an appropriate dynamic
environment.

This is intended to be called once and sets up variables, streams, and
handlers to give the desired behavior for scripting."
  (let* ((*standard-input* uiop:*stdin*)
         (*standard-output* uiop:*stdout*)
         (*error-output* uiop:*stderr*)
         (*trace-output* uiop:*stderr*)
         ;; Cf SBCL process-script.
         (*terminal-io* (make-two-way-stream *standard-input* *error-output*))
         (*debug-io* (make-two-way-stream *standard-input* *error-output*))
         (dbg? (dbg?))
         (*compile-verbose* dbg?)
         (*compile-print* dbg?)
         (*load-verbose* dbg?)
         (*load-print* dbg?))
    (labels ((kill-other-threads ()
               (let* ((current-thread (bt:current-thread))
                      (other-threads (remove current-thread (bt:all-threads))))
                 (dolist (thread other-threads)
                   (bt:destroy-thread thread))))
             (quit (exit-code)
               (unless (zerop exit-code)
                 (kill-other-threads))
               ;; NB `uiop:quit' implicitly finishes outputs.
               (uiop:quit exit-code))
             (print-error-and-backtrace (e)
               (let* ((c (type-of e)))
                 (format *error-output* "~&~@(~a~): "
                         (substitute #\Space #\- (string c))))
               (print-error e *error-output*)
               (format *error-output* "~&")
               (when (backtrace?)
                 (uiop:print-backtrace :condition e :stream *error-output*))))
      (handler-bind (#+sbcl ((or style-warning sb-ext:compiler-note) #'muffle-warning)
                     ((or #+sbcl sb-int:broken-pipe end-of-file)
                       (lambda (e)
                         ;; Shell-style.
                         (when (member (stream-error-stream e)
                                       (list *standard-input*
                                             *standard-output*
                                             *error-output*))
                           (quit 1)))))
        ;; Refresh the default pathname to the current directory.
        (setf *default-pathname-defaults*
              (uiop:getcwd))
        ;; Refresh the random state.
        (setf *random-state* (make-random-state t))
        ;; Nest handler-case and handler-bind because `uiop:quit' is
        ;; not guaranteed to unwind \(and we want a backtrace if
        ;; `--debug' is set).
        (handler-case
            (handler-bind ((serious-condition
                             (lambda (e)
                               (unless (typep e '(or user-abort
                                                  #+sbcl sb-int:broken-pipe
                                                  end-of-file))
                                 (print-error-and-backtrace e))
                               (when (repl-on-error?)
                                 (invoke-script "repl")))))
              (with-user-abort
                  (funcall
                   (catch 'exec
                     (constantly (funcall *entry-point*))))
                (quit (exit-code))))
          (user-abort (e)
            (when (backtrace?)
              (print-error-and-backtrace e))
            ;; Resignal so the process finally gets killed by a
            ;; signal and WIFSIGNALED can detect it.
            #+sbcl
            (progn
              (sb-sys:enable-interrupt sb-unix:sigint :default)
              (sb-posix:kill 0 sb-unix:sigint))
            #-sbcl
            (signal e))
          (serious-condition (e)
            (quit (error-exit-code e))))))))
