(defpackage :kiln/scripts/self-test
  (:use :cl :alexandria :serapeum :cmd :fiveam :trivial-file-size)
  (:import-from :kiln/flags :dbg :dbg?)
  (:import-from :FiveAM :is)
  (:local-nicknames (:5am :fiveam))
  (:documentation "Perform self-test of Kiln itself"))
(in-package :kiln/scripts/self-test)

(defvar-unbound *self*
  "Kiln executable running these tests.")

(defun kiln-exact-output (&rest argv)
  "Run ARGV with Kiln, but without debug flag.
This is useful when we need to test the exact output."
  (apply #'$cmd "env KILN_DEBUG=" *self* argv))

(5am:in-suite* test)

(5am:test command
  (is (zerop (cmd *self* "command true" :&> nil)))
  (is (not (zerop (cmd *self* "command false"
                       :ignore-error-status t
                       :&> nil))))
  (is (= 128 (cmd *self* "command sh -c 'exit 128'"
                  :ignore-error-status t
                  :&> nil))))

(5am:test format
  (is (equal "x,y,z"
             (with-input-from-string (in "x y z")
               ($cmd *self* "format" (list "~a,~a,~a")
                     :< in))))
  (is (equal "x,y,z"
             (with-input-from-string (in "x y z")
               ($cmd *self* "format" (list "-L" "~{~a~^,~}")
                     :< in))))
  (is (equal "x,y,z"
             (with-input-from-string (in "x:y:z")
               ($cmd *self* "format" (list "-F:" "~a,~a,~a")
                     :< in)))))

(5am:test echo
  (is (equal "-n" ($cmd *self* "echo -n -- -n")))
  (is (equal #.(fmt "-n~%")
             (with-output-to-string (s)
               (cmd *self* "echo -- -n"
                    :output s)))))

(5am:test loop
  (let ((cmd:*cmd-env*
          '(("SHELL" . "/bin/sh"))))
    (is (equal "0 1 2 3 4 5"
               (substitute #\Space #\Newline
                           (kiln-exact-output
                            "loop for '@i' from 0 to 5 do '!echo $i'"))))))

(5am:test rebuild
  (terpri *error-output*)
  (uiop:with-temporary-file (:pathname p)
    (delete-file p)
    (finishes
      (cmd *self*
           (and (dbg?) (list "--debug"))
           "rebuild --target-file" p))
    (cmd "ls -alh" p)
    (finishes
      (cmd p "version"))))

(5am:test quit-unwinds
  (is (equal "unwound"
             ($cmd *self*
                   "eval"
                   (list
                    (prin1-to-string
                     `(unwind-protect
                           (uiop:quit 0)
                        (write-string "unwound")
                        (finish-output))))))))

(5am:test multicall
  (let ((version-results ($cmd *self* "version")))
    (uiop:with-temporary-file (:pathname dir)
      (dbg "Tmp: ~a" dir)
      (delete-file dir)
      (setf dir
            (ensure-directories-exist
             (make-pathname :name nil
                            :directory
                            (append1 (pathname-directory dir)
                                     (pathname-name dir)))))
      (dbg "Dir as dir: ~a" dir)
      (let ((simple-multicall (string+ (namestring dir) "/" "version")))
        (cmd "ln -s" *self* simple-multicall)
        (is (equal version-results ($cmd simple-multicall))))
      (let ((prefixed-multicall (string+ (namestring dir) "/" "kiln-version")))
        (cmd "ln -s" *self* prefixed-multicall)
        (is (equal version-results ($cmd prefixed-multicall)))))))

(defvar *source-registry* "")
(defvar *path-systems* '())

(defun call/templated-test-system (fn system-name &key (kiln-path t))
  "Copy SYSTEM from the appropriate template into a temporary directory and make it loadable."
  (declare (function fn) (string system-name))
  (assert (notany #'upper-case-p system-name))
  (uiop:with-temporary-file (:pathname tmp)
    (delete-file tmp)
    (let* ((tmpdir (ensure-directories-exist tmp))
           ;; TODO Must be a better way.
           (tmpdir (uiop:parse-unix-namestring
                    (string+ (uiop:unix-namestring tmpdir) "/")))
           (test-system-dir
             (ensure-directories-exist
              (path-join tmpdir
                         (make-pathname
                          :directory (list :relative system-name)))))
           (template-dir
             (asdf:system-relative-pathname
              :kiln
              (fmt "test/~a" system-name)))
           (source-files
             (uiop:directory-files template-dir)))
      (dolist (source-file source-files)
        (let* ((old-name (pathname-name source-file))
               (new-name (string-replace-all "-dot-" old-name "."))
               (dest (path-join test-system-dir (pathname new-name))))
          (assert (search "-dot-" old-name))
          (copy-file source-file dest)))
      (let* ((*source-registry*
               (if (emptyp *source-registry*)
                   (fmt "~a//:~a:"
                        (drop-suffix "/" (uiop:unix-namestring tmpdir))
                        (uiop:unix-namestring
                         (asdf:system-relative-pathname :kiln "")))
                   (fmt "~a//:~a"
                        (drop-suffix "/" (uiop:unix-namestring tmpdir))
                        *source-registry*)))
             (*path-systems*
               (if kiln-path
                   (cons system-name *path-systems*)
                   *path-systems*))
             (*cmd-env*
               `(("CL_SOURCE_REGISTRY" . ,*source-registry*)
                 ("KILN_PATH_SYSTEMS"
                  . ,(string-join *path-systems* ":")))))
        (funcall fn test-system-dir)))))

(defmacro with-templated-test-system ((&key name path (kiln-path t))
                                      &body body)
  (with-thunk (body path)
    `(call/templated-test-system ,body ,name :kiln-path ,kiln-path)))

(5am:test reload
  (with-templated-test-system (:name "kiln-test-system"
                               :path test-system-dir)
    (with-templated-test-system (:name "other-test-system"
                                 :path other-test-system-dir
                                 :kiln-path nil)
      (dbg ($cmd "env"))
      (let ((script-file (path-join test-system-dir #p"kiln-test-script.lisp")))
        (is (uiop:file-exists-p script-file))
        (let ((debug-flag (and (dbg?) (list "--debug"))))
          (dbg "Passing debug flag")
          (uiop:with-temporary-file (:pathname kiln)
            (delete-file kiln)
            (dbg "Building core")
            (cmd *self* debug-flag "rebuild --no-version --target-file" kiln
                 :&> *error-output*)
            (dbg "Built core")
            (is (uiop:file-exists-p kiln))
            (is (> (file-size-in-octets kiln) 0))
            (flet ((kiln-exact-output (&rest argv)
                     "Run ARGV with Kiln, but don't propagate debug flag."
                     (let ((*self* kiln))
                       (apply #'kiln-exact-output argv))))
              (is (search "kiln-test-system" (kiln-exact-output "version")))
              (is (equal "Working" (kiln-exact-output "kiln-test-script")))
              (let ((contents (read-file-into-string script-file)))
                ;; Test that we don't need the file anymore.
                (delete-file script-file)
                ;; No debug flag.
                (is (equal "Working" (kiln-exact-output "kiln-test-script")))
                ;; Test that we catch if the file has changed.
                (write-string-into-file (string-replace "Working" contents "Still working")
                                        script-file)
                ;; No debug flag.
                (is (equal "Still working" (kiln-exact-output "kiln-test-script")))
                ;; Test that we can still reload with new dependencies.
                (is (search ":use :cl" contents))
                (write-string-into-file
                 (string-replace ":use :cl"
                                 contents
                                 ":use :cl :other-test-system")
                 script-file
                 :if-exists :supersede)
                (is (equal "Working" (kiln-exact-output "kiln-test-script")))))))))))

(5am:test shebang
  (uiop:with-temporary-file (:pathname p :type "lisp" :stream s)
    (format s "#!/usr/bin/env ~a~%(defvar *randnum* #.(random most-positive-fixnum))~%(defun kiln-user:main (args) (format t \"Hello, ~~a\" *randnum*))"
            *self*)
    (close s)
    ;; Work around file-write-date only having second precision.
    (sleep 2)
    (let ((output (kiln-exact-output p)))
      (is (string^= "Hello" output))
      (is (equal output (kiln-exact-output p)))
      (cmd "touch" p)
      (is (not (equal output (kiln-exact-output p)))))))

(5am:test exec-no-unwind
  (let* ((script (asdf:system-relative-pathname :kiln "test/test-exec.lisp"))
         (result (kiln-exact-output script)))
    (is (equal (fmt "Before exec~%exec happened")
               result))))

(5am:test exec-unwind
  (let* ((script (asdf:system-relative-pathname :kiln "test/test-exec.lisp"))
         (result (kiln-exact-output script "unwind")))
    (is (equal (fmt "Before exec~%Unwinding happened~%exec happened")
               result))))

(defun main (args)
  (destructuring-bind (&optional (test 'test)) args
    (when (stringp test)
      (setf test (intern (string-upcase test))))
    (let* ((argv0 (uiop:argv0))
           (self
             (if (uiop:absolute-pathname-p argv0) argv0
                 (or (resolve-executable argv0)
                     (truename argv0))))
           (*self* (list self))
           (5am:*test-dribble* *error-output*))
      (dbg "Self: ~a" *self*)
      (cmd *self* "version")
      (let ((results (5am:run test)))
        (5am:explain! results)
        (if (every #'5am::test-passed-p results)
            0
            1)))))
