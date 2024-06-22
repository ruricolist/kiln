(defpackage :kiln/scripts/self-test
  (:use :cl :alexandria :serapeum :cmd :fiveam :trivial-file-size)
  (:import-from :kiln/flags :dbg :dbg?)
  (:import-from :FiveAM :is)
  (:local-nicknames (:5am :fiveam))
  (:documentation "Perform self-test of Kiln itself"))
(in-package :kiln/scripts/self-test)

(defvar *self*)

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
  (is (equal "0 1 2 3 4 5"
             (substitute #\Space #\Newline
                         ($cmd *self*
                               "loop for '@i' from 0 to 5 do '!echo $i'")))))

(5am:test rebuild
  (terpri *error-output*)
  (uiop:with-temporary-file (:pathname p)
    (delete-file p)
    (finishes
      (cmd *self* "rebuild --target-file" p))
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

(5am:test reload
  (uiop:with-temporary-file (:pathname tmp)
    (delete-file tmp)
    (let* ((tmpdir (ensure-directories-exist tmp))
           ;; TODO Must be a better way.
           (tmpdir (uiop:parse-unix-namestring
                    (string+ (uiop:unix-namestring tmpdir) "/")))
           (test-system-dir
             (ensure-directories-exist (path-join tmpdir #p"kiln-test-system/")))
           (other-test-system-dir
             (ensure-directories-exist (path-join tmpdir #p"other-test-system/")))
           (templates-dir
             (asdf:system-relative-pathname
              :kiln
              "test/kiln-test-system")))
      (copy-file (path-join templates-dir #p"kiln-test-system-asd")
                 (path-join test-system-dir #p"kiln-test-system.asd"))
      (copy-file (path-join templates-dir #p"other-test-system-asd")
                 (path-join other-test-system-dir #p"other-test-system.asd"))
      (let ((script-file (path-join test-system-dir #p"kiln-test-script.lisp")))
        (copy-file (path-join templates-dir #p"kiln-test-script-lisp")
                   script-file)
        (copy-file (path-join templates-dir #p"other-test-system-lisp")
                   (path-join test-system-dir #p"other-test-script.lisp"))
        (let ((*cmd-env*
                `(("CL_SOURCE_REGISTRY"
                   . ,(fmt "~a//:~a:"
                           (uiop:unix-namestring tmpdir)
                           (uiop:unix-namestring
                            (asdf:system-relative-pathname :kiln ""))))
                  ("KILN_PATH_SYSTEMS" . "kiln-test-system")))
              (debug-flag
                (and (dbg?) (list "--debug"))))
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
    (let ((output ($cmd *self* p)))
      (is (string^= "Hello" output))
      (is (equal output ($cmd *self* p)))
      (cmd "touch" p)
      (is (not (equal output ($cmd *self* p)))))))

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
