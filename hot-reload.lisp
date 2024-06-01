(defpackage :kiln/hot-reload
  (:use :cl :alexandria :serapeum)
  (:local-nicknames
   (:flags :kiln/flags)
   (:sys :kiln/system))
  (:import-from :cmd)
  (:shadowing-import-from :cl-ppcre :scan)
  (:export
   #:hot-reload
   #:fast-load-script
   #:+fasl-type+
   #:starts-with-shebang?
   #:file-kiln-shebang?
   #:script-fasl-output-location))
(in-package :kiln/hot-reload)

(def +compiler-policy+
  ;; TODO Does low safety help?
  '((compilation-speed :min 3)
    (debug :max 0 :min 0))
  "Compiler policy for fast compilation.")

(defun set-compiler-policy ()
  (proclaim
   `(optimize
     ,@(loop for (quality . kwargs) in +compiler-policy+
             collect (list quality (getf kwargs :min 0)))))
  #+sbcl
  (loop for spec in +compiler-policy+
        do (destructuring-bind (quality &key (min 0) (max 3)) spec
             (sb-ext:restrict-compiler-policy quality min max))))

(def +fasl-type+
  (pathname-type (compile-file-pathname "foo.lisp")))

(defvar *original-output-translations*
  (progn (asdf:ensure-output-translations)
         asdf::*output-translations*))

(defun call/original-output-translations (fn)
  ;; Use copy-tree so nothing can mutate the stored translations.
  (let ((asdf::*output-translations* (copy-tree *original-output-translations*)))
    (funcall fn)))

(defmacro with-original-output-translations ((&key) &body body)
  (with-thunk (body)
    `(call/original-output-translations ,body)))

(defun original-output-translation (p)
  (with-original-output-translations ()
    (asdf:apply-output-translations p)))

(defun clear-kiln-fasls ()
  (cmd:cmd "rm -rf" #p"~/.cache/common-lisp/kiln"))

(defun redirect-output-translations (&key suffix)
  (declare (list suffix))
  ;; Add cache directory so we don't pollute
  ;; the regular fasl cache.
  (asdf:clear-output-translations)
  (asdf:initialize-output-translations
   ;; TODO It would be best if we would only
   ;; write new fasls here, but look up old
   ;; ones in the normal user cache. Use
   ;; overlayfs when available?
   `(:output-translations
     (t (:home ".cache" "common-lisp" "kiln" ,@suffix :implementation))
     :inherit-configuration)))

(defvar *original-compile-file*
  #'asdf:compile-file*)

(defun original-fasl-exists-p (input-file output-file)
  (let* ((input-file/fasl-ext
           (make-pathname :defaults input-file :type (pathname-type output-file)))
         (original-fasl (original-output-translation input-file/fasl-ext)))
    (and (uiop:file-exists-p original-fasl)
         original-fasl)))

(defun symlink-or-copy (from to)
  (if (uiop:os-unix-p)
      (cmd:cmd "ln -fs" (list from) (list to))
      (copy-file from to :if-to-exists :rename-and-delete)))

(defun copy-or-compile-file (input-file &rest kwargs &key output-file &allow-other-keys)
  (cond-let dest
    ((and (uiop:file-exists-p output-file)
          ;; If the original fasl has been removed.
          (uiop:file-exists-p (uiop:resolve-symlinks output-file)))
     (flags:dbg "Keeping old ~a" dest)
     output-file)
    ((original-fasl-exists-p input-file output-file)
     (flags:dbg "Reusing original fasl ~a" dest)
     (symlink-or-copy input-file output-file)
     output-file)
    (t (apply *original-compile-file* input-file kwargs))))

(defun override-asdf-compile-function ()
  (flags:dbg "Overriding ASDF compile function")
  (setf (symbol-function 'asdf:compile-file*)
        #'copy-or-compile-file))

(defun restore-asdf-compile-function ()
  (flags:dbg "Restoring ASDF compile function")
  (setf (symbol-function 'asdf:compile-file*)
        *original-compile-file*))

(defun call/copy-or-compile (fn)
  (unwind-protect
       (progn
         ;; Preventing loading overriding the override.
         (asdf:load-system "asdf")
         (override-asdf-compile-function)
         (funcall fn))
    (restore-asdf-compile-function)))

(defmacro with-copy-or-compile ((&key) &body body)
  (with-thunk (body)
    `(call/copy-or-compile ,body)))

(defun fast-load-system (system)
  "Load SYSTEM as fast as possible.

Fasls for `fast-load-system' are kept separate from the default fasl
cache. This means we can set Lisp to optimize `compilation-speed' by
default.

This would have the unfortunate side effect of implying that systems
that were already compiled in the default fasl cache would need
recompiling. To avoid this, we hack into ASDF to

We hack into ASDF in such a way that, if its fasls are already
compiled, we simply symlink them from the default fasl cache into
ours.

Note that this function is used exclusively for hot-reloading changed
scripts."
  ;; Redirect output so we don't pollute the fasl cache.
  (set-compiler-policy)
  (with-original-output-translations ()
    (redirect-output-translations)
    (with-copy-or-compile ()
      (sys:load-system system))))

(defun fasl-output-location (source-file &key suffix)
  (assert (uiop:absolute-pathname-p source-file))
  (let ((cf-path (compile-file-pathname (truename source-file))))
    (with-original-output-translations ()
      (redirect-output-translations :suffix suffix)
      (asdf:apply-output-translations cf-path))))

(defun script-fasl-output-location (source-file)
  (fasl-output-location source-file :suffix '("scripts")))

(defun starts-with-shebang? (file)
  (let* ((file (truename file))
         (buffer (make-octet-vector 2)))
    (handler-case
        (with-input-from-file (in file :element-type 'octet)
          (read-sequence buffer in)
          (vector= buffer #.(coerce #(35 33) 'octet-vector)))
      (file-error (e)
        (values nil e)))))

(defun kiln-shebang-line? (line)
  (scan "^#! ?/+(?:\\./+)*usr/+(?:\\./+)*bin/+(?:\\./+)*env +kiln"
        line))

(defun first-line (file)
  (with-input-from-file (in file)
    (read-line in)))

(defun file-kiln-shebang? (file)
  (and (starts-with-shebang? file)
       (kiln-shebang-line? (first-line file))))

(defun fast-load-script (path)
  (set-compiler-policy)
  (let* ((path (truename path))
         (fasl (script-fasl-output-location path)))
    (unless (and (uiop:file-exists-p fasl)
                 (> (file-write-date fasl)
                    (file-write-date path)))
      (if (not (uiop:file-exists-p fasl))
          (flags:dbg "Fasl ~a does not exist" fasl)
          (flags:dbg "Fasl out of date"))
      (uiop:delete-file-if-exists fasl)
      (flags:dbg "Compiling ~a" path)
      (flags:with-debug-output ()
        (ensure-directories-exist fasl)
        (if (not (starts-with-shebang? path))
            (compile-file path :output-file fasl)
            (uiop:with-temporary-file (:pathname p :stream s)
              (write-string (string-join (drop 1 (lines
                                                  (read-file-into-string path)
                                                  :keep-eols t))
                                         "")
                            s)
              (close s)
              (compile-file p :output-file fasl)))))
    (assert (uiop:file-exists-p fasl))
    (flags:dbg "Loading ~a" fasl)
    (flags:with-debug-output ()
      (load fasl))
    fasl))

(defun hot-reload (system package path)
  "Hot-reload package inferred system SYSTEM.
First, try to compile and load just SYSTEM's file. This should work
except when SYSTEM adds a new dependency not already present in the
image.

If reloading just the file fails, then use ASDF to load the system,
but direct fasls into a separate cache so we can optimize for
compilation speed. Also, override ASDF internals so files that already
exist are just symlinked from the old cache."
  (declare (ignorable package))
  (set-compiler-policy)
  (nlet retry ((count 1))
    (handler-bind (#+sbcl
                   (sb-int:package-at-variance-error
                     (lambda (e)
                       (flags:dbg "Delete package due to package variance")
                       (delete-package (package-error-package e))
                       (retry (1- count))))
                   (package-error
                     (lambda (e)
                       (flags:dbg "Package error: ~a" e)
                       (when (and (> count 0)
                                  (not (find-package (package-error-package e))))
                         (flags:dbg "Loading system due to package error")
                         (fast-load-system system)
                         (retry (1- count))))))
      (fast-load-script path))))
