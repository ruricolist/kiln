(defpackage :kiln/image
  (:use
   :cl :alexandria :serapeum
   :kiln/path :kiln/system
   :with-user-abort)
  (:local-nicknames (:bt :bordeaux-threads))
  (:shadowing-import-from :closer-mop :ensure-finalized)
  (:import-from :cffi)
  (:import-from :kiln/dispatch :*entry-point*)
  (:import-from
    :kiln/flags
    :dbg
    :+kiln-nursery-max-bytes+
    :+kiln-target-package+
    :+kiln-target-system+)
  (:import-from :kiln/script-cache :populate-script-cache)
  (:import-from :kiln/script-cache :populate-script-cache)
  (:import-from :kiln/utils :setpgrp)
  (:export
   :load-all-script-systems))
(in-package :kiln/image)

(let (lib-names)
  ;; Arrange to unload all shared libraries before the image is dumped
  ;; and load them again when the image is restored.

  ;; See https://github.com/cffi/cffi/pull/163
  (defun unload-all-foreign-libraries ()
    (let ((libs (cffi:list-foreign-libraries)))
      (setf lib-names (mapcar #'cffi:foreign-library-name libs))
      (dbg "Unloading libraries: ~a" lib-names)
      (mapc #'cffi:close-foreign-library libs)))
  (defun reload-all-foreign-libraries ()
    ;; Load in reverse order, since they are "pushed" as they are loaded.
    (dbg "Reloading libraries: ~a" (reverse lib-names))
    (mapc #'cffi:load-foreign-library (reverse lib-names))))

(defun mark-other-systems-immutable (&key (script-systems (list-all-script-subsystems)))
  (map nil 'asdf:register-immutable-system
       (set-difference (asdf:already-loaded-systems)
                       script-systems
                       :test #'equal)))

(defun mark-all-systems-immutable ()
  (map nil 'asdf:register-immutable-system
       (asdf:already-loaded-systems)))

(defun record-builtins ()
  (setf *builtins-by-system*
        (list-all-scripts-by-system)
        *builtins-by-system-table*
        (builtins-by-system-table *builtins-by-system*)))

(defun load-all-script-systems (&key (script-systems (list-all-script-subsystems)))
  (load-system script-systems
               :tolerant (uiop:getenvp "KILN_TOLERANT")))

(defun list-builtin-script-subsystems ()
  (mapcar #'script-subsystem
          (mappend #'cdr
                   *builtins-by-system*)))

(defun finalize-all-classes ()
  (do-all-symbols (sym)
    (when-let (cls (find-class sym nil))
      (unless (typep cls 'built-in-class)
        (ensure-finalized cls)))))

(defparameter *target-system*
  (uiop:getenvp +kiln-target-system+))

(defparameter *target-package*
  (or (uiop:getenvp +kiln-target-package+)
      (and *target-system*
           (string-invert-case *target-system*))))

(defparameter *kiln-nursery-max-bytes*
  (if-let (env-value (uiop:getenvp +kiln-nursery-max-bytes+))
    (assure (integer 0)
      (parse-integer env-value))
    (expt 2 30)))

(defun kiln-before-dump-image ()
  (setf uiop/image::*lisp-interaction* nil)
  #+sbcl (setf sb-ext:*derive-function-types* t)
  (handler-bind ((user-abort
                   (lambda (e)
                     (print e uiop:*stderr*)
                     (uiop:print-backtrace e :stream uiop:*stderr*))))
    (with-user-abort
      (record-builtins)
      ;; NB Quicklisp doesn't work if it's called inside of the ASDF
      ;; build-op. So we run it in a separate thread. (Is this still true?)
      (if *target-system*
          (let ((package-name
                  (or *target-package*
                      (error "No target package name in environment"))))
            (load-system *target-system*)
            (mark-all-systems-immutable)
            (let* ((package
                     (or (find-package package-name)
                         (error "No such package as ~a" package-name)))
                   (main
                     (or (find-symbol (string 'main) package)
                         (error "No main function for package ~a"
                                package-name))))
              (setf *entry-point*
                    (lambda ()
                      (funcall main (uiop:command-line-arguments))))))
          (let* ((subsystems (list-builtin-script-subsystems)))
            (load-all-script-systems :script-systems subsystems)
            ;; Mark systems immutable twice: first anything loaded by the
            ;; package scripts (so the shebang scripts load faster), then
            ;; again for anything loaded after the shebang scripts.
            (mark-other-systems-immutable :script-systems subsystems)
            (populate-script-cache)
            (mark-other-systems-immutable :script-systems subsystems)))
      (finalize-all-classes)
      (asdf:clear-configuration)
      (unload-all-foreign-libraries))))

;;; We need to save the location of the SBCL home directory to be able
;;; to load SBCL modules (sb-sprof, sb-introspect, etc.).

#+sbcl
(defvar *sbcl-home* (sb-int:sbcl-homedir-pathname))

(defun kiln-after-restore-image ()
  #+sbcl
  (progn
    (sb-ext:disable-debugger)
    (unless *target-system*
      ;; TODO Would it be better to preload them all?
      (setf sb-sys::*sbcl-homedir-pathname* *sbcl-home*)
      (setf sb-ext:*derive-function-types* nil))
    (unless (zerop *kiln-nursery-max-bytes*)
      (setf (sb-ext:bytes-consed-between-gcs)
            (min (sb-ext:bytes-consed-between-gcs)
                 *kiln-nursery-max-bytes*))))

  (setf uiop/image::*lisp-interaction* nil)
  (setpgrp)
  (reload-all-foreign-libraries))

(uiop:register-image-dump-hook 'kiln-before-dump-image)
(uiop:register-image-restore-hook 'kiln-after-restore-image)
