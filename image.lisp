(defpackage :kiln/image
  (:use
   :cl :alexandria :serapeum
   :kiln/path :kiln/system
   :with-user-abort)
  (:local-nicknames (:bt :bordeaux-threads))
  (:shadowing-import-from :closer-mop :ensure-finalized)
  (:import-from :cffi)
  (:import-from :kiln/dispatch)
  (:import-from :kiln/prelude)
  (:import-from :kiln/script-cache :populate-script-cache)
  (:import-from :kiln/script-cache :populate-script-cache)
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
      (mapc #'cffi:close-foreign-library libs)))
  (defun reload-all-foreign-libraries ()
    ;; Load in reverse order, since they are "pushed" as they are loaded.
    (mapc #'cffi:load-foreign-library (reverse lib-names))))

(defun mark-other-systems-immutable (&key (script-systems (list-all-script-subsystems)))
  (map nil 'asdf:register-immutable-system
       (set-difference (asdf:already-loaded-systems)
                       script-systems
                       :test #'equal)))

(defun record-builtins ()
  (setf *builtins-by-system*
        (list-all-scripts-by-system)
        *builtins-by-system-table*
        (builtins-by-system-table *builtins-by-system*)))

(defun load-all-script-systems (&key (script-systems (list-all-script-subsystems)))
  (load-system script-systems :tolerant t))

(defun list-builtin-script-subsystems ()
  (mapcar #'script-subsystem
          (mappend #'cdr
                   *builtins-by-system*)))

(defun finalize-all-classes ()
  (do-all-symbols (sym)
    (when-let (cls (find-class sym nil))
      (unless (typep cls 'built-in-class)
        (ensure-finalized cls)))))

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
      (let* ((subsystems (list-builtin-script-subsystems)))
        (load-all-script-systems :script-systems subsystems)
        ;; Mark systems immutable twice: first anything loaded by the
        ;; package scripts (so the shebang scripts load faster), then
        ;; again for anything loaded after the shebang scripts.
        (mark-other-systems-immutable :script-systems subsystems)
        (populate-script-cache)
        (mark-other-systems-immutable :script-systems subsystems))
      (finalize-all-classes)
      (asdf:clear-configuration)
      (unload-all-foreign-libraries))))

#.(if (uiop:os-unix-p)
      '(cffi:defcfun "setpgrp" :int)
      '(defun setpgrp ()))


(defun kiln-after-restore-image ()
  #+sbcl (sb-ext:disable-debugger)
  #+sbcl (setf sb-ext:*derive-function-types* nil)
  (setf uiop/image::*lisp-interaction* nil)
  (setpgrp)
  (reload-all-foreign-libraries))

(uiop:register-image-dump-hook 'kiln-before-dump-image)
(uiop:register-image-restore-hook 'kiln-after-restore-image)
