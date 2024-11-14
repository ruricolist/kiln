(defpackage :kiln/script-cache
  (:use :cl :alexandria :serapeum :kiln/hot-reload :kiln/stamp)
  (:local-nicknames (:flags :kiln/flags))
  (:import-from :kiln/user)
  (:export
   #:run-cached-script
   #:populate-script-cache
   #:*script-cache*))
(in-package :kiln/script-cache)

(defstruct-read-only shebang-script
  (path :type pathname)
  (main :type function)
  (stamp :type stamp))

(defvar *script-cache* (dict))

(defparameter *ignored-directory-subseqs*
  '(("Windows" "system32")
    ("Program Files") ("Program Files (x86)")))

(defparameter *ignored-extensions*
  '("so" "dylib"
    ;; WSL
    "bin" "exe" "dll" "NLS" "png" "sdi" "sys" "mof" "xml"))

(-> cache-script (pathname &key (:force t)) shebang-script)
(defun cache-script (script &key force)
  (declare (pathname script))
  (labels ((wrap-main (main)
             (lambda (args)
               ;; Ensure main can call itself.
               (setf (symbol-function 'kiln/user:main)
                     main)
               (funcall main args)))
           (load-and-cache (path)
             (let ((old-main (symbol-function 'kiln/user:main)))
               (unwind-protect
                    (progn
                      (fast-load-script path)
                      (setf (href *script-cache* path)
                            (make-shebang-script
                             :path path
                             :main (wrap-main (symbol-function 'kiln/user:main))
                             :stamp (file-stamp path))))
                 (setf (symbol-function 'kiln/user:main)
                       old-main)))))
    (let* ((path (truename script))
           (entry (href *script-cache* path)))
      (cond ((no entry)
             (flags:dbg "Not cached, loading: ~a" path)
             (load-and-cache path))
            (force
             (flags:dbg "Forced reload of ~a" path)
             (load-and-cache path))
            ((stamp= (shebang-script-stamp entry)
                     (file-stamp path))
             (flags:dbg "Cached, not reloading: ~a" path)
             entry)
            (t
             (flags:dbg "Out of date: ~a" path)
             (load-and-cache path))))))

(-> run-cached-script (pathname list) t)
(defun run-cached-script (script args)
  (funcall (shebang-script-main (cache-script script))
           args))

(defun kiln-scripts-in-path ()
  (mappend #'kiln-scripts-in-dir
           (path-dirs)))

(defun kiln-scripts-in-dir (dir)
  (flags:dbg "Scanning ~a" dir)
  (filter #'kiln-script?
          (uiop:directory-files dir)))

(defun path-dirs ()
  (remove-if (lambda (dir)
               (some (lambda (subseq)
                       (search subseq (pathname-directory dir)
                               :test #'equalp))
                     *ignored-directory-subseqs*))
             (mapcar (op (uiop:ensure-directory-pathname (pathname _)))
                     (split-sequence #\: (uiop:getenv "PATH")))))

(defun kiln-script? (file)
  (let ((file (pathname file)))
    (and (not (ignored-extension? file))
         (file-executable-p file)
         (starts-with-shebang? file)
         (file-kiln-shebang? file))))

(defun ignored-extension? (file)
  (member (pathname-type file)
          *ignored-extensions*
          :test #'string-equal))

(defun populate-script-cache ()
  (flags:dbg "Populating script cache~%")
  (multiple-value-prog1
      (mapc #'cache-script (kiln-scripts-in-path))
    (flags:dbg "Populated script cache")
    (setf (symbol-function 'kiln/user:main)
          (lambda (args)
            (declare (ignore args))
            (error "No main in script")))))
