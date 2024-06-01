(defpackage :kiln/scripts/version
  (:use :cl :kiln/path :cl-strftime :cmd)
  (:import-from :local-time)
  (:documentation "Version of Kiln"))
(in-package :kiln/scripts/version)

(defvar *build-date* (local-time:now))
(defvar *version* (asdf:system-version (asdf:find-system "kiln")))
(defvar *commit*
  ($cmd "git log -1" (list "--pretty=format:%h")
        :in (asdf:system-relative-pathname (asdf:find-system "kiln") "")))

(defun main (args)
  (when args
    (error "Args to version: ~a" args))
  (format t "Version: ~a~%" *version*)
  (format t "Commit: ~a~%" *commit*)
  (format t "Build date: ")
  (format-time t "%F %0k:%M:%S %Z" *build-date*)
  (terpri)
  ;; Quicklisp, yea or nay?
  (format t "Quicklisp: ~:[no~:;yes~]~%"
          (find-package :quicklisp))
  (format t "Path: ~{~a~^:~}~%"
          (scripts-path))
  ;; Report builtins.
  (format t "Builtins: ~{~a~^ ~}~%"
          (remove-duplicates
           (mapcar #'script-name
                   (list-all-scripts))
           :test #'equal
           :from-end t)))
