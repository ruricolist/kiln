(defpackage :kiln/tty
  (:documentation "Utilities for terminal output")
  (:use
   :cl
   :alexandria
   :named-readtables
   :serapeum)
  (:local-nicknames
   (:interpol :cl-interpol))
  (:export
   :beep
   :clear-line
   :bold
   :green
   :red
   :tty?
   :yellow))
(in-package :kiln/tty)
(in-readtable :interpol-syntax)

(defun clear-line (stream)
  (when (tty?)
    (write-string #?"\x1b[2K" stream)
    (force-output stream)))

(defun tty? ()
  "Return T if there is a controlling TTY."
  (handler-case
      (progn
        (open #p"/dev/tty" :direction :probe)
        t)
    (file-error () nil)))

(declaim (ftype (-> (string) string)
                red green yellow bold))

(defun red (s)
  (if (no (tty?)) s
      (string+ #?"\x1b[0;31m" s #?"\x1b[0m")))

(defun green (s)
  (if (no (tty?)) s
      (string+ #?"\x1b[0;32m" s #?"\x1b[0m")))

(defun orange (s)
  (if (no (tty?)) s
      (string+ #?"\x1b[0;35m" s #?"\x1b[0m")))

(defun yellow (s)
  (if (no (tty?)) s
      (string+ #?"\x1b[1;33m" s #?"\x1b[0m")))

(defun bold (s)
  (if (no (tty?)) s
      (string+ #?"\x1b[1m" s #?"\x1b[0m")))

(defun beep (&key (stream *standard-output*))
  (write-char #\Bel stream)
  (finish-output stream))
