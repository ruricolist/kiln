(defpackage :kiln/stamp
  (:use :cl :serapeum :trivial-file-size)
  (:export :stamp :stamp-mtime :stamp-size :file-stamp
           :stamp=
           :file-executable-p))
(in-package :kiln/stamp)

(defstruct-read-only stamp
  mtime size)

(defun file-stamp (file)
  (make-stamp
   :mtime (file-write-date file)
   :size (file-size-in-octets file)))

(defun stamp= (stamp1 stamp2)
  (and (eql (stamp-mtime stamp1)
            (stamp-mtime stamp2))
       (eql (stamp-size stamp1)
            (stamp-size stamp2))))

(defun file-executable-p (file)
  (and (uiop:file-exists-p file)
       (handler-case
           (logand sb-posix:s-ixusr
                   (sb-posix:stat-mode (sb-posix:stat file)))
         (sb-posix:syscall-error (e)
           (values nil e)))))
