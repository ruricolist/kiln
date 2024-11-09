(uiop:define-package :kiln/utils
  (:documentation "Utilities for writing scripts")
  (:use :cl :alexandria :serapeum :named-readtables)
  (:use-reexport :kiln/os :kiln/tty)
  (:import-from :cffi)
  (:import-from :cl-ppcre)
  (:import-from :kiln/dispatch :invoke-script)
  (:import-from :kiln/flags :dbg)
  (:local-nicknames
   (:interpol :cl-interpol))
  ;; TODO Remove when Quicklisp updates
  (:shadow :parse-cmd-dsl)
  (:export
   :do-lines
   :walk-lines
   :fields
   :cd
   :interpolate-escapes
   :invoke-script
   :invoke-argv))
(in-package :kiln/utils)
(in-readtable :interpol-syntax)

(defun walk-lines (fn &optional (source *standard-input*))
  (etypecase source
    (null
     (walk-lines fn *standard-input*))
    (stream
     (fbind fn
       (loop for line = (read-line source nil nil)
             while line
             do (fn line))))
    (string
     (with-input-from-string (stream source)
       (walk-lines fn stream)))))

(defmacro do-lines ((line &optional source return) &body body)
  `(do-lines-1 (,line ,source ,return)
     ,@body))

(define-do-macro do-lines-1 ((line source &optional return) &body body)
  (with-thunk (body line)
    `(walk-lines ,body ,source)))

(defun fields (string &optional (split-on #'whitespacep))
  (etypecase split-on
    (function
     (split-sequence-if split-on string))
    ((eql #\Space)
     (fields string #'whitespacep))
    (character
     (split-sequence split-on string))
    (string
     (if (single split-on)
         (split-sequence (character split-on) string)
         (cl-ppcre:split split-on string)))))


(declaim (ftype (function (string) string)
                red green yellow bold))

(defun clear-line (stream)
  (when (tty?)
    (write-string #?"\x1b[2K" stream)
    (force-output stream)))

(defun interpolate-escapes (string)
  (let ((interpol:*inner-delimiters* nil)
        (*readtable*
          (find-readtable :interpol-syntax))
        (*read-eval* nil))
    (assure string
            (with-input-from-string (in (string+ "#?\"" string #\"))
              (read in)))))

(defun invoke-argv (argv)
  "Invoke another script."
  (apply #'invoke-script (first argv) (rest argv)))
