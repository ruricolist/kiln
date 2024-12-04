(uiop:define-package :kiln/utils
  (:documentation "Utilities for writing scripts")
  (:use :cl :alexandria :serapeum :named-readtables)
  (:use-reexport :kiln/os :kiln/tty)
  (:import-from :cffi)
  (:import-from :clawk)
  (:import-from :kiln/dispatch :invoke-script)
  (:import-from :kiln/flags :dbg)
  (:local-nicknames
   (:interpol :cl-interpol))
  ;; TODO Remove when Quicklisp updates
  (:shadow :parse-cmd-dsl)
  (:export
   :fields
   :cd
   :interpolate-escapes
   :invoke-script
   :invoke-argv))
(in-package :kiln/utils)
(in-readtable :interpol-syntax)

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
