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
  (:export
   :fields
   :cd
   :interpolate-escapes
   :invoke-script
   :invoke-argv))
(in-package :kiln/utils)
(in-readtable :interpol-syntax)

(defun interpolate-escapes (string)
  (let ((interpol:*inner-delimiters* nil))
    (assure string
      (with-input-from-string (in (string+ #\" string #\"))
        (interpol:interpol-reader in nil nil :recursive-p nil)))))

(defun invoke-argv (argv)
  "Invoke another script."
  (apply #'invoke-script (first argv) (rest argv)))
