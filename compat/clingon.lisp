(defpackage :kiln/compat/clingon
  (:documentation "Kiln/Clingon compatibility.")
  (:use :cl)
  (:import-from
    :kiln/dispatch
    :error-exit-code
    :print-error-and-backtrace)
  (:local-nicknames
   (:cli :clingon)))
(in-package :kiln/compat/clingon)

(defmethod error-exit-code ((e clingon:exit-error))
  (cli:exit-error-code e))

(defmethod print-error-and-backtrace ((e clingon:exit-error))
  (unless (= 0 (cli:exit-error-code e))
    (call-next-method)))
