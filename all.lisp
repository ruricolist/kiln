(uiop:define-package :kiln/all
  (:nicknames :kiln)
  (:use-reexport :kiln/utils :kiln/flags)
  (:import-from :kiln/dispatch :invoke-script :print-error :exit)
  (:export :invoke-script :exit)
  (:use :cl))
(in-package :kiln/all)
