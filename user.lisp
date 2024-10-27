(uiop:define-package :kiln/user
  (:nicknames :kiln-user)
  (:use-reexport
   :cl
   :alexandria
   :cl-ppcre
   :cmd
   :iterate
   :named-readtables
   :serapeum
   :trivia
   :trivia.fail
   :trivia.ppcre)
  (:export :main)
  (:shadowing-import-from :iterate :in :collecting :sum :summing)
  (:shadowing-import-from :trivia :@)
  (:shadowing-import-from :cl-ppcre :scan)
  (:export :scan :@ :kiln-readtable))
(in-package :kiln/user)

(defreadtable kiln-readtable
  (:fuse :standard))

(declaim (notinline main))

(defun main (args)
  (declare (ignore args))
  (error "No main function defined for in this script"))
