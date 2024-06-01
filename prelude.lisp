(uiop:define-package :kiln/prelude
  (:use-reexport
   :alexandria
   :bordeaux-threads
   :cl
   :cl-ppcre
   :cl-strftime
   :cmd
   :drakma
   :named-readtables
   :kiln/flags
   :kiln/path
   :kiln/utils
   :serapeum
   :trivia
   :trivia.ppcre
   :trivial-file-size)
  (:shadowing-import-from :serapeum :@)
  (:shadowing-import-from :cl-ppcre :scan))
(in-package :kiln/prelude)
