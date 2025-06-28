(defpackage :kiln/scripts/beep
  (:use :cl)
  (:local-nicknames
   (:tty :kiln/tty)))
(in-package :kiln/scripts/beep)

(defun main (args)
  (tty:beep))
