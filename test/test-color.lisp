#!/usr/bin/env kiln
;; -*- mode: lisp -*-

(defun main (argv)
  (print (kiln/tty::want-color-p *standard-output*)))
