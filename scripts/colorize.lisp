(defpackage :kiln/scripts/colorize
  (:use :cl :alexandria :serapeum)
  (:local-nicknames
   (:tty :kiln/tty))
  (:documentation "Colorize output with ANSI, VGA, or RGB colors."))
(in-package :kiln/scripts/colorize)

(defun main (args)
  (destructuring-bind (color . args) args
    (let ((color
            (if (string^= "#" color)
                (if (length= color 7)
                    (let ((red (parse-integer color :start 1 :end 3 :radix 16))
                          (green (parse-integer color :start 3 :end 5 :radix 16))
                          (blue (parse-integer color :start 5 :radix 16)))
                      (tty:rgb red green blue))
                    (error "Invalid hex code: ~a" color))
                (or (find-keyword (string-upcase color))
                    (error "Unknown color: ~a" color)))))
      (dolist (arg args)
        (format t "~/tty:color/~a~/tty:color/~%" color arg nil)))))
