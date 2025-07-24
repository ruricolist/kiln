(defpackage :kiln/tty
  (:documentation "Utilities for terminal output")
  (:nicknames :kiln-tty)
  (:use
   :cl
   :alexandria
   :named-readtables
   :serapeum
   :trivia)
  (:local-nicknames
   (:interpol :cl-interpol))
  (:shadow :@)
  (:export
   :*color*
   :beep
   :clear-line
   :color
   :colour
   :colorize
   :colourise
   :rgb
   :stream-tty-p
   :ttyp))
(in-package :kiln/tty)
(in-readtable :interpol-syntax)

(deftype color-policy ()
  '(member :always :never :auto))

(defvar *color* :auto)
(declaim (type color-policy *color*))

(defun no-color-p ()
  (uiop:getenvp "NO_COLOR"))

(-> stream-tty-p (stream (member :input :output))
    (values (or null stream) boolean))
(defun stream-tty-p (stream direction)
  "Is STREAM a ttyp
DIRECTION should be :input or :output.

Returns two booleans: the underlying TTY stream (possibly but not
necessarily the same as STREAM), and a second value for confidence (T
if sure, NIL if unsure)."
  (declare (stream stream)
           ((member :input :output) direction))
  (typecase stream
    (synonym-stream
     (stream-tty-p
      (symbol-value (synonym-stream-symbol stream))
      direction))
    (two-way-stream
     (stream-tty-p
      (ecase direction
        (:input
          (two-way-stream-input-stream stream))
        (:output
          (two-way-stream-output-stream stream)))
      direction))
    (broadcast-stream
     (let ((streams (broadcast-stream-streams stream)))
       (match streams
         (() (values nil t))
         ((list stream)
          (stream-tty-p stream direction))
         ((list* _ _)
          (dolist (stream streams)
            (multiple-value-bind (stream sure?)
                (stream-tty-p stream direction)
              (return (values stream sure?))))
          (values nil nil)))))
    (otherwise
     #+ccl
     (if-let (fd (ccl::stream-device stream direction))
       (values (and (= 1 (ccl::isatty fd)) stream) t)
       (values nil nil))
     #+sbcl
     (if (typep stream 'sb-sys:fd-stream)
         (values
          (and
           (= 1 (sb-unix:unix-isatty (sb-sys:fd-stream-fd stream)))
           stream)
          t)
         (values nil nil))
     #-(or ccl sbcl)
     (values nil nil))))

(defun want-color-p (stream)
  (and (not (no-color-p))
       (stream-tty-p stream :output)))

(defun ttyp ()
  "Return T if there is a controlling TTY."
  (handler-case
      (progn
        (open #p"/dev/tty" :direction :probe)
        t)
    (file-error () nil)))

(defun clear-line (stream &key return)
  (when-let (stream (stream-tty-p stream :output))
    (write-string #?"\x1b[2K" stream)
    (when return
      (write-char #\Return stream))
    (force-output stream)))

(defun beep (&key (stream *standard-output*))
  (when-let (stream (stream-tty-p stream :output))
    (write-char #\Bel stream)
    (finish-output stream)))

(defconstructor rgb
  (red octet)
  (green octet)
  (blue octet))

(deftype color ()
  '(or symbol octet rgb))

(defconst +colors+
  (dictq
   nil 0
   :off 0
   :bold 1
   :bold-off 21
   :faint 2
   :faint-off 22
   :dim 2
   :dim-off 22
   :italic 3
   :italic-off 23
   :underline 4
   :underline-off 24
   :slow-blink 5
   :slow-blink-off 25
   :blink 5
   :blink-off 25
   :fast-blink 6
   :fast-blink-off 25
   :reverse 7
   :reverse-off 27
   :reverse-video 7
   :reverse-video-off 27
   :conceal 8
   :conceal-off 28
   :hide 8
   :hide-off 28
   :black 30
   :black-bg 40
   :red 31
   :red-bg 41
   :green 32
   :green-bg 42
   :yellow 33
   :yellow-bg 43
   :blue 34
   :blue-bg 44
   :magenta 35
   :magenta-bg 45
   :cyan 36
   :cyan-bg 46
   :white 37
   :white-bg 47
   :default 39
   :default-bg 49
   :bright-black 90
   :bright-black-bg 100
   :bright-red 91
   :bright-red-bg 101
   :bright-green 92
   :bright-green-bg 102
   :bright-yellow 93
   :bright-yellow-bg 103
   :bright-blue 94
   :bright-blue-bg 104
   :bright-magenta 95
   :bright-magenta-bg 105
   :bright-cyan 96
   :bright-cyan-bg 106
   :bright-white 97
   :bright-white-bg 107
   :bright-default 99
   :bright-default-bg 109))

(defun color (stream color &optional colon? at-sign?)
  "Write an ANSI escape for a color/effect to STREAM.

COLOR can be null (to reset formatting), a keyword (for a named ANSI
color/effect), an integer (for a VGA color), or an instance of
`kiln/tty:rgb' for an RGB color.

This function is designed so it can be invoked with the ~/ format
control:

    (format t \"~/kiln-tty:color/This is red~/kiln-tty:color/This is not\"
      :red
      nil)

By default, escapes are only written if STREAM can be determined to be
a TTY. The `NO_COLOR' environment variable is also respected (see
`no-color.org').

To force colorized output, pass non-nil `colon?', or bind
`kiln/tty:*color*' to `:always'."
  (declare (ignore at-sign?)
           (color color))
  (let ((code
          (etypecase-of color color
            (octet
             (fmt "38;5;~a" color))
            (symbol
             (or (gethash color +colors+)
                 (error "Unknown color: ~a" color)))
            (rgb
             (multiple-value-call #'fmt "38;2;~a;~a;~a"
               (ematch color
                 ((rgb red green blue)
                  (values red green blue))))))))
    (when (ecase-of color-policy *color*
            (:never nil)
            (:always t)
            (:auto
             (or colon?
                 (want-color-p stream))))
      (format stream #?"\x1b[~am" code))))

(defsubst colour (stream color &optional color? at-sign)
  "Alias for `color."
  (color stream color color? at-sign))

(defstruct-read-only
    (colorize
     (:constructor colorize (color object))
     (:constructor colourise (color object)))
  "Wrap an object with a color."
  (color :type color)
  (object :type t))

(defmethod print-object ((wrapper colorize) stream)
  (format stream "~/kiln-tty:color/" (colorize-color wrapper))
  (print-object (colorize-object wrapper) stream)
  (format stream "~/kiln-tty:color/" nil))
