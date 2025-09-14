(defpackage :kiln/test/unit/unit
  (:use :cl :alexandria :fiveam)
  (:local-nicknames
   (:args :kiln/args)
   (:cli :clingon)))
(in-package :kiln/test/unit/unit)

(def-suite kiln)
(in-suite kiln)

(defun run-tests ()
  (run! 'kiln))

(test destructuring-simple
  (is (equal '("x" "foo")
             (args:with-argument-destructuring (x &key y)
                 (:argv '("x" "-y" "foo") :description "Unknown" :name "Unknown")
               (list x y)))))

(test destructuring-empty-argv
  (is (null
       (args:with-argument-destructuring (&key (flag nil flag))
           (:argv '())
         flag))))

(test destructuring-flag
  (is (args:with-argument-destructuring (&key (flag nil flag))
          (:argv '("--flag"))
        flag)))

(test destructuring-invert-flag
  (is (not (args:with-argument-destructuring (&key (no-flag t no-flag))
               (:argv '())
             no-flag)))
  (is (args:with-argument-destructuring (&key (no-flag t no-flag))
          (:argv '("--no-flag"))
        (not no-flag))))

(test destructuring-mix-short-and-long
  (is (eql "1"
           (args:with-argument-destructuring
               (&key ((:arg-name arg)) ((:a arg)))
               (:argv '("-a" "1" "--arg-name" "2"))
             arg))))

(test destructuring-help
  (is (not
       (emptyp
        (with-output-to-string (*standard-output*)
          (ignore-some-conditions (cli:exit-error)
            (args:with-argument-destructuring
                (&key ((:arg-name arg)) ((:a arg)))
                (:argv '("--help")))))))))

(test destructuring-version
  (is (not
       (emptyp
        (with-output-to-string (*standard-output*)
          (ignore-some-conditions (cli:exit-error)
            (args:with-argument-destructuring
                (&key ((:arg-name arg)) ((:a arg)))
                (:argv '("--version") :version "1.0"))))))))

(test destructuring-trailer
  (let ((args '("-n" "--" "--not-n")))
    (args:with-argument-destructuring
        (&rest more &key (n nil n) (not-n nil not-n))
        (:argv args)
      (list n not-n more))))
