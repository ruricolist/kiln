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

(test placeholder
  (is (= 4 (+ 2 2))))


(test destructuring-1
  (is (equal '("x" "foo")
             (args:with-argument-destructuring (x &key y)
                 (:argv '("x" "-y" "foo") :description "Unknown" :name "Unknown")
               (list x y)))))

(test destructuring-2
  (is (null
       (args:with-argument-destructuring (&key (flag nil flag))
           (:argv '())
         flag))))

(test destructuring-3
  (is (args:with-argument-destructuring (&key (flag nil flag))
          (:argv '("--flag"))
        flag)))

(test destructuring-4
  (is (eql "1"
           (args:with-argument-destructuring
               (&key ((:arg-name arg)) ((:a arg)))
               (:argv '("-a" "1" "--arg-name" "2"))
             arg))))

(test destructuring-5
  (is (not
       (emptyp
        (with-output-to-string (*standard-output*)
          (ignore-some-conditions (cli:exit-error)
            (args:with-argument-destructuring
                (&key ((:arg-name arg)) ((:a arg)))
                (:argv '("--help")))))))))
