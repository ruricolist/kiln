(defpackage :kiln/argparse
  (:use :cl :alexandria :iterate :serapeum)
  (:shadowing-import-from :iterate :collecting :in :sum :summing)
  (:documentation "Argument parsing"))
(in-package :kiln/argparse)

(defmacro destructuring-bind/argv (bindings argv &body body)
  `(destructuring-bind ,bindings
       (parse-argv
        (load-time-value
         (parse-lambda-list ,bindings))
        ,argv)
     ,@body))

(defconstructor parsed-lambda-list
  (required-params list)
  (optional-params list)
  (rest-param symbol)
  (keyword-params list)
  (allow-other-keys boolean)
  (aux-params list)
  (keywordp boolean))

(deftype lambda-list-designator ()
  '(or list parsed-lambda-list))

(-> parse-lambda-list (lambda-list-designator) parsed-lambda-list)
(defun parse-lambda-list (lambda-list)
  (etypecase-of lambda-list-designator lambda-list
    (list
     (multiple-value-call #'parsed-lambda-list
       (alexandria:parse-ordinary-lambda-list lambda-list)))
    (parsed-lambda-list lambda-list)))

(defconstructor parsed-argv
  (arguments list)
  (options list))

_(defun parse-argv (lambda-list argv)
   "Parse ARGV into a argument list."
   (mvlet* ((lambda-list (parse-lambda-list lambda-list))
            (argv (parse-argv argv))
            (parsed-arguments '()))
     (let ((required (parsed-lambda-list-required-params lambda-list))
           (optional (parsed-lambda-list-opttions-params lambda-list)))
       ;; Get the positional arguments.
       (loop for (arg . more-args) on (parsed-argv-arguments argv)
             if (or (pop required)
                    (pop optional)
                    (parse-lambda-list-rest-param lambda-list))
                (pushnew arg parsed-arguments)
                (error "Too many arguments (remaining: ~{~a~^ ~}"
                       args))

       )
     (dolist (param (required-params lambda-list))
       (push (pop ))
       )

     ()
     )
   )
